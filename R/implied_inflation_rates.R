
#' @title Swap Implied Inflation
#'
#' @description Returns inflation implied by zero-coupon inflation swaps
#' 
#' @details In a zero coupon swap, one party pays fixed rate on a notional amount and the other pays floating linked to an inflation index.
#'    This function gives the expected inflation over a number of maturities in percentage terms. The index lag can be left in or removed. 
#'
#'
#' @param inflation_index Object of class character, specifying the inflation index 
#' 
#' @param lag_adjustment Logical, whether to adjust the projected curve for the lag in inflation swap market
#'
#' @return data frame swap implied inflation 
#'
#' @examples
#' \dontrun{
#'   swap_implied_inflation_data("CPURNSA Index")
#' }
#' 
#' @importFrom magrittr %>%
#' @export

swap_implied_inflation_data <- function(inflation_index, 
                                        lag_adjustment = TRUE){
  
  if (!all(
    inflation_index %in% c(
      "UKRPI Index",
      "CPURNSA Index",
      "JCPNJGBI Index",
      "FRCPXTOB Index",
      "AUCPI Index",
      "CPTFEMU Index",
      "ITCPIUNR Index",
      "CPALDE Index",
      "SPIPC Index"
    )
  )) {
    futile.logger::flog.error(paste0(inflation_index, " not supported, please check."))
  }
  
  
  infl_ind_tick <- readr::read_csv("index,ticker,lag
                                   UKRPI Index,BPSWIT,3 
                                   CPURNSA Index,USSWIT,3 
                                   JCPNJGBI Index,JYSWIT,3
                                   FRCPXTOB Index,FRSWI,3
                                   AUCPI Index,AUSWIT,3 
                                   CPTFEMU Index,EUSWI,3 
                                   ITCPIUNR Index,ILSWI,3
                                   CPALDE Index,GRSWIT,3 
                                   SPIPC Index,SPSWIT,3",
                                   col_types = "ccd")
  
  secus <- c()
  for(i in seq(1, length(inflation_index))) {
    if(inflation_index[i] %in% c("UKRPI Index", "CPTFEMU Index")){
      stub <- infl_ind_tick$ticker[which(infl_ind_tick$index == inflation_index[i])]
      for(j in c("A","B", "C", "F", "I", 1,2,3,4,5,6,7,8,9,10,12,15,20,25,30,40,50)){
        secus <- c(secus, paste0(stub, j, " Curncy"))
      }
    }else{
      stub <- infl_ind_tick$ticker[which(infl_ind_tick$index == inflation_index[i])]
      for(j in c("A","B", "C", "F", "I",1,2,3,4,5,6,7,8,9,10,12,15,20,25,30)){
        secus <- c(secus, paste0(stub, j, " Curncy"))
      }
    } 
  }  
  
  output <- FIRVr::bloomberg_query(secus,
                                   "PX_LAST",
                                   from_date = Sys.Date()) %>%
    dplyr::mutate(
      tickery = sapply(stringr::str_split(Security, " "), function(x) x[1]), temp = stringr::str_sub(tickery, 1, 4)
    ) %>%
    dplyr::left_join(infl_ind_tick %>% dplyr::mutate(temp = stringr::str_sub(ticker, 1, 4)), by = "temp") %>%
    dplyr::mutate(
      ticker_len = stringr::str_length(ticker),
      maturity = stringr::str_sub(tickery, ticker_len + 1, ticker_len + 2),
      maturity = dplyr::case_when(
        maturity == "A" ~ "0.08333",
        maturity == "B" ~ "0.16666",
        maturity == "C" ~ "0.25",
        maturity == "F" ~ "0.5",
        maturity == "I" ~ "0.75",
        TRUE ~ maturity
      ),
      maturity = as.numeric(maturity)
    ) %>%
    dplyr::select(index, maturity, Value) %>%
    tidyr::pivot_wider(id_cols = maturity,
                       names_from = index,
                       values_from = Value) %>%
    dplyr::arrange(maturity)
  
  if(lag_adjustment == TRUE){
    
    # Choose the right lag period
    index_lag <- infl_ind_tick %>%
      dplyr::filter(index == inflation_index) %>%
      dplyr::pull(lag)
    
    # Remove the lag from the index and filter all past values
    output <- output %>%
      dplyr::mutate(maturity = maturity - (index_lag / 12)) %>%
      dplyr::filter(maturity > 0)
    
  }
  
  return(output)
  
}

#' @title Implied Projected Inflation Curve 
#'
#' @description Returns inflation projected from implied inflation curve  
#' 
#' @details Implied curve can be taken from zero-coupon swap curve or elsewhere, from which the (marked) implied inflation can be backed out. 
#'      See also \link[InflationTools]{implied_proj_inflation_curve_db}.
#'
#'
#' @param implied_curve Object of class data frame, specifying the market implied curve, expects column names 'implied_infl', 'maturity' both numeric
#' 
#' @param base_level Base level of the inflation index, numeric 
#'
#' @param base_month Numeric object, base month for the inflation index
#'
#' @param base_year Numeric object, base year for the inflation index
#' 
#' @param frequency frequency of the index, e.g. 12 for monthly 
#' 
#' @return data frame of projections implied from the curve 
#'
#' @examples
#'   implied_proj_inflation_curve(readr::read_csv("maturity,implied_infl
#'                                                 1,2
#'                                                 2,3
#'                                                 3,2.5
#'                                                 5,2"), 
#'                                100, 2, 2020, 12)
#'
#'
#' @importFrom magrittr %>%
#' @export
#' 

implied_proj_inflation_curve <- function(implied_curve,
                                         base_level,
                                         base_month = lubridate::month(Sys.Date()),
                                         base_year = lubridate::year(Sys.Date()),
                                         frequency = 12
){
  
  if (!(frequency %in% c(1, 2, 3, 4, 12))) {
    futile.logger::flog.error(
      "frequency can only be monthly (12), quarterly (4), thirdly(?) (3), semi-annual (2), or annual (1)."
    )
  }
  
  if (!all(c("maturity", "implied_infl") %in% colnames(implied_curve))) {
    futile.logger::flog.error("inflation data must have colnames: 'implied_infl', 'maturity'")
  }
  
  if (!is.numeric(base_level)) {
    futile.logger::flog.error('base level must be numeric format')
  }
  
  by_period <- dplyr::case_when(frequency == 12 ~ "month",
                                frequency == 4 ~ "3 month",
                                frequency == 3 ~ "4 month",
                                frequency == 2 ~ "6 month",
                                frequency == 1 ~ "year", 
                                TRUE ~ "Error")
  
  implied_curve <- implied_curve %>% stats::na.omit()
  implied_spline <- stats::smooth.spline(implied_curve$maturity, implied_curve$implied_infl)
  
  proj_table <- tibble::enframe(seq.Date(as.Date(as.Date(paste(base_year, base_month, "01", sep = "-"))), 
                                         by = by_period, 
                                         length.out = (frequency * max(implied_curve$maturity)) + 2),
                                value = "date",
                                name = NULL) %>%
    dplyr::mutate(date = date - 1,
                  YfT = as.numeric((date - Sys.Date()) / 365), 
                  infl_rate = sapply(YfT, function(x) stats::predict(implied_spline, x)$y),
                  YfB = as.numeric((date - min(date))/365),
                  proj_infl = base_level * (1 + (infl_rate/100))^(YfB)) %>%
    dplyr::select(date, proj_infl) %>%
    dplyr::mutate(season_factor_cuml = 1)
  
  return(proj_table)
  
}


#' @title Implied Projected Inflation Curve from inflation database
#'
#' @description Returns inflation projected from implied inflation curve  
#' 
#' @details Implied curve can be taken from zero-coupon swap curve or elsewhere, from which the (marked) implied inflation can be backed out
#'      See also \link[InflationTools]{implied_proj_inflation_curve}.
#'
#' @param inflation_index Object of class character, specifying the inflation index to use 
#' 
#' @param from_date Object of class date, the start date for the series
#'
#' @param infl_database_path character, path of inflation database, default is rminfldbpath from package
#' 
#' @param sa_method method for seasonal adjustment, defaults to 'linear'
#' 
#' @param seasonally_adjust boolean argument, seasonal adjustment
#' 
#' @return data frame of projections implied from the curve 
#'
#' @examples
#' \dontrun{
#'   implied_proj_inflation_curve_db("CPURNSA Index")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate %m+%
#' @export
#' 


implied_proj_inflation_curve_db <- function(inflation_index,
                                            seasonally_adjust = FALSE,
                                            sa_method = "linear",
                                            from_date = as.Date("2019-01-01"),
                                            infl_database_path = rminfldbpath
                                            
){
  
  if (!is.character(infl_database_path) | !file.exists(infl_database_path)){
    futile.logger::flog.error('inflation database file path not found')
  }
  
  if (!is.character(sa_method)){
    futile.logger::flog.error('seasonal adjustment method not in the correct format')
  }
  
  if (!is.character(inflation_index)){
    futile.logger::flog.error('Projected inflation index not in the correct format, e.g. CPTFEMU Index')
  }
  
  query_string <- paste0("SELECT * FROM inflation_indices WHERE inflation_index = '", inflation_index, "'")
  
  # Get historic data from database 
  inflation_ticker <- inflation_index
  infl_conn <- DBI::dbConnect(RSQLite::SQLite(), infl_database_path)
  
  inflation_past <-
    DBI::dbGetQuery(infl_conn, query_string) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::select(date, index_value) %>%
    dplyr::filter(date >= from_date)
  
  if(nrow(inflation_past) < 1){futile.logger::flog.warn("No historic index data found, arguement from.date likely set too late.")}
  
  DBI::dbDisconnect(infl_conn)  
  
  # Get the market implied swap curve but in index space
  infl_curr <-  InflationTools::implied_proj_inflation_curve(
    implied_curve = InflationTools::swap_implied_inflation_data(inflation_ticker, 
                                                                lag_adjustment = TRUE) %>%
                                               dplyr::rename(implied_infl = !!inflation_ticker),
    base_level = inflation_past %>%
                    dplyr::filter(date == max(date)) %>%
                    dplyr::pull(index_value),
    base_month = inflation_past %>%
                    dplyr::filter(date == max(date)) %>%
                    dplyr::pull(date) %m+% months(1) %>%
                    lubridate::month(),
    base_year = inflation_past %>%
                    dplyr::filter(date == max(date)) %>%
                    dplyr::pull(date) %>%
                    lubridate::year()
    )
  
  # If required seasonally adjust the implied swap curve
  if(seasonally_adjust == TRUE){
    
    infl_curr <- InflationTools::seasonality_adjust_projection_db(infl_curr, 
                                                                  inflation_ticker, 
                                                                  proj_is_SA = TRUE,
                                                                  start_date = Sys.Date(),
                                                                  method = sa_method,
                                                                  outliers_shift = c("ao2009.Oct", "ao2009.Sep"),
                                                                  database_path = infl_database_path
    )
    
  }
  
  # Combine past and present inflation data
  projected_inflation_curve <- inflation_past %>%
    dplyr::filter(date != max(date)) %>%
    dplyr::rename(proj_infl = index_value) %>%
    dplyr::mutate(season_factor_cuml = 1) %>%
    dplyr::bind_rows(infl_curr)
  
  return(projected_inflation_curve)
  
}



#' @title Implied Forward Inflation Curve (using swap rates)
#'
#' @description Returns inflation projected forward curve from swap implied inflation curve  
#' 
#' @details Implied curve is taken from zero-coupon swap curve.
#'
#' @param inflation_index Object of class character, specifying the inflation index to use 
#' 
#' @param lag_adjustment Object of class Boolean, whether to include apply lag adjustment (default is TRUE)

#' 
#' @return data frame of forward rates across all available tenors 
#'
#' @examples
#' \dontrun{
#'   swap_implied_inflation_forwards("CPURNSA Index")
#' }
#'
#' @importFrom magrittr %>%
#' @export
#' 
#' 
#' 

swap_implied_inflation_forwards <- function(inflation_index, lag_adjustment = TRUE){
  
  if (!all(
    inflation_index %in% c(
      "UKRPI Index",
      "CPURNSA Index",
      "JCPNJGBI Index",
      "FRCPXTOB Index",
      "AUCPI Index",
      "CPTFEMU Index",
      "ITCPIUNR Index",
      "CPALDE Index",
      "SPIPC Index"
    )
  )) {
    futile.logger::flog.error(paste0(inflation_index, " not supported, please check."))
  }
  
  
  infl_ind_tick <- readr::read_csv("index,ticker,lag
                                   UKRPI Index,BPSWIT,3 
                                   CPURNSA Index,USSWIT,3 
                                   JCPNJGBI Index,JYSWIT,3
                                   FRCPXTOB Index,FRSWI,3
                                   AUCPI Index,AUSWIT,3 
                                   CPTFEMU Index,EUSWI,3 
                                   ITCPIUNR Index,ILSWI,3
                                   CPALDE Index,GRSWIT,3 
                                   SPIPC Index,SPSWIT,3",
                                   col_types = "ccd")
  
  secus <- c()
  for(i in seq(1, length(inflation_index))) {
    if(inflation_index[i] %in% c("UKRPI Index", "CPTFEMU Index")){
      stub <- infl_ind_tick$ticker[which(infl_ind_tick$index == inflation_index[i])]
      for(j in c("A","B", "C", "F", "I", 1,2,3,4,5,6,7,8,9,10,12,15,20,25,30,40,50)){
        secus <- c(secus, paste0(stub, j, " Curncy"))
      }
    }else{
      stub <- infl_ind_tick$ticker[which(infl_ind_tick$index == inflation_index[i])]
      for(j in c("A","B", "C", "F", "I",1,2,3,4,5,6,7,8,9,10,12,15,20,25,30)){
        secus <- c(secus, paste0(stub, j, " Curncy"))
      }
    } 
  }  
  
  output <- FIRVr::bloomberg_query(secus,
                                   "PX_LAST",
                                   from_date = Sys.Date()) %>%
    dplyr::mutate(
      tickery = sapply(stringr::str_split(Security, " "), function(x) x[1]), temp = stringr::str_sub(tickery, 1, 4)
    ) %>%
    dplyr::left_join(infl_ind_tick %>% dplyr::mutate(temp = stringr::str_sub(ticker, 1, 4)), by = "temp") %>%
    dplyr::mutate(
      ticker_len = stringr::str_length(ticker),
      maturity = stringr::str_sub(tickery, ticker_len + 1, ticker_len + 2),
      maturity = dplyr::case_when(
        maturity == "A" ~ "0.08333",
        maturity == "B" ~ "0.16666",
        maturity == "C" ~ "0.25",
        maturity == "F" ~ "0.5",
        maturity == "I" ~ "0.75",
        TRUE ~ maturity
      ),
      maturity = as.numeric(maturity), 
      forward = maturity
    ) %>%
    dplyr::select(index, tickery, maturity, forward, Value) %>% 
    dplyr::arrange(maturity)
  
  
  forward_df <-
    output %>% 
    tidyr::expand(maturity, forward) %>% 
    dplyr::left_join(dplyr::select(output,-forward), by = c("maturity")) %>% 
    dplyr::rename("forward_rate" = "Value", "forward_ticker" = "tickery")
  
  tenor_df <-
    output %>% 
    tidyr::expand(maturity, forward) %>% 
    dplyr::left_join(dplyr::select(output,-maturity), by = c("forward")) %>% 
    dplyr::rename("tenor_rate" = "Value", "tenor_ticker" = "tickery")
  
  output <- tenor_df %>% 
    dplyr::left_join(forward_df, by = c("maturity", "forward", "index")) %>% 
    dplyr::mutate(forward = forward - maturity) %>% 
    dplyr::filter(forward > 0) %>% 
    dplyr::mutate(i = forward + maturity, j = maturity , term = paste0(forward, "y", maturity, "y")) %>% 
    dplyr::mutate(fwd_rate = 100*(i*tenor_rate/100 -  j*forward_rate /100) / (i - j)) %>% 
    dplyr::select(index, tenor_ticker, forward_ticker, i, j, term, fwd_rate)
  
  
  
  return(output)
  
  
}


#' @title Implied Forward Inflation Series (using swap rates)
#'
#' @description Returns inflation projected forward series from swap implied inflation data  
#' 
#' @details Implied series is taken from zero-coupon swap curve.
#'
#' @param inflation_index Object of class character, specifying the inflation index to use 
#' 
#' @param lag_adjustment Object of class Boolean, whether to include apply lag adjustment (default is TRUE)
#' 
#' @param forward Object of character or numeric, the forward maturity, e.g. 1y
#' 
#' @param tenor Object of character or numeric, the tenor maturity, e.g. 1y
#' 
#' 
#' @return data frame of forward rates across all available tenors 
#'
#' @examples
#' \dontrun{
#'   swap_implied_inflation_forwards_series("CPURNSA Index", forward = "1y", tenor = "1y")
#' }
#'
#' @importFrom magrittr %>%
#' @export
#' 
#' 
#' 


swap_implied_inflation_forwards_series <- function(inflation_index, 
                                                   lag_adjustment = TRUE, 
                                                   forward = "1y", 
                                                   tenor = "1y"){
  
  if (!all(
    inflation_index %in% c(
      "UKRPI Index",
      "CPURNSA Index",
      "JCPNJGBI Index",
      "FRCPXTOB Index",
      "AUCPI Index",
      "CPTFEMU Index",
      "ITCPIUNR Index",
      "CPALDE Index",
      "SPIPC Index"
    )
  )) {
    futile.logger::flog.error(paste0(inflation_index, " not supported, please check."))
  }
  
  
  infl_ind_tick <- readr::read_csv("index,ticker,lag
                                   UKRPI Index,BPSWIT,3 
                                   CPURNSA Index,USSWIT,3 
                                   JCPNJGBI Index,JYSWIT,3
                                   FRCPXTOB Index,FRSWI,3
                                   AUCPI Index,AUSWIT,3 
                                   CPTFEMU Index,EUSWI,3 
                                   ITCPIUNR Index,ILSWI,3
                                   CPALDE Index,GRSWIT,3 
                                   SPIPC Index,SPSWIT,3",
                                   col_types = "ccd")
  
  
  forward = as.numeric(stringr::str_extract(forward, "[[:digit:]]{1,2}"))
  tenor = as.numeric(stringr::str_extract(tenor, "[[:digit:]]{1,2}"))
  
  forward = as.numeric(stringr::str_extract(forward, "[[:digit:]]{1,2}"))
  tenor = as.numeric(stringr::str_extract(tenor, "[[:digit:]]{1,2}"))
  
  n = tenor + forward
  m = tenor 
  
  secus <- c()
  for(i in seq(1, length(inflation_index))) {
    if(inflation_index[i] %in% c("UKRPI Index", "CPTFEMU Index")){
      stub <- infl_ind_tick$ticker[which(infl_ind_tick$index == inflation_index[i])]
      for(j in c(n, m)){
        secus <- c(secus, paste0(stub, j, " Curncy"))
      }
    }else{
      stub <- infl_ind_tick$ticker[which(infl_ind_tick$index == inflation_index[i])]
      for(j in c(n, m)){
        secus <- c(secus, paste0(stub, j, " Curncy"))
      }
    } 
  }  
  
  output <- FIRVr::bloomberg_query(secus,
                                   "PX_LAST",
                                   from_date = Sys.Date() - 2*365, to_date = Sys.Date()) %>%
    dplyr::mutate(
      tickery = sapply(stringr::str_split(Security, " "), function(x) x[1]), temp = stringr::str_sub(tickery, 1, 4)
    ) %>%
    dplyr::left_join(infl_ind_tick %>% dplyr::mutate(temp = stringr::str_sub(ticker, 1, 4)), by = "temp") %>%
    dplyr::mutate(
      ticker_len = stringr::str_length(ticker),
      maturity = stringr::str_sub(tickery, ticker_len + 1, ticker_len + 2),
      maturity = dplyr::case_when(
        maturity == "A" ~ "0.08333",
        maturity == "B" ~ "0.16666",
        maturity == "C" ~ "0.25",
        maturity == "F" ~ "0.5",
        maturity == "I" ~ "0.75",
        TRUE ~ maturity
      ),
      maturity = as.numeric(maturity), 
      forward = maturity
    ) %>%
    dplyr::select(Date, index, tickery, maturity, forward, Value) %>% 
    dplyr::arrange(maturity)
  
  forward_df <-
    output %>% 
    tidyr::expand(maturity, forward) %>% 
    dplyr::left_join(dplyr::select(output,-forward), by = c("maturity")) %>% 
    dplyr::rename("forward_rate" = "Value", "forward_ticker" = "tickery")
  
  tenor_df <-
    output %>% 
    tidyr::expand(maturity, forward) %>% 
    dplyr::left_join(dplyr::select(output,-maturity), by = c("forward")) %>% 
    dplyr::rename("tenor_rate" = "Value", "tenor_ticker" = "tickery")
  
  output <- tenor_df %>% 
    dplyr::left_join(forward_df, by = c("maturity", "forward", "index")) %>% 
    dplyr::mutate(forward = forward - maturity) %>% 
    dplyr::filter(forward > 0) %>% 
    dplyr::mutate(i = forward + maturity, j = maturity , term = paste0(forward, "y", maturity, "y")) %>% 
    dplyr::mutate(fwd_rate = 100*(i*tenor_rate/100 -  j*forward_rate /100) / (i - j)) %>% 
    dplyr::select(index, tenor_ticker, forward_ticker, i, j, term, fwd_rate)
  
  return(output)
  
}

#' @title Available inflation swap curves
#'
#' @description Returns a data table of current inflation swap curves that can be called in functions such as
#'     \code{swap_implied_inflation_data}
#'
#' @return data frame of inflation swap curves that are currently available
#'
#' @examples
#' available_inflation_curves()
#' 
#' @export
#' 
available_inflation_curves <- function(){
  
  curves <- readr::read_csv("index_name,ticker_stub,full_name,spot_ticker,lag,currency,currency_default
                                   USCPI,USSWIT,US CPI Urban Consumers NSA,CPURNSA Index,3,USD,TRUE
                                   UKRPI,BPSWIT,UK RPI All Items NSA,UKRPI Index,3,GBP,TRUE
                                   JPCPIB,JYSWIT,Japan CPI for Govt CPI Bonds,JCPNJGBI Index,3,JPY,TRUE
                                   EURCPI,EUSWIT,EZ HCIP ex Tobacco NSA,CPTFEMU Index,3,EUR,TRUE
                                   FRCPI,FRSWI,France CPI ex Tobacco,FRCPXTOB Index,3,EUR,FALSE 
                                   ITCPI,ILSWI,Italy CPI ex Tobacco,ITCPIURN Index,3,EUR,FALSE
                                   AUCPI,AUSWIT,Australia CPI All Goods NSA,AUCPI Index,3,AUD,TRUE
                                   DECPI,GRSWIT,Germany HCIP All Items NSA,CPALDE Index,3,EUR,FALSE
                                   ESCPI,SPIPC,Spain CPI NSA,SPIPC Index,3,EUR,FALSE",
                            col_types = "ccccdcl")
  
  return(curves)
  
}

