

#' Openlink report linker risk calculation
#' 
#' @description Calculate the linker risk associated with holdings from information found in the openlink report 
#' 
#' @details Runs using the more limited information provided in the daily openlink P&L reports. Is supplemented 
#'     by inflation and list management databases for static data, so it doesn't need to connect to bloomberg. 
#'     Probably shouldn't be used outside of openlink with \link[InflationTools]{linker_info_func} doing the 
#'     same calculations with less input required. 
#'
#' @param instrument_ticker character, the openlink ticker, normally of the format \code{"issuer_coupon_date"}  
#' @param maturity_date date, the date of the maturity ie final cashflow
#' @param clean_price numeric, the real (or clean price) of an inflation linked bond
#' @param nominal numeric, the face value or nominal of the instrument traded 
#' @param currency character, the currency denomination of the instrument
#' @param lm_database_path Path class character to list management database
#' @param infl_database_path Path class character to inflation database
#'
#' @return list, includes DV01, IE01 and any error messages
#' @export
#' 
#' @examples
#'  \dontrun{
#'      linker_risk_dashboard(instrument_ticker = "TII_0.5_151025", 
#'                            maturity_date = as.Date("2025-10-15"), 
#'                            clean_price = 107,
#'                            nominal = 120000000, 
#'                            currency = "USD")
#'  }
linker_risk_dashboard <- function(instrument_ticker, 
                                  maturity_date,
                                  clean_price,
                                  nominal, 
                                  currency, 
                                  ref_date = Sys.Date(), 
                                  lm_database_path = rmlmdbpath,
                                  infl_database_path = rminfldbpath){
  
  error_msgs <- ""
  
  
  
  
  # Split out the coupon from the openlink ticker name
  ins_coupon <- stringr::str_split(instrument_ticker, pattern = "_")[[1]][[2]] %>%
    as.numeric()
  
  # Find any matching linkers in the database using the coupon, maturity date and issue country/currency
  linker_data <- InflationTools::linker_static_all_db(lm_database_path = lm_database_path,
                                                      infl_database_path = infl_database_path) %>%
    dplyr::mutate(ccy = dplyr::case_when(country %in% c("DE", "ES", "FR", "IT") ~ "EUR",
                                         country == "US" ~ "USD",
                                         country == "JP" ~ "JPY",
                                         country == "CN" ~ "CAD",
                                         country == "GB" ~ "GBP")) %>%
    dplyr::filter(MaturityDate == maturity_date,
                  coupon == ins_coupon,
                  ccy == currency)
  
  # Set quick run for matured instruments
  if(maturity_date <= ref_date + linker_data$settle_convention){
    
    output <- list(PV01 = 0,
                   IE01 = 0,
                   error_msgs = "Matured")
    
    return(output)
    
  }
  
  # If the linker is not in the database use a simple treatment to find both measures
  # Assuming it is a new issue, use the 10mn
  if(nrow(linker_data) != 1){
    
    error_msgs <- paste("There was", nrow(linker_data), "in the database for bond", instrument_ticker, "so using simple pricing.")
    return(list(PV01 = nominal * lubridate::time_length(difftime(maturity_date, Sys.Date()), "years") / 10000,
                IE01 = nominal * lubridate::time_length(difftime(maturity_date, Sys.Date()), "years") / 10000,
                error_msgs = error_msgs))
    
  }else{
    
    # Code primarily taken from linker_info functions but slimmed down a bit to make faster
    
    projected_inflation_curve <- InflationTools::historic_inflation_curve_db(linker_data$reference_index,
                                                                             ref_date = NULL,
                                                                             infl_database_path = infl_database_path) %>%
      dplyr::rename(proj_infl = proj_index)
    
    current_index_value <- InflationTools::interpolate_inflation_curve(projected_inflation_curve,
                                                                       ref_date = ref_date,
                                                                       inflation_lag = linker_data$inflation_lag,
                                                                       settle_convention = linker_data$settle_convention,
                                                                       round_5 = TRUE)
    
    cashflow_table <- InflationTools::cashflow_table(maturity_date = maturity_date,
                                                     coupon = ins_coupon,
                                                     payment_frequency = linker_data$payment_frequency,
                                                     ref_date = ref_date,
                                                     settle_convention = linker_data$settle_convention,
                                                     day_count = linker_data$day_count)
    
    linker_cashflow_table <- InflationTools::linker_cashflow_table(cashflow_table = cashflow_table,
                                                                   projected_inflation_curve = projected_inflation_curve,
                                                                   reference_cpi = linker_data$reference_cpi,
                                                                   ref_date = ref_date,
                                                                   settle_convention = linker_data$settle_convention,
                                                                   inflation_lag = linker_data$inflation_lag,
                                                                   principal_floor = linker_data$principal_floor,
                                                                   coupon_floor = linker_data$coupon_floor)
    
    real_accrued <- InflationTools::real_accrued(cashflow_table)
    
    real_yield <-  InflationTools::real_yield(cashflow_table = cashflow_table, 
                                              real_price = clean_price,
                                              payment_frequency = linker_data$payment_frequency,
                                              real_accrued = real_accrued)
    
    current_index_ratio <- round((current_index_value / linker_data$reference_cpi), 5)
    
    mac_dur <- InflationTools::duration_mac_inflation(linker_cashflow_table,
                                                      real_yield,
                                                      linker_data$payment_frequency)
    
    mod_dur <- mac_dur / (1 + (real_yield / (linker_data$payment_frequency * 100)))
    
    dv01_nom <- (mod_dur * clean_price)  
    
    dv01_inf <- dv01_nom * current_index_ratio
    
    inflation_to_mat <- ((linker_cashflow_table$proj_infl[nrow(linker_cashflow_table)] / current_index_value) ^ 
                           (linker_data$payment_frequency / linker_cashflow_table$discount_period_fraction[nrow(linker_cashflow_table)])
    ) - 1  
    
    ie01 <- mac_dur * clean_price * (1 / (1 + inflation_to_mat))
    
    # transform by the nominal traded 
    dv01_inf <- dv01_inf * nominal / 1000000
    ie01 <-  ie01 * nominal / 1000000
    
  }
  
  output <- list(PV01 = dv01_inf,
                 IE01 = ie01,
                 error_msgs = error_msgs)
  
  return(output)
  
}


#' Openlink report zero-coupon risk calculation
#' 
#' @description Calculate the risk associated with zero-coupon inflation linked swap (ILS) holdings from information 
#'       found in the openlink report 
#' 
#' @details Runs using the more limited information provided in the daily openlink P&L reports. Is supplemented 
#'     by inflation and list management databases for static data, so it doesn't need to connect to bloomberg. 
#'     Probably shouldn't be used outside of openlink with \link[InflationTools]{zero_coupon_inflation_swap} doing the 
#'     same calculations with less input required. 
#'
#' @param instrument_ticker character, the openlink ticker, normally a free text format for swaps. It is possible to include 
#'       a base or reference CPI in the ticker eg \code{"New zc swap [258.27]"} to improve calculation.      
#' @param maturity_date date, the date of the maturity ie final cashflow
#' @param nominal numeric, the face value or nominal of the instrument traded 
#' @param currency character, the currency denomination of the instrument
#' @param infl_database_path Path class character to inflation database
#'
#' @return list, includes DV01, IE01 and any error messages
#' @export
#' 
#' @examples
#' ZC_risk_dashboard(instrument_ticker = "example ZC swap [257.4]",
#'                    maturity_date = as.Date("2031-01-01"),
#'                    nominal = 10000000, 
#'                    currency = "EUR")
#' 
#' 
ZC_risk_dashboard <- function(instrument_ticker,
                               maturity_date,
                               nominal,
                               currency,
                               infl_database_path = rminfldbpath){
  
  error_msgs <- ""
  
  # Add code to deal with cancelled or matured swaps.
  if(maturity_date <= Sys.Date() + 3){
    output <- list(PV01 = 0,
                   IE01 = 0,
                   error_msgs = "Cancelled or novated.")        
    
    return(output)
  }
  
  
  infl_curve <- dplyr::case_when(currency == "EUR" ~ "CPTFEMU Index",                    # just use eu hcip even if FR index, should be good proxy
                                 currency == "USD" ~ "CPURNSA Index",
                                 currency == "JPY" ~ "JCPNJGBI Index",
                                 currency == "CAD" ~ "CACPI Index",
                                 currency == "GBP" ~ "UKRPI Index")
  
  projected_inflation_curve <- InflationTools::historic_inflation_curve_db(infl_curve,
                                                                           ref_date = NULL,
                                                                           infl_database_path = infl_database_path) %>%
    dplyr::rename(proj_infl = proj_index)
  
  # The reference CPI can not be know without either knowing the start date or the base_cpi
  # If we can't get this from the ticker, where it could be included in square brackets then just use 
  # the current value. This will be off by the current index ratio of the swap
  
  num_in_string <- stringr::str_extract(instrument_ticker, pattern = "\\[(.*?)\\]")
  cpi_in_ticker <- as.numeric(stringr::str_sub(num_in_string, 2, nchar(num_in_string) - 1))
  if(is.na(cpi_in_ticker)){
      error_msgs <- "Assuming an index ratio of 1 as no data on current index ratio"
      reference_cpi <- InflationTools::interpolate_inflation_curve(projected_inflation_curve,
                                                  ref_date = Sys.Date(),
                                                  inflation_lag = 3,
                                                  settle_convention = 2,
                                                  round_5 = TRUE)
    }else{
      reference_cpi <- cpi_in_ticker
    }
  
  zc_info <- InflationTools::zero_coupon_inflation_swap(ref_date = Sys.Date(),
                                           maturity_date = maturity_date,
                                           fixed_rate = 2,                # assumed 2% as long run inflation target 
                                           notional = nominal,
                                           projected_inflation_curve = projected_inflation_curve,
                                           inflation_lag = 3,
                                           reference_cpi = reference_cpi,
                                           settle_convention = 2,
                                           start_date = NULL,
                                           interpolation = "daily",
                                           day_count = "ACT/ACT")
  
  output <- list(PV01 = zc_info$dv01,
                 IE01 = -1 * zc_info$ie01,
                 error_msgs = error_msgs)        
  
  return(output)
  
}

#' Openlink ILB ticker to ISIN
#' 
#' @description For openlink tickers inflation linked bond tickers, return the corresponding ISINs of the each bond,
#'   if it exists.    
#' 
#' @param openlink_tickers come of the form (insturment)_(coupon)_(maturity date) eg TII_0.25_151028. This can be unpacked and compared to 
#'   a list of outstanding inflation linked bonds to get the ISIN in question. 
#' @param lm_database_path Path class character to list management database
#' @param infl_database_path Path class character to inflation database
#'
#' @return data.frame, of ol tickers and corresponding ISINs 
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#'   ol_ticker_to_ISIN(openlink_tickers = "TII_0.5_151025",
#'                      infl_database_path = rminfldbpath)
#'                    
#'   ol_ticker_to_ISIN(openlink_tickers = c("TII_0.125_150122", "TII_2_150126"))
#' }
#' 
ol_ticker_to_ISIN <- function(openlink_tickers,
                              lm_database_path = rmlmdbpath,
                              infl_database_path = rminfldbpath){
  
  # check that tickers are unique then seperate the segments
  openlink_tickers <- unique(openlink_tickers) %>%
    tibble::as_tibble() %>%
    dplyr::rename(openlink_tickers = value) 
  
  split <- stringr::str_split(openlink_tickers$openlink_tickers, pattern = "_")
  openlink_tickers$ticker <- purrr::map_chr(split, 1)
  openlink_tickers$coupon <- purrr::map_chr(split, 2) %>% as.numeric()
  openlink_tickers$maturity <- purrr::map_chr(split, 3) %>% as.Date(format = "%d%m%y")
  
  # Get static data for comparison 
  # While only TIPS are set up in OL difficult to tell what the DBRIs, FRTRs etc will have for tickers
  static <- InflationTools::linker_static_all_db(lm_database_path = lm_database_path,
                                                 infl_database_path = infl_database_path) %>%
    dplyr::select(ISIN, coupon, SecurityDes, MaturityDate) 
  
  static$ticker <- purrr::map_chr(stringr::str_split(static$SecurityDes, pattern = " "), 1)
    
  # Combine the tables and output 
  openlink_tickers %>%
    dplyr::left_join(static, 
                       by = c("ticker",
                              "coupon",
                              "maturity" = "MaturityDate")) %>%
    dplyr::select(openlink_tickers, ISIN)
  
}

