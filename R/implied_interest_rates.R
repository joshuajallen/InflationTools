

#' @title Swap implied interest rate curve
#'
#' @description Returns interest rate swap curve from bloomberg
#' 
#' @details In a zero coupon swap, one party pays fixed rate on a notional amount and the other pays floating linked to an inflation index.
#'    This function gives the expected inflation over a number of maturities in percentage terms. The index lag can be left in or removed. 
#'
#' @param interest_rate_curve Object of class character, specifying the name of the swap curve index 
#' @param ref_date Date, the reference date of the curve
#'
#' @return data frame of swap curve with one column per curve
#'
#' @examples
#' \dontrun{
#'   swap_implied_interest_rate_curve("SOFR")
#' }
#' 
#' @importFrom magrittr %>%
#' @export

swap_implied_interest_rate_curve <- function(interest_rate_curve,
                                             ref_date = Sys.Date()){
  
  # Use the interest rate index function to pull in the curves that have been set up so far. 
  
  available_curves <- InflationTools::available_interest_rate_curves()
  
  if (!all(
    interest_rate_curve %in% available_curves$index_name)) {
    futile.logger::flog.error(paste0("One of ", interest_rate_curve, " is not supported, please check function 
                                     InflationTools::available_interest_rate_curves() for available index_name."))
    return(tibble::tibble())
  }
  
  secus <- c()
  for(i in seq(1, length(interest_rate_curve))) {
      stub <- available_curves$ticker_stub[which(available_curves$index_name == interest_rate_curve[[i]])]
      for(j in c("A","B", "C", "D", "E", "F", "G", "H", "I", "J", "K",1,2,3,4,5,6,7,8,9,10,12,15,20,25,30,40,50)){
        secus <- c(secus, paste0(stub, j, " Curncy"))
    }
  } 
  
  output <- FIRVr::bloomberg_query(secus,
                                   "PX_LAST",
                                   from_date = ref_date) %>%
    # Make a temporary column to join on the static data 
    dplyr::mutate(
      tickery = sapply(stringr::str_split(Security, " "), function(x) x[1]), temp = stringr::str_sub(tickery, 1, 4)
    ) %>%
    # join the static data
    dplyr::left_join(available_curves %>% dplyr::mutate(temp = stringr::str_sub(ticker_stub, 1, 4)), by = "temp") %>%
    # Get the maturity and change the bloomberg letters into years
    dplyr::mutate(
      ticker_len = stringr::str_length(ticker_stub),
      maturity = stringr::str_sub(tickery, ticker_len + 1, ticker_len + 2),
      maturity = dplyr::case_when(
        maturity == "A" ~ "0.08333",
        maturity == "B" ~ "0.16666",
        maturity == "C" ~ "0.25",
        maturity == "D" ~ "0.33333",
        maturity == "E" ~ "0.41666",
        maturity == "F" ~ "0.5",
        maturity == "G" ~ "0.58333",
        maturity == "H" ~ "0.66666",
        maturity == "I" ~ "0.75",
        maturity == "J" ~ "0.83333",
        maturity == "K" ~ "0.91666",
        TRUE ~ maturity
      ),
      maturity = as.numeric(maturity)
    ) %>%
    dplyr::select(index_name, maturity, Value) %>%
    tidyr::pivot_wider(id_cols = maturity,
                       names_from = index_name,
                       values_from = Value) %>%
    dplyr::arrange(maturity)
  
  return(output)
  
}



#' @title Available interest rate swap curves
#'
#' @description Returns a data table of current interest rate swap curves that can be called in functions such as
#'     \code{swap_implied_interest_rate_data}
#'
#' @return data frame of swap curves that are currently available
#'
#' @examples
#' available_interest_rate_curves()
#' 
#' @importFrom magrittr %>%
#' @export
#' 

available_interest_rate_curves <- function(){
  
  curves <- readr::read_csv("index_name,ticker_stub,full_name,spot_ticker
                                   CDOR03,CDSW,CDOR 3 month,CDOR03 Index
                                   CADOIS,CDSO,Canadian OIS,CAONREPO Index 
                                   SOFR,USOSFR,US SOFR,SOFRRATE Index
                                   FF,USSO,US Fed funds,FEDL01 Index
                                   USLBR03,USSW,US libor 3 month,US0003M Index 
                                   USLBR03A,USSA,US libor 3 month annual,US0003M Index
                                   ESTR,EESWE,ESTR,ESTRON Index
                                   EURIBOR,EUSA,Euribor 6 month,EUR006M Index
                                   EONIA,EUSWE,EONIA,EONIA Index
                                   JPYOIS,JYSO,Japan MUTKCALM OIS,MUTKCALM Index
                                   RBACOR,ADSO,RBA overnight cash rate,RBACOR Index,
                                   BBSW,ADSWAP,BBSW 6 month,BBSW6M Index",
                                   col_types = "cccc")
  
  return(curves)
  
}

#' @title Swap implied interest rate curve for multiple dates
#'
#' @description Returns interest rate swap curve from bloomberg
#' 
#' @details In a zero coupon swap, one party pays fixed rate on a notional amount and the other pays floating linked to an inflation index.
#'    This function gives the expected inflation over a number of maturities in percentage terms. The index lag can be left in or removed. 
#'
#' @param interest_rate_curve Object of class character, specifying the name of the swap curve index 
#' @param from_date Date, the earliest date to get the curve
#' @param to_date Date, the final date to get the curve 
#'
#' @return data frame of swap curve 
#'
#' @examples
#' \dontrun{
#'   swap_implied_interest_rate_curve_historic("SOFR")
#' }
#' 
#' @importFrom magrittr %>%
#' @export
swap_implied_interest_rate_curve_historic <- function(interest_rate_curve,
                                                      from_date = Sys.Date()-20,
                                                      to_date = Sys.Date()){
  
  # Use the interest rate index function to pull in the curves that have been set up so far. 
  
  available_curves <- InflationTools::available_interest_rate_curves()
  
  if (!all(
    interest_rate_curve %in% available_curves$index_name)) {
    futile.logger::flog.error(paste0("One of ", interest_rate_curve, " is not supported, please check function 
                                     InflationTools::available_interest_rate_curves() for available index_name."))
    return(tibble::tibble())
  }
  
  secus <- c()
  for(i in seq(1, length(interest_rate_curve))) {
    stub <- available_curves$ticker_stub[which(available_curves$index_name == interest_rate_curve[[i]])]
    for(j in c("A","B", "C", "D", "E", "F", "G", "H", "I", "J", "K",1,2,3,4,5,6,7,8,9,10,12,15,20,25,30,40,50)){
      secus <- c(secus, paste0(stub, j, " Curncy"))
    }
  } 
  
  output <- FIRVr::bloomberg_query(secus,
                                   "PX_LAST",
                                   from_date = from_date,
                                   to_date = to_date) %>%
    # Make a temporary column to join on the static data 
    dplyr::mutate(
      tickery = sapply(stringr::str_split(Security, " "), function(x) x[1]), temp = stringr::str_sub(tickery, 1, 4)
    ) %>%
    # join the static data
    dplyr::left_join(available_curves %>% dplyr::mutate(temp = stringr::str_sub(ticker_stub, 1, 4)), by = "temp") %>%
    # Get the maturity and change the bloomberg letters into years
    dplyr::mutate(
      ticker_len = stringr::str_length(ticker_stub),
      maturity = stringr::str_sub(tickery, ticker_len + 1, ticker_len + 2),
      maturity = dplyr::case_when(
        maturity == "A" ~ "0.08333",
        maturity == "B" ~ "0.16666",
        maturity == "C" ~ "0.25",
        maturity == "D" ~ "0.33333",
        maturity == "E" ~ "0.41666",
        maturity == "F" ~ "0.5",
        maturity == "G" ~ "0.58333",
        maturity == "H" ~ "0.66666",
        maturity == "I" ~ "0.75",
        maturity == "J" ~ "0.83333",
        maturity == "K" ~ "0.91666",
        TRUE ~ maturity
      ),
      maturity = as.numeric(maturity)
    ) %>%
    dplyr::select(Date, index_name, maturity, Value) %>%
    dplyr::rename(curve_date = Date) %>%
    tidyr::pivot_wider(id_cols = c(maturity, curve_date),
                       names_from = index_name,
                       values_from = Value) %>%
    dplyr::arrange(curve_date, maturity)
  
  return(output)
  
}
