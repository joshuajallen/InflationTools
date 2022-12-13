
#' @title Get IOTA spreads (US)
#'
#' @description Retrieves IOTA breakeven spreads from Bloomberg
#' 
#' @details returns data frame of IOTA breakevens, across maturities 
#'
#' @param end_date end date for the series, e.g "2018-01-01"
#' @param start_date start date for the series, e.g "2018-01-01"
#'
#' @return data frame of IOTA breakeven spreads
#'
#' @examples
#' \dontrun{
#'   InflationTools::iota_spreads_bloomberg(start_date = "2010-01-01",  end_date = Sys.Date())
#' }
#'
#' @export

iota_spreads_bloomberg <- function(start_date = Sys.Date() - 365, 
                                   end_date = Sys.Date()){
  
  
  swap_secus <- c()
  i <- 0
  MATURITIES <- c("1", "2", "5", "7", "10", "20", "30")
  for(j in MATURITIES){
    i <- i  + 1
    swap_secus[i] <- paste0("USSWIT", j, " BGN Curncy")
  }
  
  breakeven_secus <- c()
  i <- 0
  MATURITIES <- c("01", "02", "05", "07", "10", "20", "30")
  for(j in MATURITIES){
    i <- i  + 1
    breakeven_secus[i] <- paste0("USGGBE", j, " Index")
  }
  
  swap <-
    FIRVr::bloomberg_query(
      securities = c(swap_secus),
      fields = "PX_LAST",
      from_date = anytime::anydate(start_date),
      to_date = anytime::anydate(end_date)
    ) %>%   
    dplyr::mutate(Maturity =  as.numeric(stringr::str_extract(Security, pattern = "[[:digit:]]{1,2}"))) %>% 
    dplyr::rename("Swap" = "Value") %>% 
    dplyr::select(Date, Maturity, Swap)
  
    
  breakeven <-
    FIRVr::bloomberg_query(
      securities = c(breakeven_secus),
      fields = "PX_LAST",
      from_date = anytime::anydate(start_date),
      to_date = anytime::anydate(end_date)
    ) %>%   
    dplyr::mutate(Maturity = as.numeric(stringr::str_extract(Security, pattern = "[[:digit:]]{1,2}"))) %>% 
    dplyr::rename("Breakeven" = "Value") %>% 
    dplyr::select(Date, Maturity, Breakeven)
  
  iota <- dplyr::left_join(swap, breakeven, by = c("Date", "Maturity")) %>% 
    dplyr::mutate(IOTA = Swap - Breakeven) %>% 
    dplyr::select(Date, Maturity, IOTA)
  
  return(iota)
  
  # iota_spreads_bloomberg <- function(start_date = Sys.Date() - 365, 
  #                                    end_date = Sys.Date()){
  #   
  #   
  #   secus <- c()
  #   i <- 0
  #   MATURITIES <- c(2, 5, 10, 30)
  #   for(j in MATURITIES){
  #     i <- i  + 1
  #     secus[i] <- paste0(".IOTA", j, "Y", " F Index")
  #   }
  #   
  #   df <-
  #     FIRVr::bloomberg_query(
  #       securities = secus,
  #       fields = "PX_LAST",
  #       from_date = anytime::anydate(start_date),
  #       to_date = anytime::anydate(end_date)
  #     )
  #   
  #   
  #   return(df)
  #   
  #   
  # } 
  
} 

#' @title Iota ASW level 
#'
#' @description Returns inflation-linked bond par-par ASW iota levels   
#' 
#' @details takes a list of inflation linked bonds and their nominal comparitor bonds 
#'    ISINs to calculate the iota spread using  
#'
#' @param inflation_ISINs character, vector of inflation linked bonds to find the breakeven comparitor for
#' @param nominal_ISINs character, vector of nominal bonds to pair the inflation linked bonds against
#' @param ref_date date, a reference date for the yield level, defaults to \code{Sys.Date()}
#' 
#' @return data frame of inflation ISINs paired with nominal ISINs and the breakeven level
#'
#' @examples
#'    \dontrun{
#'        iota_asw_level(
#'          inflation_ISINs = c("US912810PZ57", "US912810QP66"),
#'          nominal_ISINs = c("US91282CDP32", "US912810QN19"),
#'          ref_date = Sys.Date()
#'        )
#'    }
#' @export
#' 
iota_asw_level <- function(inflation_ISINs,
                            nominal_ISINs,
                            ref_date = NULL){
  
  # check the lists are the same lengths 
  if(length(inflation_ISINs) != length(nominal_ISINs)){
    futile.logger::flog.error("The two ISIN lists are different lengths!")
  }
  
  # Default reference date to the system date if not set 
  if(is.null(ref_date)) ref_date <- Sys.Date()
  
  # Get the par-par asw level for all of the ISINs
  # requires different bloomberg call on the same day versus historic due to the field
  if(Sys.Date() == ref_date){
    con <- Rblpapi::blpConnect()
    asw_level <- Rblpapi::bdp(
        stringr::str_c(unique(c(inflation_ISINs, nominal_ISINs)), " Corp"),
        "ASSET_SWAP_SPD_MID"
      ) %>%
      tibble::rownames_to_column(var = "Security") %>%
      dplyr::rename(Value = ASSET_SWAP_SPD_MID) %>%
      dplyr::mutate(Security = gsub(" Corp", "", Security))
    Rblpapi::blpDisconnect(con)
  }
  if(Sys.Date() != ref_date){
    asw_level <- FIRVr::bloomberg_query(
      stringr::str_c(unique(c(inflation_ISINs, nominal_ISINs)), " Corp"),
      "ASSET_SWAP_SPD_MID",
      from_date = ref_date,
      to_date = ref_date
    ) %>%
      dplyr::select(Security, Value) %>%
      dplyr::mutate(Security = gsub(" Corp", "", Security))
    
  }
  if(nrow(asw_level) == 0){
    futile.logger::flog.warn(paste0("No asw data for ", ref_date, ". Could be weekend or holiday."))
    return(tibble::tibble())
  }
  
  output <- tibble::tibble(InfSec = inflation_ISINs) %>%
    dplyr::left_join(asw_level, 
                     by = c("InfSec" = "Security")) %>%
    dplyr::rename(InfASW = Value) %>%
    dplyr::bind_cols(tibble::tibble(NomSec = nominal_ISINs) %>%
                       dplyr::left_join(asw_level, 
                                        by = c("NomSec" = "Security")) %>%
                       dplyr::rename(NomASW = Value)) %>%
    dplyr::mutate(iota = InfASW - NomASW) %>%
    dplyr::select(InfSec, NomSec, iota)
  
  output
  
}


#' @title Iota ASW levels from lm database 
#'
#' @description Returns inflation-linked bond breakeven level   
#' 
#' @details takes a list of inflation linked bonds and their nominal comparitor bonds 
#'
#' @param inflation_list character, list in list management database for inflation bonds 
#' @param nominal_list character, list in list management database for nominal bonds
#' @param ref_date date, a reference date for the yield level, defaults to \code{Sys.Date()}
#' @param tie_method character, currently has two options to break ties when more than one bond has the closest maturity. 
#'     \code{"issue_date"} chooses the nominal bond with the closest similar maturity. \code{"high_yield"} chooses the nominal
#'     bond with the highest yield, effectively the cheapest. From \code{breakeven_pairing}. 
#' @param lm_database_path Path class character to list management database
#' 
#' @return data frame of inflation ISINs paired with nominal ISINs and the breakeven level
#'
#' @examples
#'    \dontrun{
#'        iota_asw_level_db(
#'          inflation_list = "USD_Infl_US",
#'          nominal_list = "USD_Govt_US",
#'          ref_date = Sys.Date()
#'        )
#'    }
#' 
#' 
#' @export
#' 
iota_asw_level_db <- function(inflation_list,
                               nominal_list,
                               ref_date = NULL,
                               tie_method = "issue_date",
                               lm_database_path = rmlmdbpath){
  
  # Default reference date to the system date if not set 
  if(is.null(ref_date)) ref_date <- Sys.Date()
  
  # Connect to database 
  lm_conn <- DBI::dbConnect(RSQLite::SQLite(), lm_database_path)
  inflation_ISINs <- FIRVr::db_lm_get_membership(lm_conn, inflation_list)$ISIN 
  nominal_ISINs <-  FIRVr::db_lm_get_membership(lm_conn, nominal_list)$ISIN     
  DBI::dbDisconnect(lm_conn)
  
  bp <- InflationTools::breakeven_pairing(inflation_ISINs,
                                          nominal_ISINs,
                                          tie_method = tie_method, 
                                          ref_date = ref_date)
  
  output <- InflationTools::iota_asw_level(inflation_ISINs = bp$InfSec,
                                            nominal_ISINs = bp$NomSec,
                                            ref_date = ref_date)
  
  output
  
}

#' @title ASW iota spread at a constant maturity point 
#'
#' @description Returns iota spread for a maturity point on the iota curve on a set day
#' 
#' @details Linear regression on iota spreads of bonds of a spread around the chosen maturity, in order
#'   to get a constant maturity style iota spread  
#'
#' @param maturity integer, constant maturity point in years
#' @param inflation_ISINs character, vector of inflation linked bonds 
#' @param nominal_ISINs character, vector of nominal bonds to pair the inflation linked bonds against
#' @param spread numeric, difference in years either side of the maturity point to chose bonds for
#'   regression, eg if maturity is 5 and spread 0.5 then the range will be 4.5y to 5.5y
#' @param ref_date date, a reference date for the yield level, defaults to \code{Sys.Date()}
#' @param tie_method character, currently has two options to break ties when more than one bond has the closest maturity. 
#'     \code{"issue_date"} chooses the nominal bond with the closest similar maturity. \code{"high_yield"} chooses the nominal
#'     bond with the highest yield, effectively the cheapest. From \code{breakeven_pairing}. 
#' 
#' @return numeric, ASW iota spread level
#'
#' @examples
#'    \dontrun{
#'        iota_asw_fixed_maturity(
#'          maturity = 5,
#'          inflation_ISINs = c("US912810PZ57", "US912810QP66"),
#'          nominal_ISINs = c("US91282CDP32", "US912810QN19"),
#'          ref_date = Sys.Date()
#'        )
#'    }
#' @export
#' 

iota_asw_fixed_maturity <- function(maturity,
                                inflation_ISINs,
                                nominal_ISINs,
                                spread = 0.5,
                                ref_date = NULL,
                                tie_method = "issue_date"){
  
  # Maturity must be an integer for lubridate years function to work
  if(maturity != round(maturity)) futile.logger::flog.error("maturity variable must be an integer!")
  
  # Default reference date to the system date if not set 
  if(is.null(ref_date)) ref_date <- Sys.Date()
  
  # filter the inflation isins to the window 
  inf_fltd <- FIRVr::bloomberg_query_static(stringr::str_c(unique(inflation_ISINs), " Corp"), 
                                            c("MATURITY", "ISSUE_DT"),
                                            tidy_data = FALSE
  ) %>% 
    # filter out bonds not issued on ref_date
    dplyr::filter(ISSUE_DT < ref_date) %>%
    dplyr::filter(MATURITY > ref_date + lubridate::years(maturity) - lubridate::days(round(365 * spread)), 
                  MATURITY < ref_date + lubridate::years(maturity) + lubridate::days(round(365 * spread))) %>%
    dplyr::mutate(Security = gsub(" Corp", "", Security))  
  
  pairs <- InflationTools::breakeven_pairing(inflation_ISINs = inf_fltd$Security,
                                             nominal_ISINs = nominal_ISINs,
                                             ref_date = ref_date,
                                             tie_method = tie_method)
  
  iota <- InflationTools::iota_asw_level(inflation_ISINs = pairs$InfSec,
                                         nominal_ISINs = pairs$NomSec,
                                         ref_date = ref_date) %>%
    stats::na.omit()
  
  if(nrow(iota) == 0){
    futile.logger::flog.info(paste0("No data for ", ref_date, " (", lubridate::wday(ref_date, label = TRUE), "), may be weekend or holiday"))
    return(NA)
  }
  
  # Linear regression model of the iota in the area    
  mod <- stats::lm(iota ~ MATURITY, 
                   data = dplyr::left_join(inf_fltd, 
                                           iota,
                                           by = c("Security" =  "InfSec")) %>%
                     dplyr::mutate(MATURITY = as.numeric(MATURITY)))
                     
  # Use predict to get the interpolated estimate 
  output <- predict(mod, list(MATURITY = as.numeric(ref_date + lubridate::years(maturity))))
  
  output
  
}

