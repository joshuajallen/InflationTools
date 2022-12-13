
#' @title Breakeven pairings 
#'
#' @description Returns inflation-linked bond paired with closest nominal comparitor  
#' 
#' @details returns a tibble of inflation linked ISINs paired with a nominal to imply breakevens.
#'     Works with 
#'
#'
#' @param inflation_ISINs character, vector of inflation linked bonds to find the breakeven comparitor for
#' @param nominal_ISINs character, vector of nominal bonds to pair the inflation linked bonds against
#' @param tie_method character, currently has two options to break ties when more than one bond has the closest maturity. 
#'     \code{"issue_date"} chooses the nominal bond with the closest similar maturity. \code{"high_yield"} chooses the nominal
#'     bond with the highest yield, effectively the cheapest. 
#' @param ref_date date, a reference date for the outstanding bonds, yield level etc, defaults to \code{Sys.Date()}
#' 
#' @return data frame of inflation ISINs paired with nominal ISINs
#'
#' @examples
#'    \dontrun{
#'        breakeven_pairing(
#'          inflation_ISINs = "US912810QP66",
#'          nominal_ISINs = c("US91282CDP32", "US912810QN19", "US912810QE10"),
#'          ref_date = Sys.Date()
#'        )
#'    }
#'    
#' @export
#' 
breakeven_pairing <- function(inflation_ISINs,
                              nominal_ISINs,
                              tie_method = "issue_date", 
                              ref_date = NULL){
  
  # Default reference date to the system date if not set 
  if(is.null(ref_date)) ref_date <- Sys.Date()
  
  # Get the maturity dates from Bloomberg for the bonds
  static <- FIRVr::bloomberg_query_static(
      stringr::str_c(unique(c(inflation_ISINs, nominal_ISINs)), " Corp"), 
      c("MATURITY", "ISSUE_DT"),
      tidy_data = FALSE
    ) %>% 
    # filter out bonds not issued on ref_date
    dplyr::filter(ISSUE_DT < ref_date) %>%
    dplyr::select(Security, ValueDate = MATURITY) 
  
  # get the top of the bucket for nominal bonds (max)
  nom_cap <- dplyr::inner_join(static,
                           tibble::tibble(ISIN = nominal_ISINs %>%
                             stringr::str_c(" Corp")),
                           by = c("Security" = "ISIN")) %>%
    dplyr::distinct(ValueDate) %>% 
    dplyr::arrange(ValueDate) %>%
    # this works out the max date of the bucket (ie halfway between two maturity dates)
    dplyr::mutate(max = as.Date((as.numeric(dplyr::lead(ValueDate, 1)) + as.numeric(ValueDate)) / 2,
                                origin = "1970-01-01"),
                  row_num = dplyr::row_number()) %>%
    dplyr::rename(NomMat = ValueDate)
  # because of the lag the last maturity date doesn't have a bucket, put a limit
  # on it as required for findInterval function
  nom_cap[nrow(nom_cap), "max"] <- as.Date("2400-01-01")
  
  # Get the distinct inflation linked maturities
  dist_infl_mat <- dplyr::inner_join(static,
                    tibble::tibble(ISIN = inflation_ISINs %>%
                                     stringr::str_c(" Corp")),
                    by = c("Security" = "ISIN")) %>%
    dplyr::distinct(ValueDate)
  
  # pair all possible nominal with the inflation linked maturities  
  paired <- dplyr::inner_join(static,
                    tibble::tibble(ISIN = inflation_ISINs %>%
                                     stringr::str_c(" Corp")),
                    by = c("Security" = "ISIN")) %>% 
    # findInterval effectively chooses the bucket 
    dplyr::left_join(dist_infl_mat %>%
                     dplyr::bind_cols(
                       tibble::tibble(intv = findInterval(unlist(dist_infl_mat), nom_cap$max) + 1)),
                     by  = "ValueDate") %>%
    dplyr::left_join(nom_cap,
                     by = c("intv" = "row_num")) %>%
    dplyr::select(InfSec = Security, InfMat = ValueDate, NomMat) %>%
    dplyr::left_join(dplyr::right_join(static,
                                       tibble::tibble(ISIN = nominal_ISINs %>%
                                                        stringr::str_c(" Corp")),
                                       by = c("Security" = "ISIN")),
                     by = c("NomMat" = "ValueDate")) %>%
    dplyr::rename(NomSec = Security) %>%
    dplyr::filter(NomMat > as.Date(ref_date))
  
  # issue date filter tries to find bonds of near matching maturity (ie 10y at issue)  
  # when there is two possible nominals to match with the linker
  if(tie_method == "issue_date"){ 
    
    issue_dt <- FIRVr::bloomberg_query_static(
      stringr::str_c(unique(c(paired$NomSec, paired$InfSec))), 
      c("ISSUE_DT")
    ) %>%
    dplyr::select(Security, ValueDate)
    
    paired <- paired %>%
      dplyr::left_join(issue_dt %>%
                         dplyr::rename(NomIss = ValueDate),
                       by = c("NomSec" = "Security")) %>%
      dplyr::left_join(issue_dt %>%
                         dplyr::rename(InfIss = ValueDate),
                       by = c("InfSec" = "Security")) %>% 
      dplyr::mutate(NomAge = as.numeric(ref_date - NomIss),
                    InfAge = as.numeric(ref_date - InfIss),
                    AgeDiff = abs(NomAge - InfAge)) %>%
      dplyr::group_by(InfSec) %>%
      dplyr::filter(AgeDiff == min(AgeDiff)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
    
  }
  # high yield filter tries to find cheapest bond of matching maturity when there is two 
  # possible nominals to match with the linker
  if(tie_method == "high_yield"){
    
    yield <- FIRVr::bloomberg_query(
      unique(paired$NomSec),
      "YLD_YTM_MID",
      from_date = ref_date,
      to_date = ref_date
      ) %>%
      dplyr::select(Security, Value)
    
    paired <- paired %>%
      dplyr::left_join(yield,
                by = c("NomSec" = "Security")) %>%
      dplyr::group_by(InfSec) %>%
      dplyr::filter(Value == max(Value)) %>%
      dplyr::ungroup()
  }  
  
  # format the outcome into tibble with two columns of ISINs
  paired %>%
    dplyr::select(InfSec, NomSec) %>%
    dplyr::mutate(NomSec = gsub(" Corp", "", NomSec),
                  InfSec = gsub(" Corp", "", InfSec))
  
}

#' @title Breakeven levels 
#'
#' @description Returns inflation-linked bond breakeven level   
#' 
#' @details takes a list of inflation linked bonds and their nominal comparitor bonds 
#'    ISINs to calculate the breakeven level  
#'
#' @param inflation_ISINs character, vector of inflation linked bonds
#' @param nominal_ISINs character, vector of nominal bonds, same length as \code{inflation_ISINs}
#' @param ref_date date, a reference date for the yield level, defaults to \code{Sys.Date()}
#' 
#' @return data frame of inflation ISINs paired with nominal ISINs and the breakeven level
#'
#' @examples
#'    \dontrun{
#'        breakeven_level(
#'          inflation_ISINs = c("US912810PZ57", "US912810QP66"),
#'          nominal_ISINs = c("US91282CDP32", "US912810QN19"),
#'          ref_date = Sys.Date()
#'        )
#'    }
#' @export
#' 
breakeven_level <- function(inflation_ISINs,
                             nominal_ISINs,
                             ref_date = NULL){
  
  # check the lists are the same lengths 
  if(length(inflation_ISINs) != length(nominal_ISINs)){
    futile.logger::flog.error("The two ISIN lists are different lengths!")
  }
  
  # Default reference date to the system date if not set 
  if(is.null(ref_date)) ref_date <- Sys.Date()
  
  # Get the yields for all of the ISINs

  yield <- FIRVr::bloomberg_query(
      stringr::str_c(unique(c(inflation_ISINs, nominal_ISINs)), " Corp"),
      "YLD_YTM_MID",
      from_date = ref_date,
      to_date = ref_date
    ) %>%
    dplyr::select(Security, Value) %>%
    dplyr::mutate(Security = gsub(" Corp", "", Security))
  
  output <- tibble::tibble(InfSec = inflation_ISINs) %>%
    dplyr::left_join(yield, 
                     by = c("InfSec" = "Security")) %>%
    dplyr::rename(InfYld = Value) %>%
    dplyr::bind_cols(tibble::tibble(NomSec = nominal_ISINs) %>%
                       dplyr::left_join(yield, 
                                        by = c("NomSec" = "Security")) %>%
                       dplyr::rename(NomYld = Value)) %>%
    dplyr::mutate(breakeven = NomYld - InfYld) %>%
    dplyr::select(InfSec, NomSec, breakeven)
  
  output
  
}


#' @title Breakeven levels 
#'
#' @description Returns inflation-linked bond breakeven level   
#' 
#' @details takes a list of inflation linked bonds and their nominal comparitor bonds 
#'    ISINs to calculate the breakeven level  
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
#'        breakeven_level_db(
#'          inflation_list = "USD_Infl_US",
#'          nominal_list = "USD_Govt_US",
#'          ref_date = Sys.Date()
#'        )
#'    }
#' 
#' 
#' @export
#' 
breakeven_level_db <- function(inflation_list,
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

  output <- InflationTools::breakeven_level(inflation_ISINs = bp$InfSec,
                                            nominal_ISINs = bp$NomSec,
                                            ref_date = ref_date)
  
  output
  
}




