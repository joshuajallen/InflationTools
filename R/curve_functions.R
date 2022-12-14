

#' @title Linker Curve from Database
#'
#' @description Returns inflation-linked bonds from inflation database for a given country 
#' 
#' @details returns universe of linkers on the real yield curve for a given country, and related levels including seasonally adjusted real yields 
#'
#'
#' @param country Object of class character, specifying country, e.g. United States = "US"
#' @param projected_inflation_curve data.table, curve of projected inflation with columns \code{date} and \code{proj_infl}
#'    , eg can be generated by \link[InflationTools]{implied_proj_inflation_curve} 
#' @param ref_date date, trade or calculation date, regular settle is assumed 
#' @param infl_database_path Path to inflation database
#' @param lm_database_path Path to list management database
#'
#' @return data frame of yield measures
#'
#' @examples
#'\dontrun{
#'    linker_curve_data_db(country = "US", 
#'                         ref_date = Sys.Date())
#' }
#'
#' @export


linker_curve_data_db <- function(country,
                                 projected_inflation_curve = NULL,
                                 ref_date = Sys.Date(),
                                 lm_database_path = rmlmdbpath,
                                 infl_database_path = rminfldbpath){
  
  if (!is.character(infl_database_path) | !file.exists(infl_database_path)){
    futile.logger::flog.error('inflation database path file path not found or is in the wrong format')
  }
  
  if (!is.character(lm_database_path) | !file.exists(lm_database_path)){
    futile.logger::flog.error('list membership database path file path not found or is in the wrong format')
  }
  
  if (!lubridate::is.Date(ref_date)){
    futile.logger::flog.error('Reference date not in correct format, must be in date format YYYY-MM-DD')
  }
  
  if (!is.character(country) | !(country %in% c("US", "GB", "DE", "FR", "ES", "IT"))){
    futile.logger::flog.error('country not found or is in the wrong format')
  }
  
  # tryCatch({
  #   RblpapiblpConnect()
  # }, error = function(e) {
  #   futile.logger::flog.error(paste0("Unable to connect to bloomberg. Execution halted with following error: ", e))
  # })
  
  lm_conn <- DBI::dbConnect(RSQLite::SQLite(), lm_database_path)
  
  linkers <- FIRVr::db_lm_get_membership(lm_conn, FIRVr::db_lm_get_lists(lm_conn) %>%
                                  dplyr::filter(stringr::str_detect(ListName, "Infl")) %>%
                                  dplyr::select(ListName) %>%
                                  dplyr::pull()
  ) %>%
    dplyr::mutate(bberg_code = stringr::str_c(ISIN, " Corp"),
                  issuer = stringr::str_sub(ISIN, 1, 2)) %>%
    dplyr::filter(issuer == country)
  
  DBI::dbDisconnect(lm_conn)
  
  linker_static <- InflationTools::linker_static_db(
    bond_ISIN = linkers$ISIN,
    lm_database_path = lm_database_path,
    infl_database_path = infl_database_path
  ) %>%
    dplyr::left_join(linkers, by = c("ISIN", "MaturityDate", "SecurityDes"))
  
  data <- FIRVr::bloomberg_query(linker_static$bberg_code,
                                 "YLD_YTM_MID",
                                 from_date = ref_date,
                                 to_date = ref_date) %>%
    dplyr::select(Security, Value) %>%
    dplyr::left_join(linker_static, by = c("Security" = "bberg_code"))
  
  ref_index <- data %>%
    dplyr::count(reference_index) %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::pull(reference_index)

  if(is.null(projected_inflation_curve)){
    futile.logger::flog.info("No projected inflation curve specified, using CPURNSA (US).")
    projected_inflation_curve <- InflationTools::implied_proj_inflation_curve_db("CPURNSA Index", seasonally_adjust = TRUE)
  }else{
    projected_inflation_curve <- InflationTools::implied_proj_inflation_curve_db(ref_index, seasonally_adjust = TRUE)
  }
    
  get_individual_linker_info_db <- function(index, data, projected_inflation_curve){
    
    linker_temp <- InflationTools::linker_info_db(data$ISIN[index],
                                                   projected_inflation_curve = projected_inflation_curve,
                                                   real_yield = data$Value[index]) 
    
    curve <- tidyr::tribble(
      ~ ISIN,
      ~ MaturityDate,
      ~ real_yield_nsa,
      ~ real_yield_sa,
      ~ nominal_equivalent_yield,
      #--|--|----
      data$ISIN[index],
      as.character(data$MaturityDate[index]),
      linker_temp$real_yield_nsa,
      linker_temp$real_yield_sa,
      linker_temp$nominal_equivalent_yield
      
    )
    
    return(curve)
    
  }
  
  curve <- lapply(X = 1:nrow(data), get_individual_linker_info_db, data = data, projected_inflation_curve = projected_inflation_curve)

  curve <- data.table::rbindlist(curve) %>%
    dplyr::mutate(
      MaturityDate = as.Date.factor(MaturityDate),
      real_yield_nsa = as.numeric(real_yield_nsa),
      real_yield_sa = as.numeric(real_yield_sa),
      nominal_equivalent_yield = as.numeric(nominal_equivalent_yield),
      time_to_mat = as.numeric((MaturityDate - ref_date) / 365)
    )
  
  return(curve)
  
}

# 
# curve %>% dplyr::select(-c(MaturityDate, ISIN)) %>% tidyr::pivot_longer(-time_to_mat, names_to = "series") %>%  #
#   ggplot2::ggplot(ggplot2::aes(x = time_to_mat, y = value, colour = series)) +
#   ggplot2::geom_point()

