
#' @title Query inflation index series from inflation database
#'
#' @description Query and return inflation index series from inflation database.  
#' 
#' @details get_inflation_series_db takes an inflation index ticker and returns data series
#'
#'
#' @param db_series Inflation index series name, e.g. CPTFEMU Index
#' @param from Object of class date, the start date for the series
#' @param infl_database_path Path to inflation database, default is the rminfldbpath
#'
#' @return inflation index series
#'
#' @examples
#'
#' get_inflation_series_db("CPTFEMU Index")
#'
#' @importFrom magrittr %>%
#' @export

get_inflation_series_db <- function(db_series,
                                    from = "1990-01-01",
                                    infl_database_path = rminfldbpath){
  
  if (!is.character(db_series)){
    stop('database series not in the correct format, e.g. CPTFEMU Index')
  }
  
  if (!is.character(infl_database_path) | !file.exists(infl_database_path)){
    stop(paste0('database file path not found: ', infl_database_path))
  }
  
  query_string <- paste0("SELECT * FROM inflation_indices WHERE inflation_index = '", db_series, "'")
  
  infl_conn <- DBI::dbConnect(RSQLite::SQLite(), infl_database_path)
  
  inflation_past <- DBI::dbGetQuery(infl_conn, query_string) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::filter(date >= as.Date(from))
  
  DBI::dbDisconnect(infl_conn)  
  
  return(inflation_past)
  
}



#' @title Inflation linked bond static information
#'
#' @description Query and return inflation static information for inflation linked bonds from the inflation database.  
#' 
#' @details linker_static_db takes inflation linked bond ISINs and returns a data frame. If bond_ISIN arguement is 
#'     set to \code{NULL} will return all data. 
#'
#' @param bond_ISIN character, string or vector of strings of inflation linked bond ISINs e.g. US912810SV17
#' 
#' @param lm_database_path Path to list maangement database, default is rmlmdbpath
#'
#' @param infl_database_path Path to inflation database, default is the rminfldbpath
#'
#' @return dataframe of static data for inflation linked bonds
#'
#' @examples
#' \dontrun{ 
#'   linker_static_db(bond_ISIN = "US912810SV17")
#' }
#'
#' @importFrom magrittr %>%
#' @export

linker_static_db <- function(bond_ISIN,
                             lm_database_path = rmlmdbpath,
                             infl_database_path = rminfldbpath){
  
  if (!is.character(infl_database_path) | !file.exists(infl_database_path)){
    futile.logger::flog.error('inflation database path file path not found or is in the wrong format')
  }
  
  if (!is.character(lm_database_path) | !file.exists(lm_database_path)){
    futile.logger::flog.error('list membership database path file path not found or is in the wrong format')
  }
  
  if (!is.character(bond_ISIN) && !is.null(bond_ISIN)){
    futile.logger::flog.error('bond ISIN must be a character string, e.g. "US912828Q608"')
  }
  
  # Special case for when input is NULL
  if(is.null(bond_ISIN)){
    bond <- InflationTools::linker_static_all_db(lm_database_path,
                                                 infl_database_path)
    return(bond)
  }
  
  # First check if bonds are infaltion linked bonds against the list management database 
  lm_conn <- DBI::dbConnect(RSQLite::SQLite(), lm_database_path)
  bonds <- FIRVr::db_lm_get_securities(lm_conn)
  DBI::dbDisconnect(lm_conn)
  
  bond <- bonds %>%
    dplyr::filter(ISIN %in% bond_ISIN)
  
  if(nrow(bond) < 1){
    futile.logger::flog.error("All bond ISINs provided are not in the database.")
  }else if(nrow(bond) < length(bond_ISIN)){
    futile.logger::flog.warn("Not all bond_ISINs provided are in the database.")
  } 
  
  # Second, retrieve the static data about the inflation linked bonds from the inflation database
  query_string <- paste0("SELECT * FROM inflation_linked_static")
  
  infl_conn <- DBI::dbConnect(RSQLite::SQLite(), infl_database_path)
  linker_static <- DBI::dbGetQuery(infl_conn, query_string) %>%
    dplyr::filter(ISIN %in% bond_ISIN)
  
  DBI::dbDisconnect(infl_conn)
  
  if(nrow(linker_static) < length(bond_ISIN)){
    futile.logger::flog.error("A bond ISIN provided is not inflation linked.")
  }
  
  bond <- bond %>%
    dplyr::left_join(linker_static,
                     by = "ISIN") %>%
    tidyr::replace_na(list(CPN_FLOOR = 0, DEFLATION_FLOOR = 0))
  
  return(bond)
  
} 


#' @title Get all database static data
#'
#' @param lm_database_path Path class character to list management database
#' @param infl_database_path Path class character to inflation database
#'
#' @return data.table, static data on inflation linked bonds from the database
#' @export
#'
#' @examples
#' \dontrun{
#'   linker_static_all_db() 
#' }
#' 
#' 
linker_static_all_db <- function(lm_database_path = rmlmdbpath,
                                 infl_database_path = rminfldbpath){
  
  if (!is.character(infl_database_path) | !file.exists(infl_database_path)){
    futile.logger::flog.error('inflation database path file path not found or is in the wrong format')
  }
  
  if (!is.character(lm_database_path) | !file.exists(lm_database_path)){
    futile.logger::flog.error('list membership database path file path not found or is in the wrong format')
  }
  
  # Get all the bonds in inflation linked database 
  query_string <- paste0("SELECT * FROM inflation_linked_static")
  infl_conn <- DBI::dbConnect(RSQLite::SQLite(), infl_database_path)
  linker_static <- DBI::dbGetQuery(infl_conn, query_string) 
  DBI::dbDisconnect(infl_conn)
  
  # Get all securities in the list management database
  lm_conn <- DBI::dbConnect(RSQLite::SQLite(), lm_database_path)
  bonds <- FIRVr::db_lm_get_securities(lm_conn)
  DBI::dbDisconnect(lm_conn)
  
  table <- linker_static %>%
    dplyr::left_join(bonds,
                     by = "ISIN") %>%
    tidyr::replace_na(list(CPN_FLOOR = 0, DEFLATION_FLOOR = 0))
  
  return(table)
  
}

#' Historic inflation curve from database 
#'
#' @description Pull historic projected inflation curves from the database, does not require a bloomberg connection 
#'
#' @param inflation_index character, the name of the inflation index, with bloomberg suffix, eg \code{"CPURNSA Index"} 
#' @param ref_date date, the date of the historic curve 
#' @param infl_database_path charracter, path to inflation database
#'
#' @return data.table, historic projected inflation curve and associated meta data 
#' @export 
#'
#' @examples
#' historic_inflation_curve_db("CPTFEMU Index", 
#'                             ref_date = NULL,
#'                             infl_database_path = rminfldbpath)
#' 
historic_inflation_curve_db <- function(inflation_index,
                                        ref_date = NULL,
                                        infl_database_path = rminfldbpath){
  
  # Connect to the database 
  infl_db_conn <- DBI::dbConnect(RSQLite::SQLite(), 
                                 infl_database_path)
  
  # If ref_date is NULL then jsut get latest curve. Get the date of that curve
  if(is.null(ref_date)){
    ref_date <- DBI::dbGetQuery(infl_db_conn, "SELECT DISTINCT curve_date FROM projected_inflation_curves") %>%
      dplyr::mutate(curve_date = as.Date(curve_date)) %>%
      dplyr::filter(curve_date == max(curve_date)) %>%
      dplyr::pull(curve_date) 
  }  
  
  # Get the historic projected inflation curve for ref_date 
  curve <- DBI::dbGetQuery(infl_db_conn, 
                           paste0("SELECT * FROM projected_inflation_curves")) %>%
    dplyr::mutate(curve_date = as.Date(curve_date),
                  date = as.Date(date)) %>%        
    dplyr::filter(curve_date == ref_date) %>%
    dplyr::select( date, !!inflation_index, curve_date) %>%
    dplyr::rename(proj_index = !!inflation_index) %>%
    stats::na.omit()
  
  DBI::dbDisconnect(infl_db_conn)
  
  return(curve)
  
}

