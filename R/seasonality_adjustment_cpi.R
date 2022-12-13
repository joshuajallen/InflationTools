# Mostly take from here: http://collaborate/workspaces/RHelpCentre/R%20Markdown/Seasonal_Adjustment_Wiki.html

# 
# library(x13binary)
# library(seasonal)
# library(DBI)
# #library(dbConnect)
# library(futile.logger)
# library(lubridate)
# library(RSQLite)
# library(purrr)
# library(tibble)
# library(tidyverse)
# library(zoo)

#source("data_import.R")

#' @title Seasonally adjust inflation index series
#'
#' @description Seasonally adjusted inflation index series.  
#'   With the the log transformation function, this functions calls the automatic procedures of X-13ARIMA-SEATS 
#'   to perform a seasonal adjustment on a given index
#' 
#' @details seasonally_adjust takes an inflation index as input as a long format data frame. 
#'   Column names should be 'index_value' for the inflation series, and 'date' for the corresponding date 
#'
#'
#' @param infl_data Object of class 'inflation data'
#' 
#' @param outliers_shift Vector of characters, of any outliers (ao) or level shifts expected in the data. The \code{seas}
#'    function from the seasonal package will identify these automatically, but they can also be pre-specified if the user
#'    has prior knowledge of the data. 
#' 
#' @author Sam H
#' 
#' @return seasonally adjusted series
#'
#' @examples
#' \dontrun{
#'   index_to_date <- past_inflation %>%
#'       dplyr::select(date, index_value) %>%
#'       dplyr::mutate(date = lubridate::as_date(date)) 
#'       
#'   seasonally_adjust(infl_data = index_to_date)
#' }
#'
#' @export

seasonally_adjust <- function(infl_data,  
                              outliers_shift = c("ao2009.Oct", "ao2009.Sep")){
  
  if (!all(c("index_value", "date") %in% colnames(infl_data))) {
    futile.logger::flog.error("inflation data must have colnames: 'index_value', 'date'")
  }
  
  first_date <- infl_data %>% dplyr::filter(date == min(date)) %>% dplyr::pull(date)  
  last_date <- infl_data %>% dplyr::filter(date == max(date)) %>% dplyr::pull(date) 
  
  if(last_date - first_date < 85*30){futile.logger::flog.error("Inflation index does not have enough (7y) data with specified dates!")}
  
  index_seasonal <- stats::ts(infl_data$index_value,
                       start = c(lubridate::year(first_date),
                                 lubridate::month(first_date)),
                       end = c(lubridate::year(last_date),
                               lubridate::month(last_date)),
                       frequency = 12) %>%
    seasonal::seas(regression.variables = outliers_shift,
                    transform.function = "log")                # locked this at log, as multiplicative models seem most appropriate
  
  seasonal_output <- index_seasonal$series$s11 %>%
    tibble::as_tibble() %>%
    dplyr::rename(index_value = x) %>%
    cbind(infl_data %>% dplyr::select(date)) %>%
    dplyr::select(date, index_value)
    
  return(seasonal_output)
  
}


#' @title Seasonally adjust inflation index series from inflation database
#'
#' @description Seasonally adjusted inflation index series from inflation database.  
#' With the the log transformation function, this functions calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment on a given index
#' 
#' @details seasonally_adjust takes an inflation index as input as a long format data frame, queried from inflation database. 
#'
#'
#' @param infl_index Inflation index series name, e.g. CPTFEMU Index
#'
#' @param end_date Object of class date, the end date for the series
#'
#' @param database_path Path to inflation database, default ////markets//DATA//DATA//Offdata//RM//_Data//Inflation//inflation.db
#' 
#' @param outliers_shift Vector of characters, of any outliers (ao) or level shifts expected in the data. The \code{seas}
#'    function from the seasonal package will identify these automatically, but they can also be pre-specified if the user
#'    has prior knowledge of the data. 
#' 
#' @seealso seasonally_adjust
#' 
#' @return seasonalally adjusted series
#'
#' @examples
#'
#' seasonally_adjust_db("CPTFEMU Index")
#'
#' @import DBI
#' @import RSQLite
#' @export


seasonally_adjust_db <- function(infl_index,
                                 end_date = Sys.Date(),
                                 outliers_shift = c("ao2009.Oct", "ao2009.Sep"),
                                 database_path = "\\\\markets\\DATA\\DATA\\Offdata\\RM\\_Data\\Inflation\\inflation.db"){
  
  if (!is.character(infl_index)){
    futile.logger::flog.error('inflation index not in the correct format, e.g. CPTFEMU Index')
  }
  
  if (!lubridate::is.Date(end_date)){
    futile.logger::flog.error('End date not in correct format, must be in format YYYY-MM-DD')
  }
  
  if (!is.character(database_path) | !file.exists(database_path)){
    futile.logger::flog.error(paste0('database file path not found: ', database_path))
  }
  
  query_string <- paste0("SELECT * FROM inflation_indices WHERE inflation_index = '", infl_index, "'")
  
  infl_conn <- DBI::dbConnect(RSQLite::SQLite(), database_path)
  inflation_past <- DBI::dbGetQuery(infl_conn, query_string) %>%
    dplyr::mutate(date = as.Date(date))
  
  DBI::dbDisconnect(infl_conn)  

  if(!(infl_index %in% unlist(inflation_past %>% dplyr::distinct(inflation_index)))){futile.logger::flog.error("Inflation index not in inflation database!")}
  
  index_to_date <- inflation_past %>%
    dplyr::select(date, index_value) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::filter(date > "1980-01-01",
                  date <= end_date)
  
  seasonally_adjust(infl_data = index_to_date)
  
}



#' @title Seasonally adjust inflation index series
#'
#' @description Seasonally adjusted inflation index series.  
#' With the the log transformation function, this functions calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment on a given index
#' 
#' @details seasonally_adjust takes an inflation index as input as a long format data frame. 
#' Column names should be 'index_value' for the inflation series, and 'date' for the corresponding date 
#'
#'
#' @param infl_data Object of class 'inflation data'
#'
#' @param method Defaul method is set to linear 
#'
#' @param outliers_shift Vector of characters, of any outliers (ao) or level shifts expected in the data. The \code{seas}
#'    function from the seasonal package will identify these automatically, but they can also be pre-specified if the user
#'    has prior knowledge of the data. 
#'
#' @return seasonalally adjusted series
#'
#' @examples
#' \dontrun{
#'   index_to_date <- past_inflation %>%
#'       dplyr::select(date, index_value) %>%
#'       dplyr::mutate(date = lubridate::as_date(date)) 
#'        
#'   seasonality_adjustment(infl_data = index_to_date, method = "linear")
#' }
#'
#' @export
#' 


seasonality_adjustment <- function(infl_data, 
                                   method = "linear", 
                                   outliers_shift = c("ao2009.Oct", "ao2009.Sep")){
  
  if (!all(c("index_value", "date") %in% colnames(infl_data))) {
    futile.logger::flog.error("inflation data must have colnames: 'index_value', 'date'")
  }
  
  if (method != "linear") {
    futile.logger::flog.error("linear is the only currently supported method!")
  }
  
  first_date <- infl_data %>% dplyr::filter(date == min(date)) %>% dplyr::pull(date)  
  last_date <- infl_data %>% dplyr::filter(date == max(date)) %>% dplyr::pull(date) 
  
  if(last_date - first_date < 85*30){futile.logger::flog.error("Inflation index does not have enough (7y) data with specified dates!")}
  
  index_seasonal <- stats::ts(infl_data$index_value,
                       start = c(lubridate::year(first_date),
                                 lubridate::month(first_date)),
                       end = c(lubridate::year(last_date),
                               lubridate::month(last_date)),
                       frequency = 12) %>%
    seasonal::seas(regression.variables = outliers_shift,
                   transform.function = "log")                # locked this at log, as multiplicative models seem most appropriate
  
  seasonal_output <- index_seasonal$series$s10 %>%
    tibble::as_tibble() %>%
    utils::tail(60) %>%
    dplyr::mutate(month = seq(from = lubridate::month(last_date) - 59, to = lubridate::month(last_date), by = 1)) %>%
    dplyr::mutate(month = dplyr::if_else(month %% 12 == 0, 12, month %% 12)) 
  
  if(method == "linear"){ 
    seasonal_output <- seasonal_output %>% 
      dplyr::group_by(month) %>%
      dplyr::summarise(seasonal_adjustment = mean(x))
  }
  
  return(seasonal_output) 
  
}


#' @title Seasonally adjust inflation index series from inflation database
#'
#' @description Seasonally adjusted inflation index series from inflation database.  
#' With the the log transformation function, this functions calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment on a given index
#' 
#' @details seasonally_adjust takes an inflation index as input as a long format data frame, queried from inflation database. 
#'
#'
#' @param infl_index Inflation index series name, e.g. CPTFEMU Index
#'
#' @param end_date Object of class date, the end date for the series
#'
#' @param start_date Object of class date, the start date for the series
#'
#' @param database_path Path to inflation database, default ////markets//DATA//DATA//Offdata//RM//_Data//Inflation//inflation.db
#'
#' @param method Defaul method is set to linear 
#' 
#' @param outliers_shift Vector of characters, of any outliers (ao) or level shifts expected in the data. The \code{seas}
#'    function from the seasonal package will identify these automatically, but they can also be pre-specified if the user
#'    has prior knowledge of the data. 
#'
#' @return seasonalally adjusted series
#'
#' @examples
#'
#' seasonality_adjustment_db("CPTFEMU Index")
#'
#'
#' @export


seasonality_adjustment_db <- function(infl_index,
                                      end_date = Sys.Date(),
                                      start_date = as.Date("1980-01-01"),
                                      method = "linear",
                                      outliers_shift = c("ao2009.Oct", "ao2009.Sep"),
                                      database_path = "\\\\markets\\DATA\\DATA\\Offdata\\RM\\_Data\\Inflation\\inflation.db"){
  
  if (!is.character(database_path) | !file.exists(database_path)){
    futile.logger::flog.error(paste0('database file path not found: ', database_path))
  }
  
  if (!lubridate::is.Date(end_date) | !lubridate::is.Date(start_date)){
    futile.logger::flog.error('Date not in correct format, must be in format YYYY-MM-DD')
  }

  if (!is.character(infl_index)){
    futile.logger::flog.error('inflation index not in the correct format, e.g. CPTFEMU Index')
  }
  
  query_string <- paste0("SELECT * FROM inflation_indices WHERE inflation_index = '", infl_index, "'")
  
  infl_conn <- DBI::dbConnect(RSQLite::SQLite(), database_path)
  inflation_past <- DBI::dbGetQuery(infl_conn, query_string) %>%
    dplyr::mutate(date = lubridate::as_date(date))
  
  DBI::dbDisconnect(infl_conn)  
  
  if(!(infl_index %in% unlist(inflation_past %>% dplyr::distinct(inflation_index)))){futile.logger::flog.error("Inflation index not in inflation database!")}
  
  index_to_date <- inflation_past %>%
    dplyr::select(date, index_value) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::filter(date > start_date,
                  date <= end_date)
  
  
  data <- InflationTools::seasonality_adjustment(infl_data = index_to_date,
                                 method = method,
                                 outliers_shift = outliers_shift)
  
  return(data)
  
}

#' @title Seasonally adjust projected inflation series 
#'
#' @description Seasonally adjust projexted inflation index series. This can be used to calculate impact the pricing of inflation linked instruments.
#' With the the log transformation function, this functions calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment on a given index
#' 
#' @details seasonally_adjust takes an inflation index as input as a long format data frame, queried from inflation database. 
#'
#'
#' @param projected_curve Projected index, expects columns names "proj_infl", "date", "season_factor_cuml", can be generated from 
#'    \code{create_linear_unseasonal_proj_curve} or \code{implied_proj_inflation_curve}. 
#'    
#' @param infl_data Object of class 'inflation data'
#'
#' @param proj_is_SA Object of class boolean. Is the projection curve seasonally adjusted TRUE or FALSE
#'
#' @param start_date Object of class date, the start date for the series
#'
#' @param method Default method is set to linear
#' 
#' @param outliers_shift Vector of characters, of any outliers (ao) or level shifts expected in the data. The \code{seas}
#'    function from the seasonal package will identify these automatically, but they can also be pre-specified if the user
#'    has prior knowledge of the data. 
#'
#' @return seasonalally adjusted series
#'
#' @examples
#' \dontrun{
#'  seasonality_adjust_projection(projected_curve = exampleUSInflationCurve, proj_is_SA = FALSE)
#' }
#'
#' @export

seasonality_adjust_projection <- function(projected_curve,
                                    infl_data,
                                    proj_is_SA = FALSE,
                                    start_date = Sys.Date(),
                                    method = "linear",
                                    outliers_shift = c("ao2009.Oct", "ao2009.Sep")    
                                    ){
  
  if (!all(c("proj_infl", "date", "season_factor_cuml") %in% colnames(projected_curve))) {
    futile.logger::flog.error("Projected curve data must have colnames: 'season_factor_cuml', 'proj_infl', 'date'")
  }
  
  if (!lubridate::is.Date(start_date)){
    futile.logger::flog.error('Date not in correct format, must be in format YYYY-MM-DD')
  }
  
  seasonal_adjustment <- InflationTools::seasonality_adjustment(infl_data = infl_data,
                                                 method = method,
                                                 outliers_shift = outliers_shift
                                                 )
  
  adjust_proj <- projected_curve %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::left_join(seasonal_adjustment, by = "month") %>%
    dplyr::mutate(season_factor_cuml = cumprod(seasonal_adjustment))
  
  adjust_proj$season_factor_cuml <- adjust_proj$season_factor_cuml / adjust_proj$season_factor_cuml[1] 
  
  adjust_proj <- adjust_proj %>%
    dplyr::mutate(
      SA = proj_is_SA,
      proj_infl = dplyr::if_else(
        SA == TRUE,
        proj_infl / season_factor_cuml,
        proj_infl * season_factor_cuml
      ),
    ) %>%
    dplyr::select(date, proj_infl, season_factor_cuml)
  
  return(adjust_proj)
  
}

#' @title Seasonally adjust projected inflation series from inflation database
#'
#' @description Seasonally adjust projected inflation index series from inflation database. This can be used to calculate impact the pricing of inflation linked instruments.
#' With the the log transformation function, this functions calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment on a given index
#' 
#' @details seasonally_adjust takes an inflation index as input as a long format data frame, queried from inflation database. 
#'
#'
#' @param projected_curve Projected index, expects columns names "proj_infl", "date", "season_factor_cuml", can be generated from 
#'        \link[InflationTools]{create_linear_unseasonal_proj_curve}.
#'
#' @param proj_is_SA Object of class boolean. Is the projection curve seasonally adjusted TRUE or FALSE
#' 
#' @param projected_index string, inflation index series name, e.g. CPTFEMU Index
#'
#' @param start_date Object of class date, the start date for the series
#'
#' @param method Default method is set to linear 
#' 
#' @param database_path Path to inflation database, default ////markets//DATA//DATA//Offdata//RM//_Data//Inflation//inflation.db
#' 
#' @param outliers_shift Vector of characters, of any outliers (ao) or level shifts expected in the data. The \link[seasonal]{seas}
#'    function from the seasonal package will identify these automatically, but they can also be pre-specified if the user
#'    has prior knowledge of the data. 
#'
#' @return seasonalally adjusted series
#'
#' @examples
#' \dontrun{
#'  seasonality_adjust_projection_db(projected_curve = exampleUSInflationCurve, 
#'                                   projected_index = "CPURNSA Index", 
#'                                   proj_is_SA = FALSE)
#' }
#'
#' @export

seasonality_adjust_projection_db <- function(projected_curve,
                                       projected_index,
                                       proj_is_SA = FALSE,
                                       start_date = Sys.Date(),
                                       method = "linear",
                                       outliers_shift = c("ao2009.Oct", "ao2009.Sep"),
                                       database_path = "\\\\markets\\DATA\\DATA\\Offdata\\RM\\_Data\\Inflation\\inflation.db"
                                       ){
  
  if (!all(c("proj_infl", "date", "season_factor_cuml") %in% colnames(projected_curve))) {
    futile.logger::flog.error("Projected curve data must have colnames: 'season_factor_cuml', 'proj_infl', 'date'")
  }
  
  if (!is.character(projected_index)){
    futile.logger::flog.error('Projected inflation index not in the correct format, e.g. CPTFEMU Index')
  }
  
  if (!lubridate::is.Date(start_date)){
    futile.logger::flog.error('Date not in correct format, must be in format YYYY-MM-DD')
  }
  
  if (!is.character(database_path) | !file.exists(database_path)){
    futile.logger::flog.error('database file path not found')
  }

  seasonal_adjustment <- InflationTools::seasonality_adjustment_db(infl_index = projected_index,
                                                    end_date = min(start_date, Sys.Date()),
                                                    method = method,
                                                    outliers_shift = outliers_shift,
                                                    database_path = database_path
                                                    )
  
  adjust_proj <- projected_curve %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::left_join(seasonal_adjustment, by = "month") %>%
    dplyr::mutate(season_factor_cuml = cumprod(seasonal_adjustment))
  
  adjust_proj$season_factor_cuml <- adjust_proj$season_factor_cuml / adjust_proj$season_factor_cuml[1] 
  
  adjust_proj <- adjust_proj %>%
    dplyr::mutate(
      SA = proj_is_SA,
      proj_infl = dplyr::if_else(
        SA == TRUE,
        proj_infl / season_factor_cuml,
        proj_infl * season_factor_cuml
      ),
    ) %>%
    dplyr::select(date, proj_infl, season_factor_cuml)
  
  
  return(adjust_proj)

}



#' @title Create projected inflation curve 
#'
#' @description Generate a unseasonal inflation curve, for a given inflation rate and base level - useful for pricing inflation instruments using inflation on a forward looking basis 
#' 
#' @details unseasonal inflation curve takes an inflation rate, base level, frequency 
#'
#'
#' @param annualised_inflation_percent annualised projected inflation rate 
#'
#' @param base_level Base level of inflation index 
#' 
#' @param base_month  Base month of inflation series, expects month in number format, e.g. for March: base_month = 3
#'
#' @param base_year  Base year of inflation series, expects year in format 'YYYY'
#'
#' @param frequency frequency of the data, e.g. for monthly, frequency = 12
#' 
#' @param proj_years Number of years to project the series 
#'
#' @return projected inflation curve 
#'
#' @examples
#'
#' create_linear_unseasonal_proj_curve(
#' annualised_inflation_percent = 2,
#' base_level = 150,
#' base_month = lubridate::month(Sys.Date()),
#' base_year = lubridate::year(Sys.Date()),
#' frequency = 12,
#' proj_years = 3
#' )
#'
#'
#' @export

create_linear_unseasonal_proj_curve <- function(annualised_inflation_percent,
                                                base_level,
                                                base_month = lubridate::month(Sys.Date()),
                                                base_year = lubridate::year(Sys.Date()),
                                                frequency = 12,
                                                proj_years = 50){
  
  if (!is.numeric(annualised_inflation_percent)) {
    futile.logger::flog.error('annualised inflation percent must be numeric format, e.g 2')
  }
  
  if (!is.numeric(base_level)) {
    futile.logger::flog.error('base level must be numeric format, e.g 150')
  }
  
  if (!is.numeric(base_level)) {
    futile.logger::flog.error('proj years must be numeric format, e.g 50')
  }
  
  if (!(frequency %in% c(1, 2, 3, 4, 12))) {
    futile.logger::flog.error(
      "frequency can only be monthly (12), quarterly (4), thirdly(?) (3), semi-annual (2), or annual (1)."
    )
  }
  
  by_period <- dplyr::case_when(frequency == 12 ~ "month",
                                frequency == 4 ~ "3 month",
                                frequency == 3 ~ "4 month",
                                frequency == 2 ~ "6 month",
                                frequency == 1 ~ "year", 
                                TRUE ~ "Error")
  
  proj_table <-
    tibble::enframe(
      seq.Date(
        lubridate::as_date(lubridate::as_date(
          paste(base_year, base_month, "01", sep = "-")
        )),
        by = by_period,
        length.out = (frequency * proj_years) + 2
      ),
      value = "date",
      name = NULL
    ) %>%
    dplyr::mutate(index = dplyr::row_number() - 2) %>%
    dplyr::mutate(proj_infl = base_level * (1 + (annualised_inflation_percent /
                                                   100)) ^ (index / frequency),
                  date = date - 1) %>%
    dplyr::select(-index) %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(season_factor_cuml = 1)
  
  return(proj_table)
  
}


#' @title Plot monthly inflation 
#'
#' @description The monthplot shows the changing trend in seasonality in different months over a number of years. 
#' 
#' @details Plots monthly inflation for a given inflation index series
#'
#'
#' @param infl_index Inflation index series name, e.g. CPTFEMU Index
#'
#' @param end_date Object of class date, the end date for the series
#'
#' @param start_date Object of class date, the start date for the series
#'
#' @param database_path Path to inflation database, default ////markets//DATA//DATA//Offdata//RM//_Data//Inflation//inflation.db
#'
#' @param outliers_shift Vector of characters, of any outliers (ao) or level shifts expected in the data. The \code{seas}
#'    function from the seasonal package will identify these automatically, but they can also be pre-specified if the user
#'    has prior knowledge of the data. 
#'
#'
#' @return Monthly inflation plot 
#'
#' @examples
#'
#' monthplot_db("CPURNSA Index")
#'
#'
#' @export

monthplot_db <- function(infl_index,
                         end_date = Sys.Date(),
                         start_date = as.Date("1980-01-01"),
                         outliers_shift = c("ao2009.Oct", "ao2009.Sep"),
                         database_path = "\\\\markets\\DATA\\DATA\\Offdata\\RM\\_Data\\Inflation\\inflation.db"){
  
  if (!is.character(database_path) | !file.exists(database_path)){
    futile.logger::flog.error('database file path not found')
  }
  
  if (!lubridate::is.Date(end_date) | !lubridate::is.Date(start_date)){
    futile.logger::flog.error('Date not in correct format')
  }
  
  if (!is.character(infl_index)){
    futile.logger::flog.error('Projected inflation index not in the correct format, e.g. CPTFEMU Index')
  }
  
  
  query_string <- paste0("SELECT * FROM inflation_indices WHERE inflation_index = '", infl_index, "'")
  
  infl_conn <- DBI::dbConnect(RSQLite::SQLite(), database_path)
  
  inflation_past <- DBI::dbGetQuery(infl_conn, query_string) %>%
    dplyr::mutate(date = lubridate::as_date(date))
  
  infl_conn <- DBI::dbConnect(RSQLite::SQLite(), database_path)
  
  DBI::dbDisconnect(infl_conn)  

  if(!(infl_index %in% unlist(inflation_past %>% dplyr::distinct(inflation_index)))){futile.logger::flog.error("Inflation index not in inflation database!")}
  
  infl_data <- inflation_past %>%
    dplyr::select(date, index_value) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::filter(date > start_date,
                  date <= end_date)
  
  first_date <- infl_data %>% dplyr::filter(date == min(date)) %>% dplyr::pull(date)  
  last_date <- infl_data %>% dplyr::filter(date == max(date)) %>% dplyr::pull(date) 
  
  if(last_date - first_date < 60*30){futile.logger::flog.error("Inflation index does not have enough (5y) data with specified dates!")}
  
  index_seasonal <- stats::ts(infl_data$index_value,
                       start = c(lubridate::year(first_date),
                                 lubridate::month(first_date)),
                       end = c(lubridate::year(last_date),
                               lubridate::month(last_date)),
                       frequency = 12) %>%
    seasonal::seas(regression.variables = outliers_shift,
         transform.function = "log")                # locked this at log, as multiplicative models seem most appropriate
  
  return(stats::monthplot(index_seasonal))
  
}
