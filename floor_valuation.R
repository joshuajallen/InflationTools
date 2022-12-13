library(fOptions)

#' @title Calculate floor valuation for TIPs from database 
#'
#' @description (In development do not use!!) This function calculated the value of the floor option in TIPS, 
#' using the generalized Black-Scholes (GBS) formula 
#' 
#' @details floor_valuation takes the current CPI, reference CPI and volatility to back out the implied option of the floor in TIPS 
#'
#' @param bond_ISIN the ISIN of the bond for which the floor value with be calculated 
#'
#' @param current_cpi The latest CPI value from the relevant index 
#'
#' @param reference_cpi The reference CPI value from the relevant index 
#' 
#' @param maturity_date The maturity date of the underlying security  
#' 
#' @param annualised_volatlity Volatility of the reference series, annualised 
#' 
#' @param risk_free_rate The risk free rate, defaults to 0
#' 
#' @return floor valuation for given inflation protected security 
#'
#' @examples
#'
#'     floor_valuation(
#'     current_cpi = log(current_cpi),
#'     reference_cpi = log(linker_static$reference_cpi),
#'     maturity_date = linker_static$MaturityDate,
#'     annualised_volatlity = annualised_volatlity,
#'     risk_free_rate = risk_free_rate,
#'     reference_date = reference_date
#'     )
#'
#'
#' @export


floor_valuation_db <- function(bond_ISIN,
                               lm_database_path = "\\\\markets\\DATA\\DATA\\Offdata\\RM\\_Data\\ListManagement\\list_membership.db",
                               infl_database_path = "\\\\markets\\DATA\\DATA\\Offdata\\RM\\_Data\\Inflation\\inflation.db",
                               current_cpi = NULL,
                               risk_free_rate = NULL,
                               reference_date = Sys.Date()){
  
  futile.logger::flog.warn("Function is currently under development. Please DO NOT USE.")
  
  if (!is.character(infl_database_path) | !file.exists(infl_database_path)){
    futile.logger::flog.error('inflation database path file path not found or is in the wrong format')
  }
  
  if (!is.character(lm_database_path) | !file.exists(lm_database_path)){
    futile.logger::flog.error('list membership database path file path not found or is in the wrong format')
  }
  
  if (!is.character(bond_ISIN)){
    futile.logger::flog.error('Bond ISIN(s) provided not in correct forma, should be a character string/vector')
  }
  

  
  linker_static <- linker_static_db(bond_ISIN = bond_ISIN,
                                    lm_database_path = lm_database_path,
                                    infl_database_path = infl_database_path)
  
  if (linker_static$principal_floor == 0) {
    floor_value <- 0
  } else{
    db_infl_series <-
      get_inflation_series_db(
        linker_static$reference_index,
        from = as.Date("1990-01-01"),
        infl_database_path = infl_database_path
      )
    
    annualised_volatlity <- db_infl_series %>%
      dplyr::mutate(
        index_value = log(index_value),
        change = index_value - dplyr::lag(index_value)
      ) %>%
      dplyr::summarise(ann_vol = sd(change, na.rm = TRUE) * (12 ^ 0.5)) %>%
      dplyr::pull(ann_vol)
    
    
    
    if (is.null(current_cpi)) {
      
      db_infl <- db_infl_series %>%
        dplyr::filter(date <= reference_date)
      
      if (nrow(db_infl) < 1) {
        futile.logger::flog.error("No recent inflation in database for floor valuation please define current_cpi.")
      }
      
      current_cpi <- db_infl %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::pull(index_value)
    }
    
    if (is.null(risk_free_rate)) {
      
      projected_inflation_curve <-
        swap_implied_inflation_data(inflation_index = linker_static$reference_index)
      
      colnames(projected_inflation_curve)[2] <- "proj_infl"
      
      implied_infl <-
        approx(
          x = projected_inflation_curve$maturity,
          y = projected_inflation_curve$proj_infl,
          xout = interval(reference_date, linker_static$MaturityDate) %>% as.numeric('years')
        )$y
      
      risk_free_rate <- log(1 + (implied_infl / 100))
    }
    
    floor_value <- floor_valuation(
      current_cpi = log(current_cpi),
      reference_cpi = log(linker_static$reference_cpi),
      maturity_date = linker_static$MaturityDate,
      annualised_volatlity = annualised_volatlity,
      risk_free_rate = risk_free_rate,
      reference_date = reference_date
    )
    
  }
  
  return(floor_value)
  
}

#' @title Calculate floor valuation for TIPs 
#'
#' @description This function calculated the value of the floor option in TIPS, using the
#' the generalized Black-Scholes (GBS) formula 
#' 
#' @details floor_valuation takes the current CPI, reference CPI and volatility to back out the implied option of the floor in TIPS 
#'
#'
#' @param current_cpi The latest CPI value from the relevant index 
#'
#' @param database_path File path to inflation database, default ////markets//DATA//DATA//Offdata//RM//_Data//Inflation//inflation.db
#' 
#' @param lm_database_path File path to list manager database

#' @param reference_date The reference date  
#' 
#' @param annualised_volatlity Volatility of the reference series, annualised 
#' 
#' @param risk_free_rate The risk free rate, defaults to 0
#' 
#' @return floor valuation for given inflation protected security 
#'
#' @examples
#'
#'     floor_valuation(
#'     current_cpi = log(current_cpi),
#'     reference_cpi = log(linker_static$reference_cpi),
#'     maturity_date = linker_static$MaturityDate,
#'     annualised_volatlity = annualised_volatlity,
#'     risk_free_rate = risk_free_rate,
#'     reference_date = reference_date
#'     )
#'
#'
#' @export


floor_valuation <- function(current_cpi,
                            reference_cpi,
                            maturity_date,
                            annualised_volatlity, 
                            risk_free_rate = 0,
                            reference_date = Sys.Date()
                            ){
  
  futile.logger::flog.warn("Function is currently under development. Please DO NOT USE.")
  
  
  floor_value <- fOptions::GBSOption(TypeFlag = "c",
                                     S = current_cpi,
                                     X = reference_cpi,
                                     Time = interval(reference_date, maturity_date) %>%
                                       as.numeric('years'), 
                                     r = risk_free_rate,
                                     b = 0,
                                     sigma = annualised_volatlity
                                     )@price
  
  return(tibble())
  
}
