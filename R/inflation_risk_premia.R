
#' @title Inflation risk premia modelled estimation 
#'
#' @description Returns modelled values for inflation risk premia, for a given maturity date 
#' 
#' @details Risk premia is estimate using a set of liquidity factors (VIX, MOVE, bid-offer) and inflation expectations, the inflation risk premia 
#'          is the breakeven inflation not explained by liquidity or expectations 
#'
#' @param maturity_input Object of class numeric, the maturity of the breakeven inflation (e.g. 5, 10, 30)
#' 
#' @param start_date Object of class date, the start date for the estimation
#'
#' @param regression_window numeric, rolling window length for the regression 
#' 
#' @param rolling Boolean, if TRUE (default), runs a rolling regression. If FALSE, runs a simple linear regression. 
#' 
#' @return data frame of projections of breakeven inflation, inflation risk premia, liquidity factors and residuals. 
#'
#' @examples
#' \dontrun{
#'   inflation_risk_premia(maturity_input = 10, 
#'                         start_date = Sys.Date() - 8*365, 
#'                         regression_window = 200)
#' }
#'
#' @importFrom magrittr %>%
#' @export
#' 


inflation_risk_premia <- function(maturity_input, start_date = Sys.Date() - 8*365, regression_window = 200, rolling = TRUE){
  
  maturity <- stringr::str_extract(string = maturity_input, pattern = "[[:digit:]]{1,2}")
  
  if(Sys.Date() - start_date < regression_window){
    futile.logger::flog.error("regression window must be less than the number of observations in the series")
    return(data.frame())
  }
  
  if(!maturity %in% c("2", "5", "10", "30")){
    futile.logger::flog.error("Maturity must be 2, 5, 10 or 30")
    return(data.frame())
  }
  
  if(!rolling %in% c(TRUE, FALSE)){
    futile.logger::flog.error("Rolling input must be TRUE or FALSE")
    return(data.frame())
  }
  
  # config settings 
  
  opt <- c(
    "periodicitySelection" = "DAILY",
    "nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
    "nonTradingDayFillMethod" = "PREVIOUS_VALUE"
  )
  
  securities_inflation_model <-
    readr::read_csv(
      file = file.path("\\\\markets\\DATA\\DATA\\Offdata\\RM\\_Data\\Inflation\\InflationModelling\\securities_inflation_model.csv"),
      col_types = readr::cols(
        measure =  readr::col_character(),
        field =  readr::col_character(),
        ticker =  readr::col_character(),
        maturity =  readr::col_character(),
        model =  readr::col_character()
      )
    )
  
  #securities_config <- InflationTools::securities_inflation_model %>% dplyr::filter(model %in% c("Inflation"))
  securities_config <- securities_inflation_model %>% dplyr::filter(model %in% c("Inflation"))
  
  df <- FIRVr::bloomberg_query(
    securities_config$ticker,
    c("px_last", "px_bid", "px_ask"),
    from_date = lubridate::ymd(start_date),
    to_date = Sys.Date(), 
    options = opt
  )
  
  data <- df %>% 
    dplyr::left_join(securities_config, by = c("Security" = "ticker")) %>% 
    dplyr::filter(maturity %in% c("ALL", as.character(maturity_input)))
  
  inflation_ticker <- unique(data$Security[data$measure == "ILB"])
  nominal_ticker <- unique(data$Security[data$measure == "Nominal"])
  predictors <- unique(data$Security[data$maturity == "ALL"])
  
  bid_offer <-
    data %>% 
    dplyr::filter(measure %in% c("ILB", "Nominal")) %>% 
    tidyr::spread(key = Field, value = Value) %>% 
    dplyr::mutate(bid_offer = px_ask - px_bid) %>% 
    dplyr::select(Security, Date, maturity, bid_offer) %>% 
    tidyr::spread(key = Security, value = bid_offer) %>% 
    dplyr::mutate(BID_ASK = !! rlang::sym(nominal_ticker) - !! rlang::sym(inflation_ticker)) %>% 
    dplyr::select(Date, BID_ASK)
  
  model_data <- df %>% 
    dplyr::filter(Field == "px_last") %>% 
    tidyr::spread(key = Security, value = Value)
  
  model_data <-
    Reduce(
      function(x, y)
        merge(
          x = x,
          y = y,
          by = "Date",
          all.x = T,
          all.y = T
        ),
      list(
        model_data,
        bid_offer
      )
    ) %>% 
    dplyr::select(Date, !!!dplyr::quos(predictors), BID_ASK, inflation_ticker, nominal_ticker) %>% 
    dplyr::mutate(BREAKEVEN = !! rlang::sym(nominal_ticker) - !! rlang::sym(inflation_ticker)) %>% 
    tidyr::drop_na()
  
  
  # fit model 
  
  if(rolling){
    
    model <-
      rollRegres::roll_regres(
        BREAKEVEN ~ `VIX Index` +  `MOVE Index` + `T5YIFR Index` + BID_ASK,
        data = as.data.frame(model_data),
        width = regression_window,
        do_compute = c("sigmas", "r.squareds", "1_step_forecasts")
      )
    
    
    coeefs <- tibble::as_tibble(as.data.frame(model$coefs))
    colnames(coeefs) <-
      c("Intercept", "VIX",  "MOVE", "5Y5Y", "BID_ASK")
    
    linear_model_preds <- data.frame(
      Date = model_data$Date,
      Breakeven = model_data$BREAKEVEN,
      predicted = model$one_step_forecasts,
      residual = model_data$BREAKEVEN -  model$one_step_forecasts,
      Intercept =  coeefs$Intercept,
      LiquidityFactors = coeefs$VIX * model_data$`VIX Index` + coeefs$MOVE *
        model_data$`MOVE Index` + coeefs$BID_ASK * model_data$BID_ASK
    )
    
    linear_model_preds$IRP <-
      linear_model_preds$Breakeven - linear_model_preds$LiquidityFactors - model_data$`T5YIFR Index`  + linear_model_preds$residual
    
    
  } else {
    
    model <-
      stats::lm(BREAKEVEN ~ `VIX Index` +  `MOVE Index` + `T5YIFR Index` + BID_ASK,
                data = as.data.frame(model_data))
    
    linear_model_preds <- data.frame(
      Date = model_data$Date,
      Breakeven = model_data$BREAKEVEN,
      predicted = stats::predict(model),
      residual = model_data$BREAKEVEN - stats::predict(model),
      Intercept =  model$coefficients["(Intercept)"],
      LiquidityFactors = model_data$`MOVE Index` *
        model$coefficients[["`MOVE Index`"]] +
        model_data$BID_ASK * model$coefficients[["BID_ASK"]] +
        model_data$`VIX Index` * model$coefficients[["`VIX Index`"]]
    )
    
    linear_model_preds$IRP <-
      linear_model_preds$Breakeven - linear_model_preds$LiquidityFactors - model_data$`T5YIFR Index`  + linear_model_preds$residual
    
  }
  
  return(linear_model_preds)
  
}



