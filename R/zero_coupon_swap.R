

#' @title Zero coupon inflation swap
#'
#' @description Returns zero coupon inflation swap information, including NPV, fixed/floating payments, DV01 and IE01
#' 
#' @details Zero coupon inflation swap calculations, for a given start, maturity, fixed rate, notional, CPI reference and projected inflation curve 
#'
#' @param start_date Object of class date, the start date for the swap, if NULL defaults to settle convention days after the reference date 
#' 
#' @param ref_date date, the trade date of the  
#' @param maturity_date date, the maturity date for the series
#' @param fixed_rate Class numeric, the fixed payment of the ZC inflation swap 
#' @param notional Class numeric, the notional amount of the ZC inflation swap 
#' @param projected_inflation_curve data.table, curve of projected inflation with columns \code{date} and \code{proj_infl},
#'    eg can be generated by \link[InflationTools]{implied_proj_inflation_curve}
#' @param inflation_lag Class numeric, the inflation lag for the index, default is 3 (months)
#' @param reference_cpi Class numeric, reference CPI value 
#' @param settle_convention numeric, the days until the instrument will be settled
#' @param start_date class, 
#' @param interpolation Class character, can be daily or monthly, inflation interpolation method 
#' @param day_count Class character, defaults to ACT/ACT, day count convention
#' 
#' @return list of swap characteristics
#'
#' @examples
#'
#' zero_coupon_inflation_swap(ref_date = Sys.Date(), maturity_date = "2025-04-02",
#'                            fixed_rate = 2.71, notional = 10000000,
#'                            projected_inflation_curve = exampleUSInflationCurve)
#'
#'
#' @export
#' 
zero_coupon_inflation_swap <- function(ref_date,
                                       maturity_date,
                                       fixed_rate,
                                       notional,
                                       projected_inflation_curve,
                                       inflation_lag = 3,
                                       reference_cpi = 259.918,
                                       settle_convention = 2,
                                       start_date = NULL,
                                       interpolation = "daily",
                                       day_count = "ACT/ACT"){
  
  if (!is.numeric(fixed_rate)){
    futile.logger::flog.error('fixed rate must be a numeric')
  }
  if (!is.numeric(inflation_lag)){
    futile.logger::flog.error('inflation lag must be a numeric')
  }
  if (!is.numeric(reference_cpi)){
    futile.logger::flog.error('reference cpi must be a numeric')
  }
  
  if(is.null(start_date)){
    start_date = ref_date + settle_convention
    # Start date should not be on a weekend
    if(lubridate::wday(start_date) %in% c(1,7)){
      start_date = start_date + 2
    }
  }
  
  cashflow_table <-
    tibble::enframe(
      jrvFinance::coupons.dates(
        settle = "2019-01-01",
        mature = maturity_date,
        freq = 1
      ),
      value = "cashflow_date",
      name = NULL
    ) %>%
    dplyr::mutate(
      days_from_start = cashflow_date - start_date,
      cashflow_period_days = as.numeric(
        days_from_start - dplyr::lag(days_from_start, default = days_from_start[1])
      )
    ) %>%
    dplyr::filter(cashflow_date > start_date) %>%
    dplyr::select(-days_from_start) %>%
    dplyr::mutate(unadjusted_principal = dplyr::if_else(maturity_date == cashflow_date, notional, 0))
  
  if (day_count == "ACT/ACT") {
    cashflow_table <- cashflow_table %>%
      dplyr::mutate(
        period_numerator = as.numeric(cashflow_date - start_date),
        period_denominator = cashflow_period_days,
        rown = dplyr::row_number(),
        prev_periods = cumsum(period_denominator) - period_denominator,
        discount_period_fraction = dplyr::if_else(
          period_denominator + prev_periods == period_numerator,
          as.numeric(rown),
          floor(period_numerator / period_denominator) + (period_numerator[1] / period_denominator[1])
        )
      ) %>%
      dplyr::select(-c(rown, prev_periods, cashflow_period_days, period_numerator, period_denominator)) %>%
      dplyr::filter(unadjusted_principal != 0) 
    
  } else if (day_count == "NL/365") {
    
    futile.logger::flog.error(
      paste0( "Day count method ", day_count, " is not currently supported. See cashflow_table function."
      )
    )
    
  } else{
    futile.logger::flog.error(
      paste0( "Day count method ", day_count, " is not currently supported. See cashflow_table function."
      )
    )
  }
  
  proj_infl <-
    InflationTools::interpolate_projected_inflation(
      cashflow_table = cashflow_table,
      projected_inflation_curve = projected_inflation_curve,
      settle_convention = 0,
      inflation_lag = inflation_lag,
      interpolation = interpolation
    )
  
  cashflow_table <- cashflow_table %>%
    cbind(proj_infl) %>%
    dplyr::mutate(
      fixed_cashflow = unadjusted_principal * ((1 + (fixed_rate / 100)) ^ discount_period_fraction),
      floating_cashflow = unadjusted_principal * (round(proj_infl / reference_cpi, 5)),
      curve_implied_inflation = ((proj_infl / reference_cpi) ^ (1 / discount_period_fraction)) - 1,
      ie01_calc = unadjusted_principal * discount_period_fraction * ((1 + curve_implied_inflation) ^ (discount_period_fraction - 1)) / 10000, 
      ie_conv = unadjusted_principal * discount_period_fraction * (discount_period_fraction - 2) * 
        ((1 + curve_implied_inflation) ^ (discount_period_fraction - 2)) / 1000000
    )
  
  MTM <- sum(cashflow_table$floating_cashflow) - sum(cashflow_table$fixed_cashflow)
  
  ie01 <- sum(cashflow_table$ie01_calc)
  
  implied_inflation <- cashflow_table$curve_implied_inflation[[nrow(cashflow_table)]]
  
  dv01 <- cashflow_table$discount_period_fraction[[nrow(cashflow_table)]] * MTM / 10000
  
  inflation_convexity <- sum(cashflow_table$ie_conv)
  
  output_list <- list(
    NPV = MTM,
    floating_cashflow = sum(cashflow_table$floating_cashflow),
    fixed_cashflow = sum(cashflow_table$fixed_cashflow),
    ie01 = ie01,
    dv01 = dv01,
    implied_inflation = implied_inflation * 100,
    inflation_convexity = inflation_convexity
    
  )
  
  return(output_list)
  
}
