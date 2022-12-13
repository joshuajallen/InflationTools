
#' @title Bond inflation swap
#'
#' @description Returns bond inflation swap table, including NPV, fixed/floating payments and IE01
#' 
#' @details inflation swap pricer, for a given start, maturity, fixed rate, notional, CPI reference and projected inflation curve 
#'
#' @param start_date Object of class date, the start date for the swap
#' 
#' @param maturity_date Object of class date, the maturity date for the series
#'
#' @param fixed_rate Class numeric, the fixed payment of the inflation swap 
#' 
#' @param notional Class numeric, the notional amount of the inflation swap 
#' 
#' @param bond_coupon Class numeric, coupon of the bond 
#' 
#' @param inflation_lag Class numeric, the inflation lag for the index, default is 3 (months)
#' 
#' @param reference_cpi Class numeric, reference CPI value 
#' 
#' @param inflation_payment_frequency Class numeric, the payment frequency, defaul is semi-annual (2)
#' 
#' @param fixed_payment_frequency Class numeric, the fixed payment frequency, defaul is semi-annual (2)
#' 
#' @param projected_inflation_curve Class data.frame, projected inflation curve for the swap 
#' 
#' @param interpolation Class character, defaults to monthly, inflation interpolation method 
#' 
#' @param inflation_day_count Class character, defaults to ACT/ACT, day count convention
#' 
#' @param fixed_day_count Class character, defaults to ACT/ACT, day count convention
#' 
#' @return data frame of projections implied from the curve 
#'
#' @examples
#'
#' implied_proj_inflation_curve_db("CPURNSA Index")
#'
#'
#' @export
#' 


inflation_bond_swap <- function(start_date,
                                maturity_date,
                                bond_coupon,
                                fixed_rate,
                                notional,
                                inflation_lag = 3,
                                reference_cpi,
                                inflation_payment_frequency,
                                fixed_payment_frequency,
                                projected_inflation_curve,
                                interpolation = "monthly",
                                inflation_day_count = "ACT/ACT",
                                fixed_day_count = "ACT/ACT") {
  if (!is.numeric(fixed_rate)) {
    futile.logger::flog.error('fixed rate must be a numeric')
  }
  
  if (!is.numeric(inflation_lag)) {
    futile.logger::flog.error('inflation lag must be a numeric')
  }
  
  if (!is.numeric(inflation_payment_frequency) |!is.numeric(fixed_payment_frequency)) {
    futile.logger::flog.error('payment frequency lag must be a numeric')
  }
  
  if (!is.numeric(reference_cpi)) {
    futile.logger::flog.error('reference cpi must be a numeric')
  }
  
  if (!is.numeric(bond_coupon)) {
    futile.logger::flog.error('bond coupon must be a numeric')
  }
  
  cashflow_table <-
    tibble::enframe(
      jrvFinance::coupons.dates(
        settle = "2019-01-01",
        mature = maturity_date,
        freq = payment_frequency
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
      dplyr::select(-c(rown, prev_periods, cashflow_period_days)) %>%
      dplyr::filter(unadjusted_principal != 0) 
    
  } else if (day_count == "NL/365") {
    futile.logger::flog.error(
      paste0(
        "Day count method ",
        day_count,
        " is not currently supported. See cashflow_table function."
      )
    )
    
  } else{
    futile.logger::flog.error(
      paste0(
        "Day count method ",
        day_count,
        " is not currently supported. See cashflow_table function."
      )
    )
  }
  
  proj_infl <-
    interpolate_projected_inflation(
      cashflow_table = cashflow_table,
      projected_inflation_curve = projected_inflation_curve,
      settle_convention = 0,
      inflation_lag = inflation_lag
    )
  
  cashflow_table <- cashflow_table %>%
    cbind(proj_infl) %>%
    dplyr::mutate(
      fixed_cashflow = unadjusted_principal * ((1 + (fixed_rate / 100)) ^ discount_period_fraction),
      floating_cashflow = unadjusted_principal * (round(proj_infl / reference_cpi, 5)),
      curve_implied_inflation = ((proj_infl / reference_cpi) ^ (1 / discount_period_fraction)) - 1,
      iv01_calc = unadjusted_principal * discount_period_fraction * ((1 + curve_implied_inflation) ^ (discount_period_fraction - 1)) / 10000
    )
  
  MTM <- sum(cashflow_table$floating_cashflow) - sum(cashflow_table$fixed_cashflow)
  
  iv01 <- sum(cashflow_table$iv01_calc)
  
  implied_inflation <- tail(cashflow_table$curve_implied_inflation)
  
  output_list <- list(value = MTM,
                      iv01 = iv01,
                      implied_inflation = implied_inflation)
  
  return(output_list)
  
}
