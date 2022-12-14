
#' Calculate linker proceeds 
#' 
#' @description Calculate the proceeds of an inflation linked bond on a day given the yield, for notional 
#'      of 100mn local currency. 
#'
#' @param projected_inflation_curve data.table, curve of projected inflation with columns \code{date} and \code{proj_infl}
#'    , eg can be generated by \link[InflationTools]{implied_proj_inflation_curve} 
#' @param real_yield numeric, real yield of bond at reference date in percent
#' @param ref_date date, trade or calculation date, regular settle is assumed in \code{_db} functions
#' @param reference_cpi numeric, the CPI index of the bond at issue, sometimes also called "base CPI"
#' @param inflation_lag numeric, the lag in months that instrument has to the inflation index
#' @param maturity_date date, the date of the maturity ie final cashflow 
#' @param coupon numeric, the coupon in percent to be paid at regular intervals
#' @param payment_frequency numeric, the number of times per year the coupon is to be paid
#' @param settle_convention numeric, the days until the instrument will be settled
#' @param day_count character, the day count convention of the instrument. Can currently only be \code{"ACT/ACT"}
#'
#' @return numeric, the proceeds amount
#' @export
#'
#' @examples
#'  \dontrun{
#'      linker_proceeds_yield(real_yield = -2.5, 
#'                            projected_inflation_curve = exampleUSInflationCurve, 
#'                            reference_cpi = 255,
#'                            inflation_lag = 3, 
#'                            maturity_date = as.Date("2030-10-15"), 
#'                            coupon = 0.25,
#'                            payment_frequency = 2)
#'  }
#' 
linker_proceeds_yield <- function(real_yield,
                                  projected_inflation_curve,
                                  reference_cpi,
                                  inflation_lag,
                                  maturity_date,
                                  coupon,
                                  payment_frequency,
                                  ref_date = Sys.Date(),
                                  settle_convention = 1,
                                  day_count = "ACT/ACT"){
  
  
  cashflow_table <- InflationTools::cashflow_table(maturity_date = maturity_date,
                                                   coupon = coupon,
                                                   payment_frequency = payment_frequency,
                                                   ref_date = ref_date,
                                                   settle_convention = settle_convention,
                                                   day_count = day_count)
  
  real_accrued <- InflationTools::real_accrued(cashflow_table)
  
  real_price <- InflationTools::real_price(cashflow_table, real_yield, payment_frequency, real_accrued) 
  
  current_index_value <- InflationTools::interpolate_inflation_curve(projected_inflation_curve,
                                                                     ref_date = ref_date,
                                                                     inflation_lag = inflation_lag,
                                                                     settle_convention = settle_convention,
                                                                     round_5 = TRUE)
  
  current_index_ratio <- round((current_index_value / reference_cpi), 5)
  
  actual_accrued <- real_accrued * current_index_ratio
  
  principal <- (real_price *  current_index_ratio * 10000)
  
  proceeds <- principal + actual_accrued
  
  return(proceeds)
  
  }