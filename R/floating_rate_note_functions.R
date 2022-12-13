
# This is commented out to work on at a later date

#' #' Title
#' #'
#' #' @param price 
#' #' @param projected_irs_curve 
#' #' @param last_fix 
#' #' @param maturity_date 
#' #' @param spread 
#' #' @param payment_frequency 
#' #' @param ref_date 
#' #' @param settle_convention 
#' #' @param day_count 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' FRN_to_fixed_yield <- function(price,
#'                                projected_irs_curve,
#'                                last_fix,
#'                                maturity_date,
#'                                spread = 0,
#'                                payment_frequency,
#'                                ref_date = Sys.Date(),
#'                                settle_convention = 2,
#'                                day_count = "ACT/ACT"){
#'   browser()
#'   # Get the fixed rate for the swap curve at the maturity of the bond and then imply the spread to get the coupon
#'   fixed_equivalent_coupon <- stats::approx(x = projected_irs_curve$maturity, 
#'                                            y = projected_irs_curve$proj_curve,
#'                                            xout = lubridate::time_length(difftime(maturity_date, ref_date), "years")
#'                                            )$y + (spread / 100)
#'   
#'   
#'   cashflow_table <- InflationTools::cashflow_table(maturity_date = maturity_date,
#'                                                    coupon = fixed_equivalent_coupon,
#'                                                    payment_frequency = payment_frequency,
#'                                                    ref_date = ref_date,
#'                                                    settle_convention = settle_convention,
#'                                                    day_count = day_count)
#'   
#'   # The next coupon will already be fixed and different to the current coupon on the swap resulting the cash flow on
#'   # our fake fixed bond will be different. Work out the (actual_cashflow_from_frn + fixed_swap_cashflow - float_swap_cashflow) 
#'   cashflow_table[1, "unadjusted_interest"] <- (((last_fix + (spread / 100))  / payment_frequency) * 10000) +    #actual_cashflow_from_frn
#'     (((fixed_equivalent_coupon - (spread / 100)) / payment_frequency) * (cashflow_table[1, "period_numerator"] / cashflow_table[1, "period_denominator"]) * 10000) 
#'   
#'   accrued <- InflationTools::real_accrued(cashflow_table)
#'   
#'   
#'   yield <-  InflationTools::real_yield(cashflow_table, 
#'                                             price,
#'                                             payment_frequency,
#'                                             accrued)
#'   
#'   return(yield)
#'   
#' }
#' 
#' 
#' curve <- InflationTools::swap_implied_interest_rate_curve("CDOR03") %>%
#'   dplyr::rename(proj_curve = CDOR03)
#' FRN_to_fixed_yield(price = 100.665, projected_irs_curve = curve, last_fix = ,
#'                    maturity_date = as.Date("2025-09-15"), spread = -3.5, payment_frequency = 4)
