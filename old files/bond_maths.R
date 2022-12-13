

library(lubridate)
library(jrvFinance)
library(tibble)
library(tidyverse)

library(readxl)

source("seasonality_adjustment_cpi.R")
source("inflation_db_functions.R")
source("data_import.R")

conventions <- read_excel("conventions.xlsx")

TII <- linkers[82,] %>% cbind(conventions %>% filter(ticker == "TII")) %>% mutate(ref_date = Sys.Date(), 
                                                                                   maturity_date = MaturityDate, 
                                                                                   nominal_amount = 1500000,
                                                                                   coupon = 0.625,
                                                                                   inflation_lag = 3, 
                                                                                   base_cpi = 225.38381, 
                                                                                   coupon_floor = 0,
                                                                                   inflation_floor = 100,
                                                                                   real_yield = -1.13,
                                                                                   payment_freq = 2)


past_inflation <- get_inflation_series_db("CPURNSA Index", from = Sys.Date() - 6*30)
projected_curve <- create_linear_unseasonal_proj_curve(1.5,
                                                       past_inflation %>% filter(date == max(date)) %>% pull(index_value),
                                                       past_inflation %>% filter(date == max(date)) %>% mutate(date = month(date)) %>% pull(date), 
                                                       past_inflation %>% filter(date == max(date)) %>% mutate(date = year(date)) %>% pull(date),
                                                       12,
                                                       32)

projected_curve <- seasonality_adjust_projection_db(projected_curve, "CPURNSA Index")
inflation_curve <- past_inflation %>%
  select(-inflation_index) %>% 
  rename(proj_infl = index_value) %>%
  rbind(projected_curve %>% 
          select(-season_factor_cuml) %>%
          filter(date != min(date)))


cashflow_table <- tibble::enframe(jrvFinance::coupons.dates(settle = "2019-01-01",
                                                            mature = TII$maturity_date,
                                                            freq = TII$payment_freq),
                                  value = "cashflow_date",
                                  name = NULL
                                  ) %>%
  dplyr::mutate(days_from_start = cashflow_date - min(cashflow_date),
                cashflow_period_days = as.numeric(days_from_start - lag(days_from_start, default = days_from_start[1]))) %>%
  dplyr::filter(cashflow_date > TII$ref_date + TII$settle_conv) %>%
  dplyr::select(-days_from_start) %>% 
  dplyr::mutate(unadjusted_interest = TII$nominal_amount * TII$coupon / (100 * TII$payment_freq),
                unadjusted_principal = dplyr::if_else(TII$maturity_date == cashflow_date, TII$nominal_amount, 0),
                period_numerator = as.numeric(cashflow_date - TII$ref_date + TII$settle_conv) - 2,
                period_denominator = cashflow_period_days,
                unadjusted_cashflow = unadjusted_interest + unadjusted_principal,
                discount_period_fraction = (floor(period_numerator / period_denominator)) + (period_numerator[1] / period_denominator[1])) 


    
proj_infl <- tibble::enframe(sapply(cashflow_table$cashflow_date, 
                                    function(xout){round(approx(inflation_curve$date, 
                                                                inflation_curve$proj_infl, 
                                                                xout = (xout %m-% months(TII$inflation_lag - 1) + TII$settle_conv - 1))$y, 
                                                         5)}
                                    ),
                             value = "proj_infl",
                             name = NULL)

cashflow_table <- cashflow_table %>%
  cbind(proj_infl) %>%
  dplyr::mutate(inflation_ref_index = round(proj_infl / TII$base_cpi, 5),
                adjusted_interest = unadjusted_interest * pmax(inflation_ref_index, TII$coupon_floor/100),
                adjusted_principal = unadjusted_principal * pmax(inflation_ref_index, TII$principal_floor/100),
                adjusted_cashflow = adjusted_interest + adjusted_principal) 




real_accrued <- cashflow_table$unadjusted_interest[1] * (cashflow_table$period_denominator[1] - cashflow_table$period_numerator[1]) / cashflow_table$period_denominator[1]

clean_price <- cashflow_table %>%
  dplyr::mutate(real_cashflow_value = unadjusted_cashflow / ((1+(TII$real_yield / (100*TII$payment_freq)))^discount_period_fraction)) %>%
  dplyr::summarise(price = 100*(sum(real_cashflow_value) - real_accrued) / TII$nominal_amount)

current_index_value <- round(approx(x = inflation_curve$date, y = inflation_curve$proj_infl, xout = Sys.Date() %m-% months(TII$inflation_lag - 1) + TII$settle_conv - 1)$y, 5)

current_index_ratio <- round((current_index_value / TII$base_cpi), 5)

actual_accrued <- real_accrued * current_index_ratio

principal <- (clean_price *  current_index_ratio * TII$nominal_amount) / 100

dirty_proceeds <- actual_accrued + principal

nominal_function <- function(nominal_equiv_yield){
  
  adj_clean_price <- cashflow_table %>%
    dplyr::mutate(adj_cashflow_value = adjusted_cashflow / ((1+(nominal_equiv_yield / (100*TII$payment_freq)))^discount_period_fraction)) %>%
    summarise(price = 100*(sum(adj_cashflow_value)-real_accrued)/TII$nominal_amount)
  
  output <- (adj_clean_price * TII$nominal_amount / 100) - principal
  
  abs(as.numeric(output))
  
}

real_function <- function(real_yield_func){
  
  adj_clean_price <- cashflow_table %>%
    dplyr::mutate(real_cashflow_value = unadjusted_cashflow / ((1+(real_yield_func / (100*TII$payment_freq)))^discount_period_fraction)) %>%
    dplyr::summarise(price = 100*(sum(real_cashflow_value) - real_accrued) / TII$nominal_amount)
  
  output <- adj_clean_price - TII$clean_price
  
  abs(as.numeric(output))
  
}

