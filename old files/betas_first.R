library(FIRVr, lib.loc = 'N:/Offdata/RM/_R code repository/RMPackages')
library(futile.logger)
library(lubridate)
library(plotly)
library(Rblpapi)
library(roll)
library(tidyverse)

source("beta_func.R")



beta_ts <- calc_beta_ts_bloomberg("USGGBE02 Index", "PX_LAST",            # first the security then the field (dependent variable)
                                  "CL1 Comdty", "PX_LAST",                # then second security and field (independent variable)
                                  from_date = Sys.Date() - (2*365),         # date to start regression
                                  window = 90,                            # the size of the rolling regression window in days 
                                  method = "change",                      # the regression method. options are "change", "level", "percent_change", "log_change", "log_level"
                                  change_window = 5                       # if choosing a change method, the window of the change in days, eg 5 is change on week 
                                  )

# plotting function
plot_beta_ts(beta_ts, 
             include_intercept = FALSE) 


#Second beta
beta_ts <- calc_beta_ts_bloomberg("USGGBE03 Index", "PX_LAST",            # first the security then the field (dependent variable)
                                  "GT3 Govt", "YLD_YTM_MID",                # then second security and field (independent variable)
                                  from_date = Sys.Date() - (365),         # date to start regression
                                  window = 90,                            # the size of the rolling regression window in days 
                                  method = "change",                      # the regression method. options are "change", "level", "percent_change", "log_change", "log_level"
                                  change_window = 5                       # if choosing a change method, the window of the change in days, eg 5 is change on week 
)

# plotting function
plot_beta_ts(beta_ts, 
             include_intercept = FALSE) 


# Third beta
beta_ts <- calc_beta_ts_bloomberg("UKGGBE05 Index", "PX_LAST",            # first the security then the field (dependent variable)
                                  "GTGBP5Y Corp", "YLD_YTM_MID",                # then second security and field (independent variable)
                                  from_date = Sys.Date() - (365*2),         # date to start regression
                                  window = 90,                            # the size of the rolling regression window in days 
                                  method = "change",                      # the regression method. options are "change", "level", "percent_change", "log_change", "log_level"
                                  change_window = 5                       # if choosing a change method, the window of the change in days, eg 5 is change on week 
)

# plotting function
plot_beta_ts(beta_ts, 
             include_intercept = FALSE) 












# securities <- c("USGGBE03 Index", "USGGBE05 Index", "USGGBE10 Index", 
#                 "DEGGBE03 Index", "DEGGBE05 Index", "DEGGBE10 Index",
#                 "GT3 Govt", "GT5 Govt", "GT10 Govt",
#                 "GTDEM3Y Corp", "GTDEM5Y Corp", "GTDEM10Y Corp", 
#                 "CO1 Comdty", "CL1 Comdty")
# 
# time_series <- bloomberg_query(securities,
#                                fields = c("PX_LAST", "YLD_YTM_MID"),
#                                from_date = today() - (365*3), 
#                                to_date = today()) 
# 
# breakeven <- "USGGBE03 Index" 
# nominal <- "GT3 Govt"
# oil_s <- "CO1 Comdty"
# wti_s <- "CL1 Comdty"
# 
# dependent <- time_series %>% 
#   filter(Field == "PX_LAST", Security == breakeven) %>%
#   select(Date, Value) 
# 
# independent <- time_series %>% 
#   filter(Field == "YLD_YTM_MID", Security == nominal) %>%
#   select(Date, Value)
# 
# oil <- time_series %>% 
#   filter(Field == "PX_LAST", Security == oil_s) %>%
#   select(Date, Value) 
# 
# wti <- time_series %>% 
#   filter(Field == "PX_LAST", Security == wti_s) %>%
#   select(Date, Value) 
# 
# 
# beta_ts <- calc_beta_ts(dependent,
#                         independent,
#                         window = 120,
#                         method = "change",
#                         change_window = 5)
# 
# plot_beta_ts(beta_ts, 
#              include_intercept = FALSE) 
# 
# beta_ts <- calc_beta_ts(dependent,
#                         oil,
#                         window = 120,
#                         method = "change",
#                         change_window = 5)
# 
# plot_beta_ts(beta_ts, 
#              include_intercept = FALSE) 
# 
# beta_ts <- calc_beta_ts(dependent,
#                         wti,
#                         window = 90,
#                         method = "change",
#                         change_window = 5)
# 
# 
# plot_beta_ts(beta_ts, 
#              include_intercept = FALSE) 