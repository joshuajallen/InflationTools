

US_eq <- calc_beta_ts_bloomberg("FWISUS55 Index", "PX_LAST",
                                "SPX Index", "PX_LAST",
                                window = 70, 
                                from_date = Sys.Date() - 365, 
                                include_r2 = TRUE)

EU_eq <- calc_beta_ts_bloomberg("FWISEU55 Index", "PX_LAST",
                                "SX5E Index", "PX_LAST",
                                window = 70, 
                                from_date = Sys.Date() - 365, 
                                include_r2 = TRUE)

US_eq2 <- calc_beta_ts_bloomberg("USGGBE03 Index", "PX_LAST",
                                "SPX Index", "PX_LAST",
                                window = 70, 
                                from_date = Sys.Date() - 365, 
                                include_r2 = TRUE)

EU_eq2 <- calc_beta_ts_bloomberg("EUSWSB03 Curncy", "PX_LAST",
                                "SX5E Index", "PX_LAST",
                                window = 70, 
                                from_date = Sys.Date() - 365, 
                                include_r2 = TRUE)




plot_beta_ts(US_eq)
plot_beta_ts(EU_eq)

US_eq %>% select(-c(intercept, `R-squared`)) %>% rename(US = beta) %>%
  left_join(EU_eq, by = "Date") %>% select(-c(intercept, `R-squared`)) %>% rename(EU = beta) %>%
  mutate(EU = EU / 800,
         US = US / 240) %>%
  plotly::plot_ly(x = ~Date, y = ~US, type= "scatter", mode = "lines", name = "US 5Y5Y inflation v S&P 500 ") %>%
  add_trace(x = ~Date, y = ~EU, type= "scatter", mode = "lines", name = "EU 5Y5Y inflation v Eurosoxx 50") %>%
  layout(yaxis = list(title = "Rolling regression beta (scaled 1 Jan 20 = 1)", rangemode = "tozero", showgrid = FALSE),
         xaxis = list(zeroline = TRUE, showline = TRUE, showgrid = FALSE),
         legend = list(x = 0.5, y = 0.9))

US_eq2 %>% select(-c(intercept, `R-squared`)) %>% rename(US = beta) %>%
  left_join(EU_eq2, by = "Date") %>% select(-c(intercept, `R-squared`)) %>% rename(EU = beta) %>%
  mutate(EU = EU / -500,
         US = US / 280) %>%
  plotly::plot_ly(x = ~Date, y = ~US, type= "scatter", mode = "lines", name = "US 3Y breakeven v S&P 500 ") %>%
  add_trace(x = ~Date, y = ~EU, type= "scatter", mode = "lines", name = "EU 3Y breakeven v Eurosoxx 50") %>%
  layout(yaxis = list(title = "Rolling regression beta (scaled 1 Jan 20 = 1)", rangemode = "tozero", showgrid = FALSE),
         xaxis = list(zeroline = TRUE, showline = TRUE, showgrid = FALSE),
         legend = list(x = 0.5, y = 0.9))


US_nom <- calc_beta_ts_bloomberg(
                                #"USSWIT3 Index", "PX_LAST",
                                "USGGBE03 Index", "PX_LAST",
                                "GT3 Govt", "YLD_YTM_MID",
                                window = 70, 
                                from_date = Sys.Date() - 400)

EU_nom <- calc_beta_ts_bloomberg(
                                #"EUSWI3 Curncy", "PX_LAST",
                                "EUSWSB03 Curncy", "PX_LAST",
                                "GTDEM3Y Govt", "YLD_YTM_MID",
                                window = 70, 
                                from_date = Sys.Date() - 400)

plot_beta_ts(US_nom)
plot_beta_ts(EU_nom)

US_nom %>% select(-intercept) %>% rename(US = beta) %>%
  left_join(EU_nom, by = "Date") %>% select(-intercept) %>% rename(EU = beta) %>%
  mutate(EU = EU*-1,
         US = US ) %>%
  plotly::plot_ly(x = ~Date, y = ~US, type= "scatter", mode = "lines", name = "US 3Y breakeven v US 3Y treasury yield") %>%
  add_trace(x = ~Date, y = ~EU, type= "scatter", mode = "lines", name = "EU 3Y breakeven v German 3Y bund yield") %>%
  layout(yaxis = list(title = "Rolling regression beta", rangemode = "tozero", showgrid = FALSE),
         xaxis = list(zeroline = TRUE, showline = TRUE, showgrid = FALSE),
         legend = list(x = 0.4, y = 1))


US_ene <- calc_beta_ts_bloomberg(
  #"USSWIT3 Index", "PX_LAST",
  "USGGBE03 Index", "PX_LAST",
  "CL1 Comdty", "PX_LAST",
  window = 70, 
  from_date = Sys.Date() - 500)

EU_ene <- calc_beta_ts_bloomberg(
  #"EUSWI3 Curncy", "PX_LAST",
  "EUSWSB03 Curncy", "PX_LAST",
  "CO1 Comdty", "PX_LAST",
  window = 70, 
  from_date = Sys.Date() - 500)

plot_beta_ts(US_nom)
plot_beta_ts(EU_nom)

US_ene %>% select(-intercept) %>% rename(US = beta) %>%
  left_join(EU_ene, by = "Date") %>% select(-intercept) %>% rename(EU = beta) %>%
  mutate(EU = EU *-1,
         US = US ) %>%
  plotly::plot_ly(x = ~Date, y = ~US, type= "scatter", mode = "lines", name = "US 3Y breakeven v WTI oil price") %>%
  add_trace(x = ~Date, y = ~EU, type= "scatter", mode = "lines", name = "EU 3Y breakeven v Brent cruse oil price") %>%
  layout(yaxis = list(title = "Rolling regression beta", rangemode = "tozero", showgrid = FALSE),
         xaxis = list(zeroline = TRUE, showline = TRUE, showgrid = FALSE),
         legend = list(x = 0, y = 0.9))
