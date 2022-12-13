## Inflation dashboard

#setwd("N:/328890/Inflation markets/Inflation dashboard")

library(dplyr)
library(data.table)
library(readxl)
library(stringr)
library(ggplot2)
library(plotly)
library(FIRVr, lib.loc = 'N:/Offdata/RM/_R code repository/RMPackages')
library(Rblpapi)
library(stringi)
library(lubridate)
library(tibble)
library(shiny)
library(readr)
library(tidyr)
library(DT)
library(tibble)
library(purrr)
library(roll)


# Read in data

assets <- c("USGGBE03 Index", "USGGBE05 Index", "USGGBE10 Index", "USGGBE30 Index", "EUSWIT3 Curncy", "EUSWIT5 Curncy", "EUSWIT10 Curncy", "EUSWIT30 Curncy", "co1 Comdty", "USGG3YR Index", "USGG5YR Index", "USGG10YR Index", "USGG30YR Index",
            "GTDEM3Y Govt", "GTDEM5Y Govt", "GTDEM10Y Govt", "GTDEM30Y Govt" )
flds <- c("LAST_PRICE", "CRNCY", "YLD_YTM_MID")

prices <- bloomberg_query(assets, flds, from_date = today()- (365*3), to_date = today(), options = NULL)

# Linear models

## Scatter plots

breakeven_10 <- prices %>%
  filter(Security == "USGGBE10 Index", Field == "LAST_PRICE") %>%
  select(Value)

nominal_10 <- prices %>%
  filter(Security == "USGG10YR Index", Field == "LAST_PRICE") %>%
  select(Value)

fv <- lm(as.matrix(as.numeric(unlist((breakeven_10)))) ~ as.matrix(as.numeric(unlist(nominal_10))))

#df <- cbind(nominal_10, breakeven_10, fv)

df <- cbind(nominal_10, breakeven_10)

#colnames(df) <- c("nominal_10", "breakeven_10", "fv")
colnames(df) <- c("nominal_10", "breakeven_10")

plot_ly(df, x = ~nominal_10, y = ~breakeven_10, mode = "markers", type = "scatter", name = "actual values") %>%
    add_trace(x = ~nominal_10, y = fv$fitted.values, mode = "lines", name = "fitted values", line = list(color = 'rgb(128, 0, 0)')) #%>%

beta <- fv$coefficients[2]

## Rolling regression

# Select maturity, select independent variable
# Turn to function: inputs: Independent variable, dependent variable, function (linear regression)

breakeven_10 <- prices %>%
  filter(Security == "USGGBE10 Index", Field == "LAST_PRICE") %>%
  select(Value)

nominal_10 <- prices %>%
  filter(Security == "USGG10YR Index", Field == "LAST_PRICE") %>%
  select(Value)

nominal_10 <- as.matrix(as.numeric(unlist(nominal_10)))
breakeven_10 <- as.matrix(as.numeric(unlist(breakeven_10)))

roll_10 <- roll_lm(nominal_10,breakeven_10, 28, intercept = FALSE)
roll_10 <- roll_10$coefficients

dates <- prices %>%
  filter(Security == "USGGBE10 Index", Field == "LAST_PRICE") %>%
  select(Date)

roll_10 <- cbind(dates, roll_10)

roll_10$Date <- as.character(roll_10$Date)
roll_10$Date <- as.Date(roll_10$Date, format = "%Y-%m-%d")

roll_10 <- na.omit(roll_10)

plot_ly(roll_10, x = ~Date,
        y = ~x1,
        type= "scatter", mode = "lines") %>%
  layout(yaxis = list(title = "One month rolling beta"))
