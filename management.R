

# Call all required packages 
library(DBI)
library(FIRVr, lib.loc = "N:/Offdata/RM/_R code repository/RMPackages")
library(fOptions)
library(futile.logger)
library(lubridate)
library(jrvFinance)
library(plotly)
library(purrr)
library(readxl)
library(Rblpapi)
library(RSQLite)
library(roll)
library(seasonal,  lib.loc = "C:/Program Files/R/R-3.6.2/library")  # needs to be installed in C drive due to executable element of package
library(tibble)
library(tidyverse)
library(x13binary,  lib.loc = "C:/Program Files/R/R-3.6.2/library")  # needs to be installed in C drive due to executable element of package
library(zoo)

# For building the package
library(devtools)
library(roxygen2)

# Module calls 
source("beta_func.R")                     #d functions for finding betas and correlations between series and plotting them 
source("curves.R")                        #d functions for creating data from curves from other functions 
source("floor_valuation.R")               #d functions for floor validation (WIP)
source("implied_rates.R")                 #d functions for getting market implied rates / curves from bloomberg data 
source("inflation_db_functions.R")        #d function to access the inflation databases
source("inflation_swaps.R")               #d functions for calculating inflation swap math
source("linker_calculation.R")
source("linker_supply.R")                 #d linker supply and plots, superceded into the FIRVr package
source("seasonality_adjustment_cpi.R")    #d functions for seasonally adjusting inflation and projections

# test data 
source("old files/data_import.R")
