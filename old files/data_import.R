# Libraries 

library(DBI)
library(FIRVr, lib.loc = "N:/Offdata/RM/_R code repository/RMPackages")
library(lubridate)
library(Rblpapi)
library(RSQLite)
library(tibble)
library(tidyverse)


# Connect to the list manager database and download the curve(s) 
LM_DATABASE <- "N:/Offdata/RM/_Data/ListManagement/list_membership.db"
INFLATION_DATABASE <- "N:/Offdata/RM/_Data/Inflation/inflation.db"

lm_conn <- dbConnect(SQLite(), LM_DATABASE)
linkers <- db_lm_get_membership(lm_conn, db_lm_get_lists(lm_conn) %>%
                                  filter(str_detect(ListName, "Infl")) %>%
                                  select(ListName) %>%
                                  pull()
                                ) %>%
  mutate(ISIN = str_c(ISIN, " Corp"))
dbDisconnect(lm_conn)

linkers <- linkers %>%
  left_join(bloomberg_query_static(linkers %>% 
                                     select(ISIN) %>% 
                                     pull(ISIN),
                                   c("INFLATION_LAG",  
                                     "REFERENCE_INDEX"), 
                                   tidy_data = FALSE
                                   ),
            by = c("ISIN" = "Security")) 

infl_conn <- dbConnect(SQLite(), INFLATION_DATABASE)
inflation_past <- dbGetQuery(infl_conn, "SELECT * FROM inflation_indices") %>%
  mutate(date = as_date(date))
dbDisconnect(infl_conn)  


         