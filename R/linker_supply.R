# 
# 
# library(DBI)
# library(FIRVr, lib.loc = "N:/Offdata/RM/_R code repository/RMPackages")
# library(futile.logger)
# library(lubridate)
# library(Rblpapi)
# library(RSQLite)
# library(tibble)
# library(tidyverse)

#' @title Linker Supply from Database
#'
#' @description Returns inflation-linked bond supply from inflation database for a given country 
#' 
#' @details returns time series of inflation-linked bond supply for a given country, including notional and PV01 statistics 
#'
#'
#' @param country Object of class character, specifying country, e.g. United States = "US"
#' 
#' @param infl_database_path Path to inflation database, default is rminfldbpath
#' 
#' @param lm_database_path Path to list management database
#' 
#'
#' @return data frame of inflation-linked bond supply for a given country
#'
#' @examples
#' \dontrun{
#'   infl_link_supply_ts_db(country = "US")
#' }
#' @importFrom magrittr %>%
#' @export

infl_link_supply_ts_db <- function(country,
                                   lm_database_path = rmlmdbpath,
                                   infl_database_path = rminfldbpath){
  
  if (!is.character(country)){
    futile.logger::flog.error('country not found or is in the wrong format')
  }
  
  if (!is.character(infl_database_path) | !file.exists(infl_database_path)){
    futile.logger::flog.error('inflation database path file path not found or is in the wrong format')
  }
  
  if (!is.character(lm_database_path) | !file.exists(lm_database_path)){
    futile.logger::flog.error('list membership database path file path not found or is in the wrong format')
  }
  
  issuer_country <- country
  if(country == "EU"){issuer_country = c("DE", "ES", "FR", "IT", "AT", "NE", "IE", "FI")}
  if(country == "UK"){issuer_country = "GB" 
    futile.logger::flog.info("ISO code for United Kingdom is GB. Code will run anyway.")
    }
  if(country == "WD"){issuer_country= c("DE", "ES", "FR", "IT", "AT", "NE", "IE", "FI", "GB", "JP", "US", "CA", "AU", "NZ")}
  
  lm_conn <- DBI::dbConnect(RSQLite::SQLite(), lm_database_path)
  
  linkers <- FIRVr::db_lm_get_membership(
      lm_conn,
      FIRVr::db_lm_get_lists(lm_conn) %>%
        dplyr::filter(stringr::str_detect(ListName, "Infl")) %>%
        dplyr::select(ListName) %>%
        dplyr::pull()
    )
  
  DBI::dbDisconnect(lm_conn)
  
  infl_conn <- DBI::dbConnect(RSQLite::SQLite(), infl_database_path)
  linkers_static <- DBI::dbGetQuery(infl_conn, "SELECT * FROM inflation_linked_static")
  DBI::dbDisconnect(infl_conn)
  
  bond_list <- linkers %>% 
    dplyr::left_join(linkers_static, 
                     by = "ISIN") %>% 
    dplyr::filter(country %in% issuer_country) %>%
    dplyr::mutate(ticker = stringr::str_c(ISIN, " Govt")) %>%
    dplyr::select(ticker, ISIN)
  

  get_supply_fields <- function(index, bond_list){
    

    issue_data <- FIRVr::bloomberg_query_static(securities = bond_list$ticker[index],
                                                fields = c("ISSUE_DT",
                                                           "AMT_ISSUED", 
                                                           "ISSUE_PX", 
                                                           "MATURITY"), 
                                                tidy_data = FALSE) %>%
      dplyr::rename(open_date = ISSUE_DT, 
                    notional = AMT_ISSUED,
                    price = ISSUE_PX) %>%
      dplyr::select(-Security) %>% 
      dplyr::filter(MATURITY > Sys.Date() - 1)
    
    if(nrow(issue_data) == 0){return(data.frame())}
    
    
    reopen_data <- FIRVr::bloomberg_query_dataset(security = bond_list$ticker[index],
                                                  field = "HISTORY_OF_REOP_TAPS_OF_THE_BD") 
    
    if(nrow(reopen_data > 0)){ 
      
      reopen_data <- reopen_data %>%
        dplyr::rename(open_date = `Effective (Selling) Date`, 
                      notional = `Increase Amount`,
                      price = `Increase Price`) %>%
        dplyr::select(open_date, notional, price) 
      
      issue_data$notional <- issue_data$notional - (reopen_data %>% dplyr::summarise(notional = sum(notional)) %>% dplyr::pull(notional))
      
    } else{
      return(data.frame())
    }
    
    issue_data <- reopen_data %>%
      dplyr::bind_rows(issue_data) %>%
      dplyr::mutate(ISIN = bond_list$ISIN[index]) %>%
      dplyr::left_join(linkers %>%
                         dplyr::select(ISIN, MaturityDate) %>%
                         dplyr::rename(maturity = MaturityDate) %>%
                         dplyr::left_join(linkers_static %>%
                                            dplyr::select(ISIN, country), 
                                          by = "ISIN"),
                       by = "ISIN") %>%
      dplyr::mutate(est_PV01 = as.numeric((maturity - open_date) / 365) * notional * price * 0.000001) %>%
      dplyr::select(ISIN, country, open_date, maturity, notional, est_PV01)
    
  }
  
  supply <- lapply(1:nrow(bond_list), get_supply_fields, bond_list = bond_list)
  
  supply <- supply %>%
    dplyr::bind_rows() %>% 
    dplyr::arrange(open_date)
  
  return(supply)
  
}

#' @title Plot Monthly Linker Supply from Database
#'
#' @description Returns plotly object of inflation-linked bond supply from inflation database for a given series
#' 
#' @details Returns a plot of inflation linked bond supply
#'
#'
#' @param infl_link_supply_ts Object of class data.frame, consisting of inflation linked bond supply statistics 
#' 
#' @param field field to plot, e.g. notional 
#' 
#' @param start_date start date for the series, e.g "2018-01-01"
#'
#' @return plot of inflation-linked bond supply 
#'
#' @examples
#' \dontrun{
#'   infl_link_supply_ts <- InflationTools::infl_link_supply_ts_db("US")
#'   plot_monthly_supply(infl_link_supply_ts)
#' }
#'
#' @export


plot_monthly_supply <- function(infl_link_supply_ts,
                                field = "notional",
                                start_date = "2018-01-01") {
  
  if (!all(c("open_date", field) %in% colnames(infl_link_supply_ts))) {
    futile.logger::flog.error(paste0("inflation supply data must have colnames: index_value ", field))
  }
  
  if (is.data.frame(infl_link_supply_ts)) {
    futile.logger::flog.error("inflation supply data must be a data frame")
  }
  
  open_data <- infl_link_supply_ts %>%
    dplyr::filter(open_date >= start_date) %>%
    dplyr::mutate(year = lubridate::year(open_date),
                  month = lubridate::month(open_date)) %>%
    dplyr::select(month, year, field) %>%
    dplyr::rename(value = field) %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(value =  sum(value))
  
  supply_plot <- plotly::plot_ly()
  
  for (i in unlist(dplyr::distinct(open_data, year))) {
    supply_plot <- supply_plot %>%
      plotly::add_trace(
        data = open_data %>% dplyr::filter(year == i),
        x = ~ month,
        y = ~ value,
        name = i,
        type = "bar"
      )
  }
  
  supply_plot <- supply_plot %>%
    plotly::layout(
      xaxis = list(
        title = "Month",
        tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        ticktext = list(
          "Jan",
          "Feb",
          "Mar",
          "Apr",
          "May",
          "Jun",
          "Jul",
          "Aug",
          "Sep",
          "Oct",
          "Nov",
          "Dec"
        )
      ),
      yaxis = list(title = field),
      barmode = "group"
    )
  
  return(supply_plot)
  
}

#' @title Plot linker Supply by Maturity from Database
#'
#' @description Returns plotly object of inflation-linked bond supply from inflation database for a given series
#' 
#' @details returns plot of inflation linked bond supply 
#'
#'
#' @param infl_link_supply_ts Object of class data.frame, consisting of inflation linked bond supply statistics 
#' 
#' @param start_date start date for the series, e.g "2018-01-01"
#'
#' @return plot of inflation-linked bond supply by maturity 
#'
#' @examples
#' \dontrun{
#'   infl_link_supply_ts <- InflationTools::infl_link_supply_ts_db("US")
#'   plot_supply_maturity(infl_link_supply_ts)
#' }
#'
#' @export

plot_supply_maturity <- function(infl_link_supply_ts,
                                 start_date = "2017-01-01") {
  
  if (!all(c("open_date") %in% colnames(infl_link_supply_ts))) {
    futile.logger::flog.error(paste0("inflation supply data must have colnames: index_value, maturity "))
  }
  
  if (is.data.frame(infl_link_supply_ts)) {
    futile.logger::flog.error("inflation supply data must be a data frame")
  }
  
  maturity_data <- infl_link_supply_ts %>%
    dplyr::mutate(issuance_mat = (maturity - open_date) / 365,
                  year = lubridate::year(open_date)) %>%
    dplyr::filter(open_date >= start_date) %>%
    dplyr::mutate(
      issuance_bucket = dplyr::case_when(
        issuance_mat < 6 ~ "<6 years",
        issuance_mat < 11 ~ "6 to 11 years",
        issuance_mat < 21 ~ "11 to 21 years",
        issuance_mat < 31 ~ "21 to 31 years",
        TRUE ~ "31+ years"
      )
    ) %>%
    dplyr::group_by(year, issuance_bucket) %>%
    dplyr::summarise(amount_issued = sum(notional)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = dplyr::if_else(
      year == max(year),
      stringr::str_c(year, " YTD"),
      as.character(year)
    ))
  
  supply_plot <- plotly::plot_ly()
  
  for (i in unlist(dplyr::distinct(maturity_data, year))) {
    supply_plot <- supply_plot %>%
      plotly::add_trace(
        data = maturity_data %>% dplyr::filter(year == i),
        x = ~issuance_bucket,
        y = ~amount_issued,
        name = i,
        type = "bar"
      )
  }
  
  supply_plot <- supply_plot %>%
    plotly::layout(
      xaxis = list(
        title = "Issuance Maturity Bucket",
        categoryorder = "array",
        categoryarray = list(
          "<6 years",
          "6 to 11 years",
          "11 to 21 years",
          "21 to 31 years",
          "31+ years"
        )
      ),
      yaxis = list(title = "Amount Issued"),
      barmode = "group"
    )
  
  return(supply_plot)
  
}



#' @title Get TIPS Fed SOMA holdings data
#'
#' @description Retrieves data frame of Fed SOMA holdings of TIPS securities, from as early as 2003
#' 
#' @details returns data frame of Fed holdings
#'
#'
#' @param path Object of class character, the location of the data 
#' 
#' @param start_date start date for the series, e.g "2018-01-01"
#'
#' @return data frame of Fed holdings
#'
#' @examples
#' \dontrun{
#'   InflationTools::infl_link_fed_purchases(start_date = "2010-01-01")
#' }
#'
#' @export

infl_linked_fed_purchases <- function(path = "https://markets.newyorkfed.org/api/soma/tsy/get/monthly.csv", start_date = Sys.Date() - 10*365){
  
  if (!lubridate::is.Date(anytime::anydate(start_date))) {
    futile.logger::flog.error(paste0("start date must be a date object, e.g. '2010-01-01'"))
  }
  
  if (!is.character(path)) {
    futile.logger::flog.error("Supplied path must be a character string")
  }
  
  data <- utils::read.csv(url(paste0(path)), check.names = F) 
  
  if (nrow(data) > 0) {
    
    data_clean <- data %>%
      dplyr::mutate(
        Maturity = as.numeric(difftime(`Maturity Date`, Sys.Date(),  units = "weeks")) / 52,
        month_year = lubridate::floor_date(as.Date(`As Of Date`), unit = "months"),
        OutstandingAmt = `Par Value` / `Percent Outstanding`
      ) %>%
      dplyr::filter(Maturity > 0,
                    `Security Type` == "TIPS",
                    as.Date(`As Of Date`) >= as.Date(start_date)) %>%
      dplyr::mutate(
        Bucket = dplyr::case_when(
          Maturity <= 2   ~ "0-2 years",
          Maturity > 2 &
            Maturity <= 7   ~ "2-7 years",
          Maturity > 7 &
            Maturity <= 11   ~ "7-11 years",
          Maturity > 11 &
            Maturity <= 30   ~ ">11 years"
        )
      ) %>%
      dplyr::select(
        "As Of Date",
        "CUSIP",
        "Security Type" ,
        "Maturity Date",
        "Coupon (%)" ,
        "Par Value",
        "Percent Outstanding",
        "OutstandingAmt",
        "Change From Prior Week",
        "Change From Prior Year",
        "Maturity",
        "month_year",
        "Bucket"
      )
    
  } else {
    data_clean <- data.frame()
  }
  
  
  return(data_clean)
  
}


