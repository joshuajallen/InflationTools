
#' @title ILB portfolio cashflows
#'
#' @description Returns a data table of future cashflows for a portfolio of ISINs. Can be either inflation adjusted
#'  or unadjusted. Assumes 1mn notional (face value), so that adjustments for holding size will have to be done 
#'  after calling the function.  
#'  
#'  Built on the back of \link[InflationTools]{cashflow_table} and  \link[InflationTools]{linker_cashflow_table} 
#'  functions.
#'
#' @param bond_ISINs character, string or vector of strings of inflation linked bond ISINs e.g. US912810SV17
#' @param ref_date date, calculation date for cashflows, only future cashflows shown
#' @param inflation_adj logical, whether the cashflow should include the inflaton adjusted cashflows
#' @param seasonally_adjust logical, seasonal adjustment
#' @param sa_method method for seasonal adjustment, defaults to 'linear'
#' @param lm_database_path Path class character to list management database
#' @param infl_database_path Path class character to inflation database
#' 
#' @return data frame of cashflows for the bonds
#'
#' @examples
#' \dontrun{  
#'   cashflow_table_ilb_portfolio(bond_ISINs = c("US91282CCM10", "US91282CDC29"))
#' }
#' 
#' @export
#' 
cashflow_table_ilb_portfolio <- function(bond_ISINs,
                                         ref_date = Sys.Date(), 
                                         inflation_adj = TRUE,
                                         seasonally_adjust = FALSE,
                                         sa_method = "linear", 
                                         lm_database_path = rmlmdbpath,
                                         infl_database_path = rminfldbpath){
  
  # ensure ISIN list is distinct
  bond_ISINs <- unique(bond_ISINs)
  
  # Get the static data for each ISIN
  static <- InflationTools::linker_static_db(bond_ISIN = bond_ISINs,
                                             lm_database_path = lm_database_path,
                                             infl_database_path = infl_database_path)
  
  # Generate a cashflow table for all ISINs  
  cashflow_table <- t(mapply(
           InflationTools::cashflow_table,
           static$MaturityDate,
           static$coupon,
           static$payment_frequency,
           ref_date = ref_date,
           static$settle_convention,
           static$day_count
        )) %>% 
    tibble::as_tibble() %>% 
    dplyr::bind_cols(ISIN = bond_ISINs) %>%
    tidyr::unnest(cols = dplyr::everything()) %>%
    dplyr::left_join(static %>%
                       dplyr::select(ISIN, reference_index), 
                     by = "ISIN")
  
  # Will have to loop to deal with different instruments having different inflation curves    
  if(inflation_adj == TRUE){
    adj_table <- tibble::tibble()
    for(i in 1:length(unique(static$reference_index))){
      
      # Filter the static to match the inflation index 
      static_fltrd <- static %>%
        dplyr::filter(reference_index == unique(static$reference_index)[[i]])
      
      # Get the inflation curve
      proj_curve <- InflationTools::implied_proj_inflation_curve_db(inflation_index = unique(static$reference_index)[[i]],
                                                                    seasonally_adjust = seasonally_adjust,
                                                                    sa_method = "linear",
                                                                    from_date = ref_date - 370,
                                                                    infl_database_path = infl_database_path)
      
      for(j in 1:length(unique(static_fltrd$ISIN))){
        
        static_fltrd_2 <- static_fltrd %>%
          dplyr::filter(ISIN == unique(static_fltrd$ISIN)[[j]])
        
        cf_fltrd <- cashflow_table%>%
          dplyr::filter(ISIN == unique(static_fltrd$ISIN)[[j]])
          
        temp_cf_tbl <- InflationTools::linker_cashflow_table(cashflow_table = cf_fltrd,
                                                             projected_inflation_curve = proj_curve,
                                                             reference_cpi = static_fltrd_2$reference_cpi,
                                                             ref_date = ref_date,
                                                             settle_convention = static_fltrd_2$settle_convention,
                                                             inflation_lag = static_fltrd_2$inflation_lag,
                                                             principal_floor = static_fltrd_2$principal_floor,
                                                             coupon_floor = static_fltrd_2$coupon_floor)
        
        adj_table <- dplyr::bind_rows(adj_table, 
                                      temp_cf_tbl)
        
      }
    }
    cashflow_table <- adj_table
  }    
      
  cashflow_table
  
} 




