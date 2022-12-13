# 
# sampleExposurePortfolio <- readr::read_csv("instrument_type,maturity_date,notional,payment_frequency,coupon,index_ratio
#                                             ILB,2027-04-15,15000000,2,0.5,1.23
#                                             ILB,2025-05-14,25000000,2,0.25,1.1
#                                             ZC,2029-07-02,10000000,0,0,0.98
#                                             ASW,2023-11-01,20000000,2,1,1.15
#                                             ZC,2022-12-04,23000000,0,0,1.4", 
#                                            col_types = "cDdddd")


inflation_exposure_table <- function(exposure_portfolio){
  
  
  
  
  
}




inflation_Exposure_plot <-function(inflation_exposure_table){
  
  plotly::plot_ly(data = inflation_exposure_table)
  
}

