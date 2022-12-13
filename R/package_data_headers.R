#' InflationTools package
#' 
#' Fixed income inflation tools and analysis
#' 
#' @docType package
#' @name InflationTools
#' 
"_PACKAGE"


# Column headers or variables that due to tidyverse the package builder may think are functions
utils::globalVariables(c("Value", "x1", "Date", "(Intercept)", "ISIN", "index", "lag", "maturity", "ticker_len", "ticker",
                         "tickery", "Security", "index_value", "proj_infl", "YfB", "infl_rate", "YfT", "inflation_index",
                         "SA", "season_factor_cuml", "month", "year", "x", "AMT_ISSUED", "Price", "ListName", "MaturityDate",
                         "issuance_bucket", "notional", "open_date", "price", "value", "est_PV01", "Increase Amount",
                         "Effective (Selling) Date", "Increase Price", "ISSUE_DT", "ISSUE_PX", "adjusted_interest", 
                         "adjusted_principal", "cashflow_date", "cashflow_period_days", "days_from_start", 
                         "inflation_ref_index", "period_denominator", "period_numerator", "sa_factor", "unadjusted_interest",
                         "unadjusted_principal", "adj_cashflow_value", "adjusted_cashflow", "discount_period_fraction",
                         "real_cashflow_value", "unadjusted_cashflow", "macdur", "mac_dur_col", "convx", "conv_col",
                         "rown", "prev-periods", "curve_implied_inflation", "prev_periods", "rminfldbpath",
                         "aes", "coupon_pro", "element_blank", "issuer", "real_yield_nsa", "real_yield_sa", "reference_index", 
                         "rmlmdbpath", "ticker_stub", "ccy", "coupon", "curve_date", "index_name", "proj_index", "ref_date",
                         "term", "tenor_ticker", "forward", "forward_rate", "forward_ticker", "fwd_rate", "tenor_rate", 
                         "measure", "Field", "px_last", "px_bid", "BID_ASK", "quos", "px_ask", "n", "Swap", "Breakeven", 
                         "Maturity", "Security Type", "Percent Outstanding", "Par Value", "As Of Date", "Maturity Date",
                         "MATURITY", "IOTA", "openlink_tickers", "SecurityDes", "inflation_ISINs", "nominal_ISINs", "InfSec", 
                         "NomSec", "NomMat", "InfMat", "Value", "ValueDate", "InfYld", "AgeDiff", "InfAge", "InfIss",  "NomAge",
                         "NomIss", "NomYld", "breakeven", "InfASW", "NomASW", "input", "iota"))



#' Historic inflation index data 
#'
#' A dataset of historic monthly G7 inflation index readings 
#' 
#' @format A tibble of 2653 rows and 3 columns
#' \describe{
#'  \item{inflation_index}{character, the bloomberg code for the inflation index}
#'  \item{index_value}{numeric, the inflation index reading on that date}
#'  \item{date}{date, the date of the inflation reading}
#' }
#' 
#' @source bloomberg
"past_inflation"


#' Linker info from RM list management database
#'
#' A dataset of inflation linked bonds their ISINs, maturities, reference indexes and lags. 
#' 
#' @format A data frame of 135 rows and 8 columns
#' \describe{
#'  \item{SecurityDes}{character, the name of the linker}
#'  \item{ISIN}{character, the ISIN of the linker plus the bloomberg suffix eg Govt, Corp}
#'  \item{ListName}{character, the list in the list management database the linker is in}
#'  \item{REFERENCE_INDEX}{character, the bloomberg code for the inflation index the linker references}
#'  \item{INFLATION_LAG}{numeric, the lag of the linker accrual to the reference index}
#'  \item{StartDate}{date, the date the linker was added to the list management database}
#'  \item{EndDate}{date, the date the linker will leave the list management database}
#'  \item{MaturityDate}{date, the maturity date of the linker}
#' }
#' 
#' @source RM list_management database
"linker_lm_info"

#' Projected US inflation curve (July 2021)
#'
#' A datatable with the realised and projected US CPURNSA inflation curve from Jan 2019 until Mar 2051   
#' 
#' @format A data frame of 387 rows and 3 columns
#' \describe{
#'  \item{date}{date, the date of the index value}
#'  \item{proj_infl}{numeric, the projected (or realised for historic) inflation index value on the date}
#'  \item{season_factor_cuml}{numeric, the multiplicative seasonal adjustment applied to the index value}
#' }
#' 
#' @source Bloomberg and package calculations 
"exampleUSInflationCurve"


#' Path of RM inflation database
#'
#' The specific filepath of the current Reserve Managers inflation database 
#' 
#' @format character string, a file path
#' 
#' @source SH
"rminfldbpath"


#' Path of RM list management database
#'
#' The specific filepath of the current Reserve Managers list management database 
#' 
#' @format character string, a file path
#' 
#' @source SH
"rmlmdbpath"


#' Fake portfolio of trades
#'
#' A datatable of fake trades and trade dates for use with the exposure functions.    
#' 
#' @format A data frame of 5 rows and 6 columns
#' \describe{
#'  \item{instrument_type}{character,  whether the instrument is a inflation linked bond, zero coupon swap or asset swap.}
#'  \item{maturity_date}{date, the maturity date of the instrument}
#'  \item{notional}{numeric, the notional of the instrument}
#'  \item{payment_frequency}{numeric, payment frequency of the instrument, eg 2 is semi-annual}
#'  \item{coupon}{numeric, coupon (annualised) of the instrument}
#'  \item{index_ratio}{numeric, the index ratio of the instrument on the day of calculation}
#' }
#' 
#' @source SH 
"exampleExposurePortfolio"


#' Path of RM list management database
#'
#' The securities config file for the inflation/liquidity risk premia modelling 
#' 
#' @format data frame 
#' 
#' @source JA
#' 
"securities_inflation_model"

