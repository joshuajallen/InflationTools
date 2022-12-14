% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ILB_carry.R
\name{linker_carry_func}
\alias{linker_carry_func}
\alias{linker_carry_yield}
\alias{linker_carry_bloomberg}
\title{Linker carry calculations}
\usage{
linker_carry_yield(
  real_yield,
  carry_days = 30,
  projected_inflation_curve,
  reference_cpi,
  inflation_lag,
  principal_floor,
  coupon_floor,
  maturity_date,
  coupon,
  payment_frequency,
  ref_date = Sys.Date(),
  settle_convention = 1,
  day_count = "ACT/ACT"
)

linker_carry_bloomberg(
  security,
  projected_inflation_curve,
  carry_days,
  principal_floor = 100,
  cpn_floor = 0
)
}
\arguments{
\item{real_yield}{numeric, real yield of bond at reference date in percent}

\item{carry_days}{numeric, the number of days over which to calculate the carry}

\item{projected_inflation_curve}{data.table, curve of projected inflation with columns \code{date} and \code{proj_infl}
, eg can be generated by \link[InflationTools]{implied_proj_inflation_curve}}

\item{reference_cpi}{numeric, the CPI index of the bond at issue, sometimes also called "base CPI"}

\item{inflation_lag}{numeric, the lag in months that instrument has to the inflation index}

\item{principal_floor}{numeric, the percentage floor of the principal, eg 100 is floored at par}

\item{coupon_floor}{numeric, the percentage floor of the coupon, eg 0 is not floored}

\item{maturity_date}{date, the date of the maturity ie final cashflow}

\item{coupon}{numeric, the coupon in percent to be paid at regular intervals}

\item{payment_frequency}{numeric, the number of times per year the coupon is to be paid}

\item{ref_date}{date, trade or calculation date, regular settle is assumed in \code{_db} functions}

\item{settle_convention}{numeric, the days until the instrument will be settled}

\item{day_count}{character, the day count convention of the instrument. Can currently only be \code{"ACT/ACT"}.}

\item{security}{character, can be a bond ISIN or security name in bloomberg format e.g. \code{"TII 0.375 07/15/27"}}

\item{bond_ISIN}{character, specifying bond ISIN of an inflation linked bond, e.g. \code{"US912810SV17"}, must be in the database}

\item{real_price}{numeric, clean price of bond at reference date}

\item{lm_database_path}{Path class character to list management database}

\item{infl_database_path}{Path class character to inflation database}
}
\value{
list of carry statistics. \code{carry} is the actual carry for the period, while \code{carry_dv01} is
the carry per unit of DV01  (inflation).
}
\description{
Returns the carry of an inflation-linked bond given either a price or yield, for 1mn local currency nominal. Using either
an ISIN with the database (\code{_db}), an ISIN/security name (\code{_bloomberg}), or with the full set of information about the bond.
One of either a real yield or price is required in order to work out the values.
}
\examples{

linker_carry_yield(real_yield = -2.56,
       carry_days = 30,
       projected_inflation_curve = exampleUSInflationCurve,
       reference_cpi = 269.59839,
       inflation_lag = 3,
       principal_floor = 100,
       coupon_floor = 0,
       maturity_date = as.Date("2023-01-15"),
       coupon = 0.125,
       payment_frequency = 2
       )
\dontrun{ 
   linker_carry_bloomberg(security = "US912810SV17",
         carry_days = 30,
         projected_inflation_curve = exampleUSInflationCurve
   )  
}

}
