% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/implied_inflation_rates.R
\name{swap_implied_inflation_forwards_series}
\alias{swap_implied_inflation_forwards_series}
\title{Implied Forward Inflation Series (using swap rates)}
\usage{
swap_implied_inflation_forwards_series(
  inflation_index,
  lag_adjustment = TRUE,
  forward = "1y",
  tenor = "1y"
)
}
\arguments{
\item{inflation_index}{Object of class character, specifying the inflation index to use}

\item{lag_adjustment}{Object of class Boolean, whether to include apply lag adjustment (default is TRUE)}

\item{forward}{Object of character or numeric, the forward maturity, e.g. 1y}

\item{tenor}{Object of character or numeric, the tenor maturity, e.g. 1y}
}
\value{
data frame of forward rates across all available tenors
}
\description{
Returns inflation projected forward series from swap implied inflation data
}
\details{
Implied series is taken from zero-coupon swap curve.
}
\examples{
\dontrun{
  swap_implied_inflation_forwards_series("CPURNSA Index", forward = "1y", tenor = "1y")
}

}
