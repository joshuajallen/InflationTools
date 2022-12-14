% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/implied_inflation_rates.R
\name{swap_implied_inflation_forwards}
\alias{swap_implied_inflation_forwards}
\title{Implied Forward Inflation Curve (using swap rates)}
\usage{
swap_implied_inflation_forwards(inflation_index, lag_adjustment = TRUE)
}
\arguments{
\item{inflation_index}{Object of class character, specifying the inflation index to use}

\item{lag_adjustment}{Object of class Boolean, whether to include apply lag adjustment (default is TRUE)}
}
\value{
data frame of forward rates across all available tenors
}
\description{
Returns inflation projected forward curve from swap implied inflation curve
}
\details{
Implied curve is taken from zero-coupon swap curve.
}
\examples{
\dontrun{
  swap_implied_inflation_forwards("CPURNSA Index")
}

}
