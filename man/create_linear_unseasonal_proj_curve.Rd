% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seasonality_adjustment_cpi.R
\name{create_linear_unseasonal_proj_curve}
\alias{create_linear_unseasonal_proj_curve}
\title{Create projected inflation curve}
\usage{
create_linear_unseasonal_proj_curve(
  annualised_inflation_percent,
  base_level,
  base_month = lubridate::month(Sys.Date()),
  base_year = lubridate::year(Sys.Date()),
  frequency = 12,
  proj_years = 50
)
}
\arguments{
\item{annualised_inflation_percent}{annualised projected inflation rate}

\item{base_level}{Base level of inflation index}

\item{base_month}{Base month of inflation series, expects month in number format, e.g. for March: base_month = 3}

\item{base_year}{Base year of inflation series, expects year in format 'YYYY'}

\item{frequency}{frequency of the data, e.g. for monthly, frequency = 12}

\item{proj_years}{Number of years to project the series}
}
\value{
projected inflation curve
}
\description{
Generate a unseasonal inflation curve, for a given inflation rate and base level - useful for pricing inflation instruments using inflation on a forward looking basis
}
\details{
unseasonal inflation curve takes an inflation rate, base level, frequency
}
\examples{

create_linear_unseasonal_proj_curve(
annualised_inflation_percent = 2,
base_level = 150,
base_month = lubridate::month(Sys.Date()),
base_year = lubridate::year(Sys.Date()),
frequency = 12,
proj_years = 3
)


}
