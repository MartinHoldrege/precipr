% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_description.R
\docType{data}
\name{wdata119}
\alias{wdata119}
\title{Example dataset of daily temperature and precipitation}
\format{
A data frame with 11323 rows and 6 variables:
\describe{
  \item{Year}{year of measurement}
  \item{DOY}{day of year}
  \item{Tmax_C}{daily max temp, C}
  \item{Tmin_C}{daily min temp, C}
  \item{PPT_cm}{daily precipitation, cm}
}
}
\usage{
wdata119
}
\description{
A dataset for daily weather (from daymet 1 km product).
This is the daily
weather for site 119 in Palmquist et al in press (2021). I'm including this
dataset because it is useful for testing, because adjusting markov
coefficients generated from this dataset exposed errors that other datasets
didn't.
}
\keyword{datasets}
