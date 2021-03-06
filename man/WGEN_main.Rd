% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WGEN_main.R
\name{WGEN_main}
\alias{WGEN_main}
\title{WGEN wrapper}
\usage{
WGEN_main(
  data,
  mean_mult = 1,
  sd_mult = 1,
  start_date = "1980-01-01",
  end_date = "2010-12-31"
)
}
\arguments{
\item{data}{data frame of original weather data. Requires at least the
following columns: "year",  "month", "date", "Tmax_C", "Tmin_C", "PPT_cm"}

\item{mean_mult}{scalar to multiply the mean PPT event size by
(for adjusting intensity)}

\item{sd_mult}{scalar to multiply the standard deviation of ppt event size by
(for adjusting intensity).}

\item{start_date}{Start date of weather simulation}

\item{end_date}{End date of weather simulation}
}
\value{
dataframe with simulated daily temperature and precipitation
}
\description{
WGEN wrapper
}
\examples{
# these are the minimum required columns
data <- wx_data[,  c("year",  "month", "date", "Tmax_C", "Tmin_C", "PPT_cm")]
mean_mult = 1
sd_mult = 1
start_date = "1980-01-01"
end_date = "1980-12-31"
WGEN_main(data = data, mean_mult = mean_mult, sd_mult = sd_mult,
          start_date = start_date, end_date = end_date)
}
