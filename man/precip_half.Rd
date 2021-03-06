% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_intensity.R
\name{precip_half}
\alias{precip_half}
\title{Number of days in which half of precip falls}
\usage{
precip_half(x, threshold = 0)
}
\arguments{
\item{x}{numeric vector of daily precip for one year}

\item{threshold}{discard values under a given threshold (good if worried about
sensor sensitivity of old data, etc)}
}
\value{
number of wettest days it took to get half of the annual precip
  (this is a metric of precip intensity) trying to do what is described by
  Pendergrass and Knutti 2018
}
\description{
Number of days in which half of precip falls
}
\examples{
precip_half(c(0, 0.1, 0.5, 0.5))
precip_half(x = c(1, 2, 1.5, 0.1, 0.1, 0.1), threshold = 0.1)
}
