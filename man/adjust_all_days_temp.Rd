% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WGEN_temp_modifiers.R
\name{adjust_all_days_temp}
\alias{adjust_all_days_temp}
\title{Adjust temp for change in number of wet days}
\usage{
adjust_all_days_temp(Tdry, Twet, n_all, n_wet, mean_mult = 1)
}
\arguments{
\item{Tdry}{mean temp on dry days}

\item{Twet}{mean temp on wet days}

\item{n_all}{total number of days}

\item{n_wet}{total number of dry days}

\item{mean_mult}{scalar that mean precip event is being multiplied by}
}
\value{
A new fake mean temperature on all days (will be higher if mean_mult > 1)
This should be used as input to temp_modifer() functions, so that when there
are fewer ppt events, dry days are adjusted up less, and wet days are
adjusted down more
}
\description{
A new (fake) mean temp on 'all' days needs to be calculated.
Because the actual proportion of wet days decreases, this keeps the mean temp
adjusted for wet days and dry days same as before. Otherwise decreasing the number
of wet days would just elevate mean temperature.
}
\examples{
# Tdry = 11; Twet = 9; n_all = 100; n_wet = 30
adjust_all_days_temp(11, 9, 100, 30) # original mean
adjust_all_days_temp(11, 9, 100, 30, mean_mult = 2) # adjust mean
}
