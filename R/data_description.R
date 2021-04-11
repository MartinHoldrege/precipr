# data_description

#' Example dataset of daily temperature and precipitation
#'
#' A dataset for daily weather (from daymet 1 km product). This is the daily
#' weather for site 5 in Palmquist et al in press (2021).
#'
#' @format A data frame with 11223 rows and 9 variables:
#' \describe{
#'   \item{site}{site number, just one site in this dataset}
#'   \item{year}{year of measurement}
#'   \item{month}{month of measurement}
#'   \item{day}{day of month}
#'   \item{date}{date, yyyy-mm-dd}
#'   \item{DOY}{day of year}
#'   \item{Tmax_C}{daily max temp, C}
#'   \item{Tmin_C}{daily min temp, C}
#'   \item{PPT_cm}{daily precipitation, cm}
#' }
"wx_data"


#' @title Example dataset of daily temperature and precipitation
#'
#' @description A dataset for daily weather (from daymet 1 km product).
#' This is the daily
#' weather for site 119 in Palmquist et al in press (2021). I'm including this
#' dataset because it is useful for testing, because adjusting markov
#' coefficients generated from this dataset exposed errors that other datasets
#' didn't.
#'
#' @format A data frame with 11323 rows and 6 variables:
#' \describe{
#'   \item{Year}{year of measurement}
#'   \item{DOY}{day of year}
#'   \item{Tmax_C}{daily max temp, C}
#'   \item{Tmin_C}{daily min temp, C}
#'   \item{PPT_cm}{daily precipitation, cm}
#' }
"wdata119"
