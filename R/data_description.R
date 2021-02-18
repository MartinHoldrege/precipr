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
