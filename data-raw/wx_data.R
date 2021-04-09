## code to prepare `DATASET`

library(dplyr)
library(rSOILWAT2)

# site 5 ------------------------------------------------------------------


# data for site-5 (ambient intensity), from the weather database provided by KP

# It includes the columns used by STEPWAT (plus sit and date)

wx_data <- readr::read_csv("inst/extdata/wx_ambient_site-5.csv")


# site 119 ----------------------------------------------------------------

# also now adding example dataset for site 119 because adjusting coeffs
# for this site proved more difficult, so this is a good test data set

# connect to db ---------------------------------------------------------

db_path <- "../dbWeather/dbWeatherData_STEPWAT2_200sites.sqlite3"

rSOILWAT2::dbW_setConnection(db_path, check_version = TRUE)
wdata <- dbW_getWeatherData(Site_id = 119)
wdata119 <- wdata %>%
  dbW_weatherData_to_dataframe() %>%
  as.data.frame()
rSOILWAT2::dbW_disconnectConnection()
# saving data -------------------------------------------------------------


usethis::use_data(wx_data, overwrite = TRUE)
usethis::use_data(wdata119, overwrite = TRUE)
