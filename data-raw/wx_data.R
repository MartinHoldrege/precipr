## code to prepare `DATASET`

# data for site-5 (ambient intensity), from the weather database provided by KP

# It includes the columns used by STEPWAT (plus sit and date)

wx_data <- readr::read_csv("inst/extdata/wx_ambient_site-5.csv")

usethis::use_data(wx_data, overwrite = TRUE)
