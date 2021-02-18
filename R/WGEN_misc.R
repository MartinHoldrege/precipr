# misc functions ----------------------------------------------------------

week52 <- function(date) {
  # make last week of year 'long' so don't have short partial week
  # that can't estimate parameters for
  week <- lubridate::week(date)
  week <- ifelse(week ==53, 52, week)
  week
}
