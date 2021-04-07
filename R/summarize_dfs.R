# calculate map -----------------------------------------------------------


#' calculate mean annual precipitation
#'
#' @param wout either a dataframe of rSOILWAT2 weather list
#'
#' @return single value (MAP)
#' @export
calc_map <- function(wout) {
  # calculate mean annual ppt

  # allowing dataframe or weatherdata list
  df <- if (is.data.frame(wout)) {
    wout
  } else {
    wout %>%
      rSOILWAT2::dbW_weatherData_to_dataframe() %>%
      as.data.frame()
  }
  stopifnot(c("Year", "PPT_cm") %in% names(df))

  out <- df %>%
    group_by(.data$Year) %>%
    summarise(PPT_cm = sum(.data$PPT_cm), .groups = "drop") %>%
    pull(PPT_cm) %>%
    mean()
  out
}



# expected n wet days -----------------------------------------------------

#' @title calculate average number of wet days per year
#'
#' @param wout either a dataframe of rSOILWAT2 weather list
#'
#' @return
#' @export
calc_nwet <- function(wout) {
  # calculate mean annual ppt

  # allowing dataframe or weatherdata list
  df <- if (is.data.frame(wout)) {
    wout
  } else {
    wout %>%
      rSOILWAT2::dbW_weatherData_to_dataframe() %>%
      as.data.frame()
  }
  stopifnot(c("Year", "PPT_cm") %in% names(df))

  out <- df %>%
    group_by(.data$Year) %>%
    summarise(nwet = sum(.data$PPT_cm > 0), .groups = "drop") %>%
    pull(.data$nwet) %>%
    mean()
  out
}
