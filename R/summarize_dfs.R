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

#' @title calculate mean temperature
#'
#' @param wout either a dataframe of rSOILWAT2 weather list
#' @param col string, name of the column to take an average of
#'
#' @return single value (MAT)
#' @export
#'
#' @examples
#' calc_mat(rSOILWAT2::weatherData, col = "Tmax_C")
calc_mat <- function(wout, col) {
  # calculate mean temp

  # allowing dataframe or weatherdata list
  df <- if (is.data.frame(wout)) {
    wout
  } else {
    wout %>%
      rSOILWAT2::dbW_weatherData_to_dataframe() %>%
      as.data.frame()
  }
  stopifnot(c("Year", col) %in% names(df))

  out <- df %>%
    group_by(.data$Year) %>%
    summarise(PPT_cm = mean(.data[[col]]), .groups = "drop") %>%
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
