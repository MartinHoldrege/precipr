# functions to describe precipitation intensity using a couple metrics


# wet_lengths -------------------------------------------------------------


#' Lengths of rain events
#'
#' Utility function used in subsequent functions
#'
#' @param x numeric vector
#'
#' @return vector containing the lengths (number of consecutive days) of
#' rain events
#'
#' @export
#' @examples
#' wet_lengths(c(0, 1, 1, 0, 1))
#' wet_lengths(c(0, 0))
wet_lengths <- function(x) {
  stopifnot(is.numeric(x))

  # if no ppt, then return 0 (ie wet length is 0)
  if (all(x %in% 0)) {
    return(0)
  }

  # consider changing this threshold if necessary
  is_wet <- x > 0

  # length of wet and dry runs
  runs <- rle(is_wet)

  # lengths of consecutive runs of TRUE (ie wet day)
  out <- runs$lengths[runs$values]
  out
}


# n_events ----------------------------------------------------------------

#' Number of rain events
#'
#' @param x numeric vector
#' @param min_length a positive integer. The minimum number of days an event
#' has to be in order to count it.
#'
#' @return The number of rain events in x. Rain events are defined
#' as a sequence with 1 (default) or more consecutive days with rain
#' @export
#'
#' @examples
#' n_events(rep(c(0, 1), 5)) == 5
#' n_events(rep(c(0, 1), each = 5)) == 1
#' n_events(c(1, 1, 0, 0.5, 0, .1, .2, 0), min_length = 2) == 2
n_events <- function(x, min_length = 1) {
  stopifnot(min_length >= 1,
            is.numeric(x))

  wet_length <- wet_lengths(x)

  # number of wet days sequences with at least min_length days long
  n <- sum(wet_length >= min_length)
  n
}


# max_event_length --------------------------------------------------------

#' Max rain event length
#'
#' @param x numeric vector of daily rain events
#'
#' @return the length of the longest sequence of consecutive rainy days
#' @export
#'
#' @examples
#' max_event_length(c(0, 1, 1, 2, 0, 1)) == 3
max_event_length <- function(x) {
  max(wet_lengths(x))
}


# create_event_df ---------------------------------------------------------

#' Create dataframe with event numbers
#'
#' @param x numeric vector (daily precip)
#'
#' @return tibble with 2 columns, x (input vector), and event which is the
#' event number corresponding to that value of x
#'
#' @export
#' @examples
#' create_event_df(c(0.1, 0.1, 0, 0.2))
create_event_df <- function(x) {
  stopifnot(is.numeric(x))

  # for now this fun doesn't work with NAs, so considered 0 for now
  x_old <- x
  x[is.na(x)] <- 0
  # consider changing this threshold if necessary
  is_wet <- x > 0

  # length of wet and dry runs
  runs <- rle(is_wet)

  num_events <- sum(runs$values) # how many "TRUE"s
  # lengths of consecutive runs of TRUE (ie wet day)

  # this if statement not really needed
  event_seq <- if(num_events >= 1) {
    1:num_events
  } else {
    0
  }

  # replace logicals with event numbers
  runs2 <- runs
  runs2$values[runs2$values] <- event_seq

  x_events <- inverse.rle(runs2) # sequence w/ length x, giving event numbers

  df <- tibble::tibble(x = x, event = x_events)
  df$x[is.na(x_old)] <- NA_real_ # add back original NAs
  df
}

# mean_event_size ---------------------------------------------------------

#' Calculate mean event size
#'
#' @param x numeric vector (daily precip)
#'
#' @return mean event size, where an event is the amount of rain on one or more
#' consecutive days of rain
#' @export
#'
#' @examples
#' mean_event_size(c(0.1, 0.1, 0, 0.2))
#' mean_event_size(0)
#' mean_event_size(rep(0.1, 10))
mean_event_size <- function(x) {

  df <- create_event_df(x)

  if (all(df$x == 0)) return(0)

  # mean size of events
  mean_size <- df %>%
    filter(.data$x > 0) %>%  # just days of rain
    group_by(.data$event) %>%
    # summing across days of an event
    summarise(event_size = sum(.data$x), .groups = "drop") %>%
    pull(event_size) %>%
    mean()

  mean_size
}


# half precip -------------------------------------------------------------



#' Number of days in which half of precip falls
#'
#' @param x numeric vector of daily precip for one year
#' @param threshold discard values under a given threshold (good if worried about
#'  sensor sensitivity of old data, etc)
#'
#' @return  number of wettest days it took to get half of the annual precip
#'   (this is a metric of precip intensity) trying to do what is described by
#'   Pendergrass and Knutti 2018
#' @export
#'
#' @examples
#' precip_half(c(0, 0.1, 0.5, 0.5))
#' precip_half(x = c(1, 2, 1.5, 0.1, 0.1, 0.1), threshold = 0.1)
precip_half <- function(x, threshold = 0){

  if(sum(!is.na(x)) == 0) return(NA) # if all NA

  prop_NA <- sum(is.na(x))/length(x) # proportion missing

  if(prop_NA > 0.3) warning("greater than 30% missing values")

  # if vector has no non-0 values
  if(all(x %in% c(0, NA_real_))) return(0)

  x <- x[x >= threshold]

  total <- sum(x, na.rm = TRUE) # annual precip
  x2 <- sort(x, decreasing = TRUE)
  frac <- x2/total # faction of total precip
  cum <- cumsum(frac) # cumulative fraction of total precip

  # number of days it took to get at least half annual precip:
  n_days <- which(cum >= .5)[1] # which

  n_days
}

