# Functions for increasing precipitation intensity

#' @import dplyr

#' @title Increase daily precipitation intensity
#'
#' @param x numeric vector
#'
#' @return numeric vector with same. Doubles daily precipitation intensity.
#' Takes precip from odd days and adds it to even days.
#' @export
#'
#' @examples
#' x <- c(0, 0, 0, 0.1, 0, 2, 0.1, 0, 0, 1, 0, 0.2, 0.2, 0)
#' x2 <- incr_dly_intensity(x)
#' x2
incr_dly_intensity <- function(x) {
  # takes odd days of precip, adds them to even days

  # locations in vector on which it rained
  precip_loc <- which(x > 0)

  # num rain days
  n <- length(precip_loc)

  # days to remove rain from [ie locations in precip_loc vector]
  rm_days <- seq(from = 1, to = (n - 1), by = 2)
  add_days <- 1:n
  add_days <- add_days[!add_days %in% rm_days]

  # positions in precip_loc vector to add precip too.
  add_days <- if (length(add_days) == length(rm_days)) {
    add_days
  } else if ((length(add_days) - 1) == length(rm_days)) {
    add_days[-length(add_days)] # removing last day b/ odd number of days
  } else {
    stop("vector of days removing precip from is wrong length\n")
  }

  # positions in original vector to add/remove from
  add_loc <- precip_loc[add_days]
  rm_loc <- precip_loc[rm_days]

  x_new <- x
  x_new[add_loc] <- x_new[add_loc] + x_new[rm_loc]

  # precip added to other days now goes to 0
  x_new[rm_loc] <- 0
  x_new
}



# incr_event_intensity ----------------------------------------------------


#' Increase event size intensity
#'
#' @param x numeric vector (daily precip)
#' @param from positive integer. The number of events to take precip from
#' and add to the following event(s)
#' @param to positive integer. The number of events to add the removed precip
#' to
#'
#' @return numeric vector, same length as x. Precip events are considered
#' 1 or more consecutive days with precip. Precip was removed from one or more (from)
#' events and added to one or more (to) events, and this is repeated for all events
#' @export
#'
#' @examples
#' y1 <- incr_event_intensity(c(0.1, 0.1, 0, 1))
#' all.equal(y1, c(0, 0, 0, 1.2))
#' y2 <- incr_event_intensity(c(0.2, 0.1, 0.1, 0, 1, 2))
#' all.equal(y2, c(0, 0, 0, 0, 1.2, 2.2))
#' # 2 events added to 1
#' y3 <- incr_event_intensity(c(0.1, 0, 0.2, 0.1, 0, 1),
#'                            from = 2, to = 1)
#' all.equal(y3, c(0, 0, 0, 0, 0, 1.4))
#' y4 <- incr_event_intensity(c(0.3, 0, 0.2, 0.1, 0, 1, 0, 1),
#'                            from = 1, to = 2)
#' all.equal(y4, c(0, 0, 0.3, 0.2, 0, 1.1, 0, 1))
incr_event_intensity <- function(x, from = 1, to = 1) {

  stopifnot(from >= 1,
            to >=1)

  if(from %% 1 != 0 | to %% 1 != 0) {
    stop("from and to arguments need to be positive integers")
  }

  if (from != 1 & to != 1) {
    warning("at least one of from and to arguments should probably be 1")
  }

  df1 <- create_event_df(x)

  df1$is_event <- df1$x > 0

  # calculating length of each event
  df2 <- df1 %>%
    group_by(.data$event) %>%
    mutate(event_length = sum(.data$is_event)) %>%
    ungroup()

  # size of individual events
  size_df <- df2 %>%
    filter(.data$is_event) %>%
    group_by(.data$event) %>%
    summarize(event_size = sum(.data$x), .groups = "drop_last")

  n <- nrow(size_df) # number of events

  cycle_len <- from + to # length of 1 cycle
  n_cycles <- floor(n / (cycle_len)) # number of remove/add combos

  # one remove add cycle (TRUE is if remove)
  one_cycle <- c(rep(TRUE, from), rep(FALSE, to))

  cycles <- rep(one_cycle, n_cycles) # cycles of remove (T) and add (F)
  rm_event_nums = which(cycles)
  # which events to add to
  add_event_nums = which(!cycles)

  events_df <- tibble(
    # these are the event nums that will be manipulated
    "event_num" = 1:(n_cycles*cycle_len),
    "is_rm" = cycles, # remove from this event
    "cycle_num" = rep(1:n_cycles, each = cycle_len)
  )

  df3 <- df2 %>%
    left_join(events_df, by = c("event" = "event_num")) %>%
    group_by(cycle_num, is_rm) %>%
    mutate(
      # number of days that are to be added to in that cycle
      # (equal to event_length when to = 1)
      cycle_add_len = sum(!is_rm))

  # df of how much rain is being removed/added each cycle
  rm_df <- df3 %>%
    filter(.data$is_rm) %>%
    group_by(.data$cycle_num, .data$is_rm) %>%
    summarize(add_size = sum(.data$x), # amount to be added
              .groups = "drop") %>%
    mutate(is_rm = FALSE) # reversing so can join with the add events

  df4 <- df3 %>%
    left_join(rm_df, by = c("cycle_num", "is_rm")) %>%
    mutate(x = ifelse(!is_rm & !is.na(is_rm),
                      # if add event add equal amount to each day
                      .data$x + .data$add_size/.data$cycle_add_len,
                      x))

  # events removed from become 0
  df4$x[df3$is_rm] <- 0

  return(df4$x)
}








