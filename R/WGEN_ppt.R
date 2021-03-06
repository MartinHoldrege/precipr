# Martin Holdrege

# script started 2/16/21

# functions to code a richardson and wright (1984) weather generator
# with some simplifications made on my part

#' @import dplyr

# recommended by devtools::check():
# #' @importFrom stats, cor, rgamma, rnorm, runif, sd

# To Do
# checks: monthly SD, and mean temp, and precip
# consecutive rain days
# adjust all temps to match long term mean
# (i.e. to compensate for changes in wet/dry days)
# adjust precip to match MAP (optional)
# create ppt percentile mapping

# precipitation functions -------------------------------------------------

#' @title  probability of rain given previous day's wet/dry status
#'
#' @param x numeric vector of daily precip
#' @param year numeric vector. Passing function the year as well,
#' so don't 'look' at previous day that is actually in previous year.
#' @param return use "both" (return both probabilities), "P_W_W" (probability
#' is wet given previous day was wet), or "P_W_D" (prob wet given previous
#' day was dry)
#'
#' @return vector of length 1 or 2
#' @export
#'
#' @examples
#' x <- wx_data$PPT_cm[wx_data$month == 1]
#' year <- wx_data$year[wx_data$month == 1]
#' return <- "both"
#' P_W_X(x, year, return)
P_W_X <- function(x, year, return = "both") {
  # probability of wet day following wet day
  stopifnot(is.numeric(x),
            all(!is.na(x)),
            return %in% c("both", "P_W_W", "P_W_D"),
            length(x) == length(year),
            sort(year) == year)

  # could change definition of wet day to some threshold but using 0
  # because that is what stepwat2 was using
  threshold <- 0
  is_wet <- x > threshold

  # if diff == 0, then is_prev_wet applies be/ the previous day of month
  # was that year
  same_year <- diff(year) == 0
  is_prev_wet <- is_wet[1:(length(x)-1)] # is previous day wet
  is_prev_wet <- is_prev_wet[same_year]

  x2 <- x[-1] # can't look at day before the first day
  x2 <- x2[same_year]

  # wet days that were wet on the preceding day
  W_W <- sum(x2[is_prev_wet] > 0)
  P_W_W <- W_W/sum(is_prev_wet) # probability of wet day given previous day wet

  # dry day were wet on preceding day
  W_D <- sum(x2[!is_prev_wet] > 0)

  # prob of wet day given previous day dry
  P_W_D <- W_D/sum(!is_prev_wet)

  out <- c("P_W_W" = P_W_W,
           "P_W_D" = P_W_D)

  if(return == "both") {
    return(out)
  } else if (return == "P_W_D") {
    return(out["P_W_D"])
  } else if (return == "P_W_W") {
    return(out["P_W_W"])
  } else {
    stop("incorrect return string given")
  }
}


#' monthly precipitation parameters
#'
#'
#' @param data dataframe of daily precipitation  (must include PPT_cm, year,
#' and  month columns)
#'
#' @return dataframe
#' @export
#'
#' @examples
#' monthly_ppt_params(wx_data)
monthly_ppt_params <- function(data) {
  threshold <- 0 # threshold for wet day
  # minimum required columns
  stopifnot(c("PPT_cm", "month", "year") %in% names(data))

  if ("site" %in% names(data) && length(unique(data$site)) != 1) {
    stop("function only meant to handle one site worth of data")
  }

  out <- data %>%
    ungroup() %>%
    group_by(.data$month) %>%
    summarize(P_W_W = P_W_X(.data$PPT_cm, year = .data$year,
                            return = "P_W_W"), # prob wet, prev wet
              # prob wet given previous dry
              P_W_D = P_W_X(.data$PPT_cm, year = .data$year,
                            return = "P_W_D"),
              PPT_mean = mean(.data$PPT_cm[.data$PPT_cm > threshold]),
              PPT_SD = sd(.data$PPT_cm[.data$PPT_cm > threshold]),
              .groups = "drop")
  out
}


#' first order markov chain
#'
#' Markov chain used simulate wet and dry days
#'
#' @param params dataframe of parameters from monthly_ppt_params() function or
#' from adjust_params() function
#' @param start_date start date of simulation
#' @param end_date  end date of simulation
#'
#' @return dataframe, with date, year, month, day and is_wet columns. This final
#' column is logical of whether precip occured that day
#'
#' @export
#'
#' @examples
#' params <- monthly_ppt_params(wx_data)
#' df <- markov_chain(params)
#' df
#' sum(df$is_wet)/nrow(df) # percent of days that are wet
#' sum(wx_data$PPT_cm > 0)/nrow(wx_data) # comparison to original data
markov_chain <- function(params,
                         start_date = "1980-01-01",
                         end_date = "2010-12-31") {

  stopifnot(is.data.frame(params),
            c("month", "P_W_W", "P_W_D") %in% names(params))

  date_seq <- seq(from = lubridate::ymd(start_date),
                  to = lubridate::ymd(end_date), by = "1 day")

  stopifnot(length(date_seq) > 2)

  # data frame with sequential dates
  df1 <- dplyr::tibble(date = date_seq) %>%
    mutate(year = lubridate::year(.data$date),
           month = lubridate::month(.data$date),
           day = lubridate::mday(.data$date),
           is_wet = NA)

  # markov chain

  # for starters making first day dry (could make stochastic)
  # shouldn't matter if simulated chain is long enough
  df1$is_wet[1] <- FALSE
  for (i in 2:length(df1$date)) {
    wet_prob <- if (df1$is_wet[(i-1)]) {
      # prob if prev day wet
      params$P_W_W[params$month == df1$month[i]]
    } else {
      # prob if prev day dry
      params$P_W_D[params$month == df1$month[i]]
    }

    # stochastically decide if day is wet or not
    df1$is_wet[i] <- runif(1) < wet_prob
  }

  return(df1)
}

#' Adjust parameters of monthly precipitation
#'
#' @param params  dataframe of parameters from monthly_ppt_params() function
#' @param mean_mult amount to multiply the daily mean by
#' @param sd_mult amount to multiply the standard deviation by
#'
#' @return adjusted parameters with probability of rain multiplied by
#' a factor of 1/mean_mult so that total precip is held constant
#' @export
#'
#' @examples
#' mean_mult <- 1.5
#' sd_mult <- 2
#' params <- monthly_ppt_params(wx_data)
#' params2 <- adjust_params(params, mean_mult, sd_mult)
#' df <- markov_chain(params2)
#' df2 <- generate_events(df, params2)
#' # make histograms
#' if (FALSE){
#' breaks <- seq(from = 0, to =10, by = 0.25)
#' hist(df2$PPT_cm[df2$PPT_cm > 0], breaks = breaks)
#' hist(wx_data$PPT_cm[wx_data$PPT_cm > 0],breaks = breaks)
#' }
adjust_params <- function(params, mean_mult, sd_mult) {

  stopifnot(is.data.frame(params),
            c("P_W_W", "P_W_D", "PPT_mean", "PPT_SD") %in% names(params)
  )

  # probability multiplier (to keep totals the same)
  P_mult <- 1/mean_mult

  # adjust mean, sd and probabilities, goal is that totals remain the same
  params2 <- params %>%
    mutate(P_W_W = .data$P_W_W*P_mult,
           P_W_D = .data$P_W_D*P_mult,
           PPT_mean = .data$PPT_mean*mean_mult,
           PPT_SD = .data$PPT_SD*sd_mult)

  params2
}

#' generate precipitation events
#'
#' Generate daily precipitation on wet days based on gamma distribution
#' described by mean and SD in the params dataframe
#'
#' @param df dataframe, output from markov_chain() function
#' @param params  dataframe of parameters from monthly_ppt_params() function
#' or from adjust_params() function
#'
#' @return dataframe with PPT_cm (daily precip) column added
#' @export
#'
#' @examples
#' params <- monthly_ppt_params(wx_data)
#' df <- markov_chain(params)
#' df2 <- generate_events(df, params)
#' # calculate yearly ppt (original data is 31.6 cm)
#' df2 %>% dplyr::group_by(year) %>%
#' dplyr::summarize(PPT_cm = sum(PPT_cm)) %>%
#' dplyr::summarize(PPT_cm = mean(PPT_cm))
generate_events <- function(df, params) {
  # df is output from markov_chain

  # make sure months are ordered
  stopifnot(1:12==params$month)

  # parameters of gamma distribution
  params$alpha <- params$PPT_mean^2/params$PPT_SD^2
  params$beta <-  params$PPT_mean/params$PPT_SD^2
  df$PPT_cm <- NA_real_
  df$PPT_cm[!df$is_wet] <- 0

  # random draws from gamma distribution
  df$PPT_cm[df$is_wet] <- rgamma(sum(df$is_wet),
                                 # params for each month
                                 shape = params$alpha[df$month[df$is_wet]],
                                 rate = params$beta[df$month[df$is_wet]]
  )
  df$is_wet <- NULL
  return(df)
}



