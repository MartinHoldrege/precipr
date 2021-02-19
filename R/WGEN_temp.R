# temperature functions ---------------------------------------------------

# NEXT make function that adjusts mean temp to observed MAT
# also make a main function that is a wrapper for all the other functions

#' calculate the M0 matrix
#'
#' This is the autocorrelation/correlation matrix described in
#' eq 8 in Richardson and Wright 1984, and is similar to Wilks, 2011, eq 11.21,
#' except that Wilks uses covariance instead
#'
#' @param Tmax daily max temperature vector
#' @param Tmin daily min temperature vector
#'
#' @return 2x2 matrix
#' @export
#'
#' @examples
#' df <- dplyr::filter(wx_data, month == 1 & day %in% 1:7)
#' year <- df$year
#' Tmax <- df$Tmax_C
#' Tmin <- df$Tmin_C
calc_M0 <- function(Tmax, Tmin) {

  # M0 matrix;  eq 8 in Richardson and Wright 1984,
  r0_max_min <- cor(Tmax, Tmin) # correlation (not lagged)
  out <- matrix(c(1, r0_max_min, r0_max_min, 1),
                ncol = 2,
                byrow = TRUE)
  out
}

#' Calculate the M1 matrix
#'
#' Lag 1 autocorrelation and correlation of min and max temp.
#' eq 11.22b in Wilks 2011, except wilks used covariance instead of correlation.
#' This is eq. 7 in Richardson and Wright (1984).
#' Since I am calculating the
#' lagged correlation by 'hand' (not using acf())
#' the values are slightly (1% for one data set I check)
#' different (b/ of how means calculated), but I'm
#' doing that to deal with the issue of lagging across years.
#'
#' @param Tmax daily max temperature vector
#' @param Tmin daily min temperature vector
#' @param year vector of year. Year is included so lag 1 one works (not finding correlation
#' with lagged value of the previous year)
#'
#' @return 2x2 matrix
#' @export
calc_M1 <- function(Tmax, Tmin, year) {
  stopifnot(length(Tmax) == length(Tmin),
            length(year) == length(Tmax))



  # vectors for lagging
  t <- 1:(length(year)-1)

  # t + 1 (i.e. the lagged index)
  t_p_1 <- t + 1

  # when lagged values refer to the same year
  # these are the endices to take the covariance at
  same_yr <- year[t] == year[t_p_1]


  t_p_1 <- t_p_1[same_yr] # only use values when years match
  t <- t[same_yr]

  r1_max <- cor(Tmax[t], Tmax[t_p_1])
  r1_min <- cor(Tmin[t], Tmin[t_p_1])


  # cross correlations (lagged)

  # lagged Tmin
  r1_max_min <-  cor(Tmax[t], Tmin[t_p_1])

  # lagged Tmax
  r1_min_max <-  cor(Tmin[t], Tmax[t_p_1])

  out <- matrix(c(r1_max, r1_min_max, r1_max_min, r1_min),
                ncol = 2,
                byrow = TRUE)
  out
}

#' Calculate A matrix
#'
#' Equation 4 in Richardson and Wright (1984)
#'
#' @param M0 M0 matrix (from calc_M0() function)
#' @param M1 M1 matrix (from calc_M1() function)
#'
#' @return 2x2 matrix
#' @export
#'
#' @examples
#' df <- dplyr::filter(wx_data, month == 2 & day %in% 1:7)
#' Tmax <- df$Tmax_C
#' Tmin <- df$Tmin_C
#' year <- df$year
#' M0 <- calc_M0(Tmax, Tmin)
#' M1 <- calc_M1(Tmax, Tmin, year)
#' A <- calc_A(M0, M1)
calc_A <- function(M0, M1) {

  A <- M1 %*% solve(M0)
  A
}

#' Calculate B matrix
#'
#' Equation 5 in Richardson and Wright (1984)
#'
#' @param M0 M0 matrix (from calc_M0() function)
#' @param M1 M1 matrix (from calc_M1() function)
#'
#' @return 2x2 matrix
#' @export
calc_B <- function(M0, M1) {

  # eq 5 in Richardson and Wright 1984
  BBT <- M0 - M1 %*% solve(M0) %*% t(M1)

  # taking square root
  # switched back to correlation matrices because the
  # covariance matrix had imaginary numbers in the square root
  B <- expm::sqrtm(BBT)
  B
}

#' List of weekly temperature parameters
#'
#' @param data dataframe of original weather dat with "date", "Tmax_C",
#'  "Tmin_C", "PPT_cm" columns
#'
#' @return list with 52 elements (1 for each week of the year, week 52 and 53
#' are combined). Each list element is a list that contains several elements
#' including: "mean", mean temperature on all days; "sd", sd of temp;
#' "wet_dry_temp", which is mean temp and wet and dry days; "A", A matrix;
#' "B", B matrix.
#'
#' @export
#'
#' @examples
#' wk_list <- temp_wk_list(wx_data)
#' # plotting min/max temps
#' Tmax_mean <- purrr::map_dbl(wk_list, function(x) x$means[1])
#' plot(Tmax_mean, type = "l", col = "red")
#' lines(purrr::map_dbl(wk_list, function(x) x$means[2]), col = "blue")
temp_wk_list <- function(data) {

  stopifnot(
    is.data.frame(data),
    c("date", "Tmax_C", "Tmin_C", "PPT_cm") %in% names(data)
  )

  threshold <- 0
  # create list with weekly means, and A and B matrices
  df1 <- data
  df1$week <- week52(df1$date)

  # for calculating temp on wet and dry days
  df1$is_wet <- df1$PPT_cm > threshold

  wk_list <- split(df1, df1$week)

  # making sure list is correctly ordered
  stopifnot(names(wk_list) == as.character(1:52))

  # M0 matrices (covariance version of what is in) Richardson wright 1984
  M0_list <- purrr::map(wk_list, function(df) {
    calc_M0(Tmax = df$Tmax_C, Tmin = df$Tmin_C)
  })

  M1_list <- purrr::map(wk_list, function(df) {
    calc_M1(Tmax = df$Tmax_C, Tmin = df$Tmin_C, year = df$year)
  })

  A_list <- purrr::map2(M0_list, M1_list, calc_A)
  B_list <- purrr::map2(M0_list, M1_list, calc_B)

  # eq 12 in Richardson and Wright 1984 calls for seasonal sd, (here
  # i'm using weekly sd, but across years)
  sd_list <- purrr::map(wk_list, function(df) {
    c(Tmax_sd = sd(df$Tmax_C), Tmin_sd = sd(df$Tmin_C))
  })

  # means for the week
  mean_list <- purrr::map(wk_list, function(df) {
    c("Tmax_mean" = mean(df$Tmax_C), "Tmin_mean" = mean(df$Tmin_C))
  })

  # temp on wet and dry days
  wet_dry_temp_list <- purrr::map(wk_list, function(df) {
    c("Tmax_dry" = mean(df$Tmax_C[!df$is_wet]),
      "Tmax_wet" = mean(df$Tmax_C[df$is_wet]),
      "Tmin_dry" = mean(df$Tmin_C[!df$is_wet]),
      "Tmin_wet" = mean(df$Tmin_C[df$is_wet]))
  })

  # combined elements needed for prediction (eq. 11.21 in Wilks, 2011)
  out <- purrr::pmap(list(mean_list, sd_list, wet_dry_temp_list,  A_list, B_list),
                     function(mean, sd, wet_dry_temp, A, B) {
                       list(means = mean, sd = sd, wet_dry_temp = wet_dry_temp, A = A, B = B)
                     })
  out
}



#' generate temperature
#'
#' @param wk_list list of weekly parameters from temp_wk_list() function
#' @param start_date "yyy-mm-dd" string for start date of simulated temperature
#' @param end_date "yyy-mm-dd" string for end date of simulated temperature
#'
#' @return dataframe, that includes simulated minimum and maximum temperature
#' @export
#'
#' @examples
#' wk_list <- temp_wk_list(wx_data)
#' test <- generate_temp(wk_list, "1980-01-01", "1982-12-31")
#' plot(test$Tmax_C, col = "red", type = "l")
#' lines(test$Tmin_C, col = "blue")
#' hist((test$Tmax_C-test$Tmin_C))
#' # testing
#' if (FALSE) {
#' test <- generate_temp(wk_list, "1980-01-01", "2079-12-31") # 30 sec to run
#' dplyr::summarize(test, Tmax_C = mean(Tmax_C), Tmin_C = mean(Tmin_C))
#' # compare to original data
#' dplyr::summarize(wx_data, Tmax_C = mean(Tmax_C), Tmin_C = mean(Tmin_C))
#' }
generate_temp <- function(wk_list,
                          start_date = "1980-01-01",
                          end_date = "2010-12-31") {

  # setting up empty dataframe
  df1 <- tibble(date = seq(from = lubridate::ymd(start_date),
                           to = lubridate::ymd(end_date),
                           by = "1 day")) %>%
    mutate(week = week52(.data$date),
           Tmax_C = NA_real_,
           Tmin_C = NA_real_)

  # first day in chain just using mean, to 'initialize'
  df1$Tmax_C[1] <- wk_list[[as.character(df1$week[1])]]$means["Tmax_mean"]
  df1$Tmin_C[1] <- wk_list[[as.character(df1$week[1])]]$means["Tmin_mean"]

  # generating Tmax/tmin as per eq 11.21 in wilks, 2011
  for (t in 1:(nrow(df1) -1)) {

    # mean vector for the given week (note just using sample mean, not some
    # smoothed value, to simplify things)
    mu <- wk_list[[as.character(df1$week[t])]]$means
    A <- wk_list[[as.character(df1$week[t])]]$A
    B <- wk_list[[as.character(df1$week[t])]]$B

    sd <- as.matrix(wk_list[[as.character(df1$week[t])]]$sd, ncol = 1)

    # error at time t +1
    et1 <- rnorm(2, mean = 0, sd = 1)
    et1 <- as.matrix(et1, ncol = 1)
    xt <- c(df1$Tmax_C[t], df1$Tmin_C[t])
    rt <- (xt  - mu)/sd # residuals at time t
    rt <- as.matrix(rt, ncol = 1)

    # residual vector (Tmax, Tmin), at time t + 1 (eq 3 Richardson and wilks)
    rt1 <- A%*%rt + B%*%et1

    # eq 12 in R & W 1984). Calculate observed value
    xt1 <- rt1*sd + as.matrix(mu, ncol = 1)

    df1$Tmax_C[(t+1)] <- xt1[1,]
    df1$Tmin_C[(t+1)] <- xt1[2,]
  }

  # fix Tmin/max
  # on rare occasions a tmin greater than a tmax can be generated
  # in those cases the two will be flipped
  is_flipped <- df1$Tmax_C < df1$Tmin_C
  df1$Tmin_C <- ifelse(is_flipped, df1$Tmax_C, df1$Tmin_C)
  df1$Tmax_C <- ifelse(is_flipped, df1$Tmin_C, df1$Tmax_C)

  df1

}

#' join temp and precip dataframes
#'
#' @param ppt_df dataframe with precip (must have "date" column)
#' @param temp_df dataframe with temperature (must have "date" column)
#'
#' @return dataframe joined on "date"
#' @export
#'
#' @examples
#' params <- monthly_ppt_params(wx_data)
#' df1 <- markov_chain(params,  start_date = "1980-01-01", end_date = "1980-12-31")
#' ppt_df <- generate_events(df1, params)
#' wk_list <- temp_wk_list(wx_data)
#' temp_df <- generate_temp(wk_list, start_date = "1980-01-01", end_date = "1980-12-31")
join_temp_ppt <- function(ppt_df, temp_df) {
  out <- inner_join(ppt_df, temp_df, by = "date")

  # very strict to catch errors. could change
  stopifnot(nrow(out) == nrow(temp_df))

  out
}

# convert celsius to kelvin
c2k <- function(x) x + 273.15

# convert kelvin to c
k2c <- function(x) x - 273.15

# temperature modifier for wet or dry days
temp_modifier <- function(all_days, x_days) {
  # args:
  #  all_days--temp on all days
  #   x_days--temp on either wet or dry days
  # returns: temp modifier to wet/dry days

  # equation 13 in appendix S3 in Palmquist et al (in press, 2021)

  # convert to degrees C to K
  all_days <- c2k(all_days)
  x_days <- c2k(x_days)
  out <- x_days/all_days +(x_days - all_days)/all_days
  out
}


smooth_modifier <- function(x) {
  # to smooth the temp modifier, take averages of 4 week periods
  # i'm doing this because there is a lot of scatter in the weekly modifiers
  # however, I do see some seasonal trend so just a straight yearly average
  # may not be appropriate
  stopifnot(length(x) == 52)

  # break into 4 week periods.
  # this is mildly incorrect because 'week' 52 has more than 7 days, so weighting
  # will be slightly off
  period <- rep(1:13, each = 4)
  period_mean <- aggregate(x, by = list(period = period), FUN = mean)
  # 52 length vector, but each element is just the mean for the period
  out <- period_mean$x[period]
  names(out) <- names(x)
  out
}

use_modifier <- function(x, week, is_wet, dry_mod, wet_mod) {
  stopifnot(names(dry_mod) == as.character(1:52),
            names(wet_mod) == as.character(1:52),
            week %in% 1:52,
            length(x) == length(week),
            length(x) == length(is_wet))



  x_k <- c2k(x)

  # multiply by the wet day modifier for the given week
  x_k[is_wet] <- x_k[is_wet]*wet_mod[week[is_wet]]

  # multiply by dry day modifier
  x_k[!is_wet] <- x_k[!is_wet]*dry_mod[week[!is_wet]]
  x_c <- k2c(x_k)
  x_c
}


#' adjust temperature for wet and dry days
#'
#' @param ppt_df data frame of precip data (from generate_events())
#' @param temp_df dataframe of temp data from generate_temp()
#' @param wk_list list of weakly temperature parameters from temp_wk_list()
#'
#' @return dataframe of PPT and temp, with Tmax_C and Tmin_C adjusted
#' for wet/dry days
#'
#' @export
#'
#' @examples
#' params <- monthly_ppt_params(wx_data)
#' df1 <- markov_chain(params,  start_date = "1980-01-01", end_date = "1980-12-31")
#' ppt_df <- generate_events(df1, params)
#' wk_list <- temp_wk_list(wx_data)
#' temp_df <- generate_temp(wk_list, start_date = "1980-01-01", end_date = "1980-12-31")
#' comb <- adjust_temp(ppt_df, temp_df, wk_list)
#' mean(comb$Tmax_C); mean(temp_df$Tmax_C)
adjust_temp <- function(ppt_df, temp_df, wk_list) {

  threshold <- 0 # wet day threshold

  df1 <- join_temp_ppt(ppt_df, temp_df)

  # calculating temperature modifiers for each week
  Tmax_wet_mod <- purrr::map_dbl(wk_list, function(x) {
    temp_modifier(all_days = x$means[["Tmax_mean"]],
                  x_days = x$wet_dry_temp[["Tmax_wet"]])
  })
  Tmax_wet_mod <- smooth_modifier(Tmax_wet_mod)
  Tmax_dry_mod <- purrr::map_dbl(wk_list, function(x) {
    temp_modifier(all_days = x$means[["Tmax_mean"]],
                  x_days = x$wet_dry_temp[["Tmax_dry"]])
  })
  Tmax_dry_mod <- smooth_modifier(Tmax_dry_mod)
  Tmin_wet_mod <- purrr::map_dbl(wk_list, function(x) {
    temp_modifier(all_days = x$means[["Tmin_mean"]],
                  x_days = x$wet_dry_temp[["Tmin_wet"]])
  })
  Tmin_wet_mod <- smooth_modifier(Tmin_wet_mod)
  Tmin_dry_mod <- purrr::map_dbl(wk_list, function(x) {
    temp_modifier(all_days = x$means[["Tmin_mean"]],
                  x_days = x$wet_dry_temp[["Tmin_dry"]])
  })
  Tmin_dry_mod <- smooth_modifier(Tmin_dry_mod)

  is_wet <- df1$PPT_cm > threshold
  df1$Tmax_C <- use_modifier(df1$Tmax_C,
                             df1$week,
                             is_wet = is_wet,
                             dry_mod = Tmax_dry_mod,
                             wet_mod = Tmax_wet_mod)

  df1$Tmin_C <- use_modifier(df1$Tmin_C,
                             df1$week,
                             is_wet = is_wet,
                             dry_mod = Tmin_dry_mod,
                             wet_mod = Tmin_wet_mod)
  return(df1)
}
