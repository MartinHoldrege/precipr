# temperature functions ---------------------------------------------------

# NEXT make function that adjusts mean temp to observed MAT

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
      "Tmin_wet" = mean(df$Tmin_C[df$is_wet]),
      # number of days total
      "n_all" = length(df$is_wet),
      # number of wet days
      "n_wet" = sum(df$is_wet))
  })



  # combined elements needed for prediction (eq. 11.21 in Wilks, 2011)
  out <- purrr::pmap(list(mean_list, sd_list, wet_dry_temp_list,  A_list, B_list),
                     function(mean, sd, wet_dry_temp, A, B) {
                       list(means = mean, sd = sd, wet_dry_temp = wet_dry_temp, A = A, B = B)
                     })
  out
}


#' dataframe of weekly temp parameters for adjusting rSOILWAT Wgen coeffs
#'
#'
#' @param data dataframe of original weather dat with "Year", "DOY", "Tmax_C",
#'  "Tmin_C", "PPT_cm" columns (columns as needed by
#'  rSOILWAT2::dbW_estimate_WGen_coefs)
#' @param mean_mult what mean event size will be multiplied by
#'
#' @return a dataframe with mean temperature, and number of wet days in each
#' week of year
#'
#' @export
#'
#' @examples
#' data <-data.frame(rSOILWAT2::dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))
#' head(wk_summary_wgen(data))
#' head(rSOILWAT2::dbW_estimate_WGen_coefs(data)[[1]])
#' # compare
#' wk_summary_wgen(data, mean_mult = 2)
wk_summary_wgen <- function(data, mean_mult = 1) {

  stopifnot(
    is.data.frame(data),
    c("Year", "DOY", "Tmax_C", "Tmin_C", "PPT_cm") %in% names(data)
  )

  threshold <- 0 # threshold for what is considered a wet day
  df1 <- data

  # this is the same way week is defined in rSOILWAT2::dbW_estimate_WGen_coefs
  df1$week <- 1 + floor((df1$DOY - 1)/7)

  # for calculating temp on wet and dry days
  df1$is_wet <- df1$PPT_cm > threshold

  out <- df1 %>%
    group_by(.data$week) %>%
    summarize("Tmax_mean" = mean(.data$Tmax_C),
              "Tmin_mean" = mean(.data$Tmin_C),
              "Tmax_dry" = mean(.data$Tmax_C[!.data$is_wet]),
              "Tmax_wet" = mean(.data$Tmax_C[.data$is_wet]),
              "Tmin_dry" = mean(.data$Tmin_C[!.data$is_wet]),
              "Tmin_wet" = mean(.data$Tmin_C[.data$is_wet]),
              # number of days total
              "n_all" = length(.data$is_wet),
              # number of wet days
              "n_wet" = sum(.data$is_wet),
              .groups = "drop") %>%
    # solving issue that when there are no wet days in a week, NaN is
    # returned
    mutate("Tmax_wet" = ifelse(!is.finite(.data$Tmax_wet), .data$Tmax_mean,
                               .data$Tmax_wet),
           "Tmin_wet" = ifelse(!is.finite(.data$Tmin_wet), .data$Tmin_mean,
                               .data$Tmin_wet)) %>%
    mutate(Tmax_new = adjust_all_days_temp(Tdry = .data$Tmax_dry,
                                           Twet = .data$Tmax_wet,
                                           n_all = .data$n_all,
                                           n_wet = .data$n_wet,
                                           mean_mult = mean_mult),
          Tmin_new = adjust_all_days_temp(Tdry = .data$Tmin_dry,
                                           Twet = .data$Tmin_wet,
                                           n_all = .data$n_all,
                                           n_wet = .data$n_wet,
                                           mean_mult = mean_mult)
    )


  # making sure df is correctly ordered
  stopifnot(out$week == 1:53)

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

