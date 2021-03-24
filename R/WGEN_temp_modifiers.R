# temperature modifications/correction functions

# convert celsius to kelvin
c2k <- function(x) x + 273.15

# convert kelvin to c
k2c <- function(x) x - 273.15


#' Adjust temp for change in number of wet days
#'
#' @description   A new (fake) mean temp on 'all' days needs to be calculated.
#' Because the actual proportion of wet days decreases, this keeps the mean temp
#' adjusted for wet days and dry days same as before. Otherwise decreasing the number
#' of wet days would just elevate mean temperature.
#'
#' @param Tdry mean temp on dry days
#' @param Twet mean temp on wet days
#' @param n_all total number of days
#' @param n_wet total number of dry days
#' @param mean_mult scalar that mean precip event is being multiplied by
#'
#' @return A new fake mean temperature on all days (will be higher if mean_mult > 1)
#' This should be used as input to temp_modifer() functions, so that when there
#' are fewer ppt events, dry days are adjusted up less, and wet days are
#' adjusted down more
#'
#' @export
#'
#' @examples
#' # Tdry = 11; Twet = 9; n_all = 100; n_wet = 30
#' adjust_all_days_temp(11, 9, 100, 30) # original mean
#' adjust_all_days_temp(11, 9, 100, 30, mean_mult = 2) # adjust mean
adjust_all_days_temp <- function(Tdry, Twet, n_all, n_wet, mean_mult = 1) {

  prop_wet <- n_wet/n_all # original proportion wet days

  # decreasing proportion wet (by same amount will actually decrease it)
  prop_wet <- prop_wet/mean_mult
  prop_dry <- 1 - prop_wet # adjust proportion wet

  all_days <- prop_wet*Twet + prop_dry*Tdry
  all_days

}



#' temperature modifier
#'
#' @description Lower (raise) temperature of a day based on whether it is wet
#' (dry).
#'
#' @param all_days mean temp on all days
#' @param x_days mean temp on wet (dry) days
#'
#' @return scalar modifier for wet (dry) days
#'
#' @export
#'
#' @examples
#' temp_modifier(10, 2)
temp_modifier <- function(all_days, x_days) {

  # convert to degrees C to K
  all_days <- c2k(all_days)
  x_days <- c2k(x_days)

  # this is in appendix S3 but I'm not sure i'm implementing it correctly
  # out <- x_days/all_days +(x_days - all_days)/all_days

  # therefore using simpler modifier:
  out <- x_days/all_days
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

use_modifier <- function(x, week, is_wet, dry_mod, wet_mod, run_checks = TRUE) {
  if (run_checks){
    # set run_checks to FALSE for testing when want to test on incomplete inputs
    stopifnot(names(dry_mod) == as.character(1:52),
              names(wet_mod) == as.character(1:52),
              week %in% 1:52,
              length(x) == length(week),
              length(x) == length(is_wet))
  }

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
#' @param mean_mult scalar that mean is multiplied when increasing ppt intensity
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
adjust_temp <- function(ppt_df, temp_df, wk_list, mean_mult = 1) {

  threshold <- 0 # wet day threshold

  df1 <- join_temp_ppt(ppt_df, temp_df)

  # adjusting all_days mean for change in number of wet days
  # so mean temps don't get hotter when reduce wet days
  Tmax_adj <- purrr::map(wk_list, function(x) {
    adjust_all_days_temp(Tdry = x$wet_dry_temp[["Tmax_dry"]],
                         Twet = x$wet_dry_temp[["Tmax_wet"]],
                         n_all = x$wet_dry_temp[["n_all"]],
                         n_wet= x$wet_dry_temp[["n_wet"]],
                         mean_mult = mean_mult)
  })

  Tmin_adj <- purrr::map_dbl(wk_list, function(x) {
    adjust_all_days_temp(Tdry = x$wet_dry_temp[["Tmin_dry"]],
                         Twet = x$wet_dry_temp[["Tmin_wet"]],
                         n_all = x$wet_dry_temp[["n_all"]],
                         n_wet= x$wet_dry_temp[["n_wet"]],
                         mean_mult = mean_mult)
  })


  # calculating temperature modifiers for each week
  Tmax_wet_mod <- purrr::map2_dbl(wk_list, Tmax_adj,
                                  function(x, all_days) {

    temp_modifier(all_days = all_days,
                  x_days = x$wet_dry_temp[["Tmax_wet"]])
  })
  Tmax_wet_mod <- smooth_modifier(Tmax_wet_mod)

  Tmax_dry_mod <- purrr::map2_dbl(wk_list, Tmax_adj,
                                  function(x, all_days) {
    temp_modifier(all_days = all_days,
                  x_days = x$wet_dry_temp[["Tmax_dry"]])
  })
  Tmax_dry_mod <- smooth_modifier(Tmax_dry_mod)

  Tmin_wet_mod <- purrr::map2_dbl(wk_list, Tmin_adj, function(x, all_days) {
    temp_modifier(all_days = all_days,
                  x_days = x$wet_dry_temp[["Tmin_wet"]])
  })
  Tmin_wet_mod <- smooth_modifier(Tmin_wet_mod)

  Tmin_dry_mod <- purrr::map2_dbl(wk_list, Tmin_adj,  function(x, all_days) {
    temp_modifier(all_days = all_days,
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



