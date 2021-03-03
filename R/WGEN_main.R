# wrapper WGEN functions than incorporate ppt and temp simulation functions


#' WGEN wrapper
#'
#' @param data data frame of original weather data. Requires at least the
#' following columns: "year",  "month", "date", "Tmax_C", "Tmin_C", "PPT_cm"
#' @param mean_mult scalar to multiply the mean PPT event size by
#' (for adjusting intensity)
#' @param sd_mult scalar to multiply the standard deviation of ppt event size by
#' (for adjusting intensity).
#' @param start_date Start date of weather simulation
#' @param end_date End date of weather simulation
#'
#' @return dataframe with simulated daily temperature and precipitation
#' @export
#'
#' @examples
#' # these are the minimum required columns
#' data <- wx_data[,  c("year",  "month", "date", "Tmax_C", "Tmin_C", "PPT_cm")]
#' mean_mult = 1
#' sd_mult = 1
#' start_date = "1980-01-01"
#' end_date = "1980-12-31"
#' WGEN_main(data = data, mean_mult = mean_mult, sd_mult = sd_mult,
#'           start_date = start_date, end_date = end_date)
WGEN_main <- function(data,
                      mean_mult = 1,
                      sd_mult = 1,
                      start_date = "1980-01-01",
                      end_date = "2010-12-31") {

  # need at least these columns
  req_cols <- c("year",  "month", "date", "Tmax_C", "Tmin_C", "PPT_cm")

  stopifnot(req_cols %in% names(data),
            length(mean_mult) == 1,
            length(sd_mult) == 1)

  # monthly precipitation parameters
  params <- monthly_ppt_params(data = data)

  # adjust parameters to increase intensity
  # note--later update these functions to adjust the 95th percentile instead
  if (mean_mult != 1 | sd_mult != 1) {
    params <- adjust_params(params, mean_mult = mean_mult, sd_mult = sd_mult)
  }

  # generate wet/dry days
  df_markov <- markov_chain(params = params,
                            start_date = start_date,
                            end_date = end_date)

  # generate events on wet days (gamma distribution)
  ppt_df <- generate_events(df = df_markov, params = params)

  # weekly (then 4 week avgs taken) temp parameters
  wk_list <- temp_wk_list(data)

  # generate daily temperature
  temp_df <- generate_temp(wk_list, start_date = start_date,
                           end_date = end_date)

  # adjust wet and dry days
  out <- adjust_temp(ppt_df = ppt_df, temp_df =  temp_df, wk_list = wk_list,
                     mean_mult = mean_mult)

  # here consider adjusting temp to MAT (b/ of change in number of
  # wet days) and ppt to MAP
  out
}
