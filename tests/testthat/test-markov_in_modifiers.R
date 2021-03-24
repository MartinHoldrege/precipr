# test for functions defined in markov_in_modifiers.R

# test adjust markov in -----------------------------------------------------

# test a couple related functions
test_that("adjust markov in", {
  data <-data.frame(rSOILWAT2::dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))

  # removing column attributes (dim), so can compare
  coeffs <- rSOILWAT2::dbW_estimate_WGen_coefs(data)
  covar_in <- as.data.frame(coeffs[[1]]) %>%
    purrr::map_dfc(function(x) {
      attributes(x) <- NULL
      x
    })

  expect_equal(adjust_covar_in(covar_in, data), covar_in)

  # creating both temp and precip markov files
  coeffs_adjust <- adjust_mkv_in(coeffs, data, mean_mult = 2)
  coeffs_adjust_sd <- adjust_mkv_in(coeffs, data, mean_mult = 2,
                                    adjust_sd = TRUE)

  # testing that ppt was adjusted correctly
  expect_equal(coeffs_adjust$mkv_doy$p_W_D, coeffs$mkv_doy$p_W_D/2)
  expect_equal(coeffs_adjust$mkv_doy$p_W_W, coeffs$mkv_doy$p_W_W/2)
  expect_equal(coeffs_adjust$mkv_doy$PPT_avg, coeffs$mkv_doy$PPT_avg*2)
  expect_equal(coeffs_adjust$mkv_doy$PPT_sd, coeffs$mkv_doy$PPT_sd)

  # standard dev adjusted correctly
  orig_z0 <- with(coeffs$mkv_doy, (0 - PPT_avg)/PPT_sd)
  adj_z0 <- with(coeffs_adjust_sd$mkv_doy, (0 - PPT_avg)/PPT_sd)
  expect_equal(adj_z0, orig_z0)
})
