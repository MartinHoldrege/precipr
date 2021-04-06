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

  # checking unconditional ppt probs
  p_W <- calc_p_W(coeffs$mkv_doy)
  p_W_adjust <- calc_p_W(coeffs_adjust$mkv_doy)
  expect_equal(p_W_adjust, p_W/2)

  # the current way I understand to adjust coeffs is to keep P_W_W constant
  expect_equal(coeffs_adjust$mkv_doy$p_W_W, coeffs$mkv_doy$p_W_W)

  expect_equal(coeffs_adjust$mkv_doy$PPT_avg, coeffs$mkv_doy$PPT_avg*2)
  expect_equal(coeffs_adjust$mkv_doy$PPT_sd, coeffs$mkv_doy$PPT_sd)

  # standard dev adjusted correctly
  orig_z0 <- with(coeffs$mkv_doy, (0 - PPT_avg)/PPT_sd)
  adj_z0 <- with(coeffs_adjust_sd$mkv_doy, (0 - PPT_avg)/PPT_sd)
  expect_equal(adj_z0, orig_z0)

  # expected values should be unchanged (this maybe the most important
  # part of the test!)
  ex <- expected_ppt(coeffs)
  ex_adjust <- expected_ppt(coeffs_adjust)
  expect_equal(ex, ex_adjust)
})
