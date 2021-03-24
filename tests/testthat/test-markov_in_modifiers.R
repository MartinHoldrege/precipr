# test for functions defined in markov_in_modifiers.R

# test adjust_covar_in -----------------------------------------------------

test_that("adjust covar in", {
  data <-data.frame(rSOILWAT2::dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))

  # removing column attributes (dim), so can compare
  covar_in <- as.data.frame(rSOILWAT2::dbW_estimate_WGen_coefs(data)[[1]]) %>%
    purrr::map_dfc(function(x) {
      attributes(x) <- NULL
      x
    })
  covar_in_adjust <- adjust_covar_in(covar_in, data, mean_mult = 2)
  expect_equal(adjust_covar_in(covar_in, data), covar_in)
})
