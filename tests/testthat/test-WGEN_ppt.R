# test functions in WGEN_ppt script

test_that("P_W_X", {
  x <- c(0, 1, 0, 0, 2, 0,
         0, 1, 1, 0, 0, 1)
  year <- rep(c(2015, 2016), each = 6)

  # days that that were previously wet, don't count the 7th day (year transition)
  prev_wet <- 4
  P_W_W <- 1/prev_wet # probability of rain if prev day rainy
  prev_dry <- 6
  P_W_D <- 4/prev_dry # probability of rain if prev day dry

  expect_equal(P_W_X(x, year), c(P_W_W = P_W_W, P_W_D = P_W_D))

})
