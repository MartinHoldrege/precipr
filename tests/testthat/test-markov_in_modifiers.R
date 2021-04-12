# test for functions defined in markov_in_modifiers.R
library(dplyr)
# library(testthat)

# misc objects used below -------------------------------------------------

data <-data.frame(rSOILWAT2::dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))

coeffs <- rSOILWAT2::dbW_estimate_WGen_coefs(data)
# doubling intensity
coeffs_adjust <- precipr::adjust_coeffs(coeffs, data, mean_mult = 2,
                                        adjust_sd = TRUE)
# test adjust markov in -----------------------------------------------------

# test a couple related functions
test_that("adjust markov in", {

  # removing column attributes (dim), so can compar
  mkv_woy <- as.data.frame(coeffs[[1]]) %>%
    purrr::map_dfc(function(x) {
      attributes(x) <- NULL
      x
    })

  # true map
  map <- data %>%
    group_by(Year) %>%
    summarize(PPT_cm = sum(PPT_cm), .groups = "drop") %>%
    pull(PPT_cm) %>%
    mean()
  expect_equal(adjust_mkv_woy(mkv_woy, data), mkv_woy)

  # testing that ppt was adjusted correctly

  # checking unconditional ppt probs
  p_W <- calc_p_W(coeffs$mkv_doy, adjust_for_truncnorm = TRUE)
  p_W_adjust <- calc_p_W(coeffs_adjust$mkv_doy, adjust_for_truncnorm = TRUE)

  # probability of ppt doubled with mean_mult = 2
  p_W_diff <- abs(p_W_adjust - p_W/2)

  # not sure why p_W_diff isn't 0, but at least it is very close
  expect_lt(max(p_W_diff), 0.001)


  # the current way I understand to adjust coeffs is to keep P_W_W constant
  expect_equal(coeffs_adjust$mkv_doy$p_W_W, coeffs$mkv_doy$p_W_W)

  expect_equal(coeffs_adjust$mkv_doy$PPT_avg, coeffs$mkv_doy$PPT_avg*2)

  # standard dev adjusted correctly
  orig_z0 <- with(coeffs$mkv_doy, (0 - PPT_avg)/PPT_sd)
  adj_z0 <- with(coeffs_adjust$mkv_doy, (0 - PPT_avg)/PPT_sd)
  expect_equal(adj_z0, orig_z0)

  # expected values should be unchanged (this maybe the most important
  # part of the test!)
  ex <- expected_ppt(coeffs, adjust_for_truncnorm = TRUE)

  ex_adjust <- expected_ppt(coeffs_adjust, adjust_for_truncnorm = TRUE)

  # allowing only trivial differences in map with 2x precip intensity
  expect_lt(abs(ex -ex_adjust), 0.01)

  # expected and actual map should be very close (say 0.1 cm)
  expect_lt(abs(ex - map), 0.1)
})



# creating artificial coeffs -----------------------------------------------

# creating coeffs for later use
coeffs2 <- coeffs

# creating fake mkv_doy values for easier testing
p_W_W <- 0.1
p_W_D <- 0.1
coeffs2$mkv_doy$p_W_W <- p_W_W
coeffs2$mkv_doy$p_W_D <- p_W_D
coeffs2$mkv_doy$PPT_avg <- 1
coeffs2$mkv_doy$PPT_sd <- 0.1
coeffs3 <- coeffs2
coeffs3$mkv_doy$PPT_sd <- 2

# simulate weather
years <- 2000:2300
x_empty <- list(new("swWeatherData")) # empty weather object

# generate weather just based on the input coeffs
wout1 <- rSOILWAT2::dbW_generateWeather(x_empty, years = years,
                                        wgen_coeffs = coeffs,
                                        seed = 123)

wout1_adjust <- rSOILWAT2::dbW_generateWeather(x_empty, years = years,
                                               wgen_coeffs = coeffs_adjust,
                                               seed = 123)

wout2 <- rSOILWAT2::dbW_generateWeather(x_empty, years = years,
                                        wgen_coeffs = coeffs2,
                                        seed = 123)


wout3 <- rSOILWAT2::dbW_generateWeather(x_empty, years = years,
                                        wgen_coeffs = coeffs3,
                                        seed = 123)

# testing generation of n wet days ----------------------------------------

# I want to determine if the number of wet days is being
# properly understood (ie is expectation correct). It looks like
# there are still problems. if this is incorrect, nothing else will work

test_that("n wet days", {
  # coeffs

  # observed and expected should be basically the same
  ex_nwet <- expected_nwet(coeffs)
  obs_nwet <- calc_nwet(data)
  expect_lt(abs(ex_nwet - obs_nwet), 0.1)

  # coeffs vs coeffs_adjust
  ex_nwetb <- expected_nwet(coeffs, adjust_for_truncnorm = TRUE)
  ex_nwet_adjust <- expected_nwet(coeffs_adjust, adjust_for_truncnorm = TRUE)

  # adjusting coeffs should have halved the number of wet days
  expect_lt(abs(ex_nwetb/2 - ex_nwet_adjust), 0.01)

  obs_nwet_adjust <- calc_nwet(wout1_adjust)

  # expected and simulated nwet should be similar
  expect_lt(abs(obs_nwet_adjust - ex_nwet_adjust), 0.5)

  # this here is an issue with the wgen. it doesn't preserve number
  # of ppt days.

  sim_nwet <- calc_nwet(wout1)
  # num nwet expected given current understanding of wgen
  # at least it is working here. unsure why not working later
  ex_nwet_b <- expected_nwet(coeffs, adjust_for_truncnorm = TRUE)
  expect_lt(abs(ex_nwet_b - sim_nwet), 1)

  # Coeffs2

  obs_nwet2 <- 36.525 # what nwet should be for coeffs2
  ex_nwet2 <- expected_nwet(coeffs2)
  expect_equal(ex_nwet2, obs_nwet2)

  sim_nwet2 <- calc_nwet(wout2)
  ex_nwet2b <- expected_nwet(coeffs2, adjust_for_truncnorm = TRUE)
  # here I want to see that simulated ppt has correct number of wet days
  # when sd is small
  expect_lt(abs(ex_nwet2b - sim_nwet2), 1)

  # coeffs3

  # now look at large sd
  sim_nwet3 <- calc_nwet(wout3)

  # it looks like the adjustment for truncated normal distribution is
  # more or less correct.
  diff3 <- sim_nwet3 - expected_nwet(coeffs3, adjust_for_truncnorm = TRUE)

  # want expected and simulated nwet to be similar
  # righ now this is failing, suggesting my new adjustment for p_W
  # is wrong
  expect_lt(abs(diff3), 1)
})


# testing expected ppt ----------------------------------------------------


test_that("test expected map etc", {

  # comparing simulated map of original vs doulbed ppt intensity
  map1 <- calc_map(wout1)
  map1_adjust <- calc_map(wout1_adjust)
  expect_lt(abs(map1 - map1_adjust), 1)

  # tests for fake coeffs
  map <- 36.525 # the last 25 is accounting for leap years

  # unadjusted expected should match with observed map
  expect_equal(expected_ppt(coeffs2), map)

  # adjusted expected should match with simulated map
  ex_map2b <- expected_ppt(coeffs2, adjust_for_truncnorm = TRUE)

  # simulated and expected map should be close
  expect_lt(abs(calc_map(wout2) - map), 1)

  # note here that b/ of large sd simulated map is much higher!
  ex_map3 <- expected_ppt(coeffs3, adjust_for_truncnorm = TRUE)
  diff <- abs(ex_map3 - calc_map(wout3))
  expect_lt(diff, 1)

})


# test adjust temp --------------------------------------------------------

# ran into an issue with adjusting the correction factors, when the CFs
# are 0, because no wet days in a given day. This was an issue with site 197.
# testing that that is resolved, as well as the simulated temps are close
test_that("adjusting CFs", {
  coeffs_0cf <- coeffs
  data_0cf <- data
  # making one weeks have no pptc
  data_0cf$PPT_cm[data_0cf$DOY %in% 7:15] <- 0
  coeffs_0cf <- rSOILWAT2::dbW_estimate_WGen_coefs(data_0cf,
                                                   imputation_type = "mean")

  coeffs_0cf_2x <- adjust_coeffs(coeffs_0cf, data,
                                 mean_mult = 2)

  finite_check <- purrr::map(coeffs_0cf_2x$mkv_woy, is.finite) %>%
    unlist()
  expect_true(isTRUE(all(finite_check)))

  # checking that min/max temps are similar
  Tmax_amb <- calc_mat(wout1, col = "Tmax_C")
  Tmax_2x <- calc_mat(wout1_adjust, col = "Tmax_C")

  Tmin_amb <- calc_mat(wout1, col = "Tmin_C")
  Tmin_2x <- calc_mat(wout1_adjust, col = "Tmin_C")

  expect_lt(abs(Tmax_2x - Tmax_amb), 0.2)
  expect_lt(abs(Tmin_2x - Tmin_amb), 0.1)
})

# test site 119 data ------------------------------------------------------

# There weren't actually problems with simulating site119,
# it just has high inter-annual variance, so MAP is variable, unless
# very many years simulated
if (FALSE){
test_that("site 119 coeffs", {

  # coeffs
  c119_amb <- rSOILWAT2::dbW_estimate_WGen_coefs(wdata119,
                                                 imputation_type = "mean")

  c119_2x <- adjust_coeffs(c119_amb, wdata119, mean_mult = 2)

  # simple initial checks
  expect_equal(c119_amb$mkv_doy$PPT_avg*2, c119_2x$mkv_doy$PPT_avg)
  expect_equal(c119_amb$mkv_doy$PPT_sd*2, c119_2x$mkv_doy$PPT_sd)

  ex_nwet_amb <- expected_nwet(c119_amb, adjust_for_truncnorm = TRUE)
  ex_nwet_2x <- expected_nwet(c119_2x, adjust_for_truncnorm = TRUE)

  nwet_diff <- abs(ex_nwet_amb/2 - ex_nwet_2x)
  # very little difference in number of expected wet days
  expect_lt(nwet_diff, 0.01)

  # unconditional wet probabilities
  p_W_amb <- calc_p_W(c119_amb$mkv_doy, adjust_for_truncnorm = TRUE)
  p_W_2x <- calc_p_W(c119_2x$mkv_doy, adjust_for_truncnorm = TRUE)

  p_W_diffs <- abs(p_W_amb/2 - p_W_2x)
  # p_W difference as expected (2:1 ratio)
  expect_lt(max(p_W_diffs), 0.001)

  # expected ppt
  ex_map_amb <- expected_ppt(c119_amb, adjust_for_truncnorm = TRUE)
  ex_map_2x <- expected_ppt(c119_2x, adjust_for_truncnorm = TRUE)

  expect_lt(abs(ex_map_2x - ex_map_amb), 0.01)

  # simulated map
  years2 <- 2000:3500
  seed <- 126
  wout119_amb <- rSOILWAT2::dbW_generateWeather(x_empty, years = years2,
                                                wgen_coeffs = c119_amb,
                                                seed = seed)
  wout119_2x <- rSOILWAT2::dbW_generateWeather(x_empty, years = years2,
                                               wgen_coeffs = c119_2x,
                                               seed = seed)
  sim_map_amb <- calc_map(wout119_amb)
  sim_map_amb; ex_map_amb
  sim_map_2x; ex_map_2x
  sim_map_2x <- calc_map(wout119_2x)
  sim_diff <-  sim_map_2x - sim_map_amb
  sim_diff
  if(abs(sim_diff) > 1) {
    warning("simulated 2x and ambient MAP different for site 119")
  }
  hist(c119_amb$mkv_doy$PPT_avg/c119_amb$mkv_doy$PPT_sd)
  hist(coeffs$mkv_doy$PPT_avg/coeffs$mkv_doy$PPT_sd)

  # simulated nwet
  sim_nwet_amb <- calc_nwet(wout119_amb)
  sim_nwet_2x <- calc_nwet(wout119_2x)
  # this difference suggests that the number of wet day's (ie p_W), is failing
  # somehow. Not the mean event size (expected value on wet days)
  sim_nwet_diff <- sim_nwet_2x*2 - sim_nwet_amb
  if(abs(sim_nwet_diff) > 1) {
    warning("site 119, simulating wrong number of wet days")
  }

})
}
