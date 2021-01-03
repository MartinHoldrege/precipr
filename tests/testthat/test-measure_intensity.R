context("measure intensity")
library(dplyr)

# n_events ----------------------------------------------------------------


test_that("n events", {

  expect_equal(n_events(rep(c(0, 1), 5)), 5)

  expect_equal(n_events(rep(c(0, 1), each = 5)), 1)

  expect_equal(n_events(c(1, 1, 0, 0.5, 0, .1, .2, 0), min_length = 2), 2)
})


# max_event_length -----------------------------------------------------------


test_that("max event length", {
  expect_equal(max_event_length(c(0, 1, 1, 2, 0, 1)), 3)
})


# mean event size ---------------------------------------------------------

test_that("mean event size", {
  expect_equal(mean_event_size(c(0.1, 0.1, 0, 0.2)), 0.2)
  expect_equal(mean_event_size(0), 0)
  expect_equal(mean_event_size(rep(0.1, 10)), 1)
})


# precip_half -------------------------------------------------------------

test_that("precip half", {
  expect_equal(precip_half(c(0.1, 0.1, 0, 0.2)), 1)
  expect_equal(precip_half(c(0.1, 0.1, 0, 0.2, 0.1)), 2)
  expect_equal(precip_half(c(0.1, 0.1, 0, 0.2, 0.1), threshold = 0.2), 1)
  expect_equal(precip_half(rep(0, 10)), 0)
})
