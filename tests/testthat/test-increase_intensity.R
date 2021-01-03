library(dplyr)
context("increase intensity functions")

test_that("increase daily intensity", {

  x <- c(0, 0, 0, 0.1, 0, 2, 0.1, 0, 0, 1, 0, 0.2, 0.2, 0)
  x2 <- incr_dly_intensity(x)
  expect_equal(sum(x2), sum(x))
  expect_equal(sum(x2 > 0), ceiling(sum(x > 0) / 2))
})

test_that("increase event intensity", {

  # default (doubling event size)
  y1 <- incr_event_intensity(c(0.1, 0.1, 0, 1))
  expect_equal(y1, c(0, 0, 0, 1.2))

  y2 <- incr_event_intensity(c(0.2, 0.1, 0.1, 0, 1, 2))
  expect_equal(y2, c(0, 0, 0, 0, 1.2, 2.2))

  # 2 events added to 1
  y3 <- incr_event_intensity(c(0.1, 0, 0.2, 0.1, 0, 1),
                             from = 2, to = 1)
  expect_equal(y3, c(0, 0, 0, 0, 0, 1.4))

  # 1 event distributed to the next two
  y4 <- incr_event_intensity(c(0.3, 0, 0.2, 0.1, 0, 1, 0, 1),
                             from = 1, to = 2)
  expect_equal(y4, c(0, 0, 0.3, 0.2, 0, 1.1, 0, 1))

})
