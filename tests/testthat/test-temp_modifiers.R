# test temperature modifiers (multiple functions)


# setting up vectors used below
x <- wx_data$PPT_cm[wx_data$month == 1]
x[1] <- 0.2 # so have even num of wet days
temp_vec <- wx_data$Tmax_C[wx_data$month == 1] # daily temp
is_wet <- x > 0
n_all <- length(x)
n_wet <- sum(is_wet)
all_days1 <- mean(temp_vec) # original mean on all days

# mean on wet/dry days
Tdry <-  mean(temp_vec[!is_wet])
Twet <-  mean(temp_vec[is_wet])

# new fake vectors for testing
week <- rep(1, n_all)
x2 <- rep(all_days1, n_all) # mean temp every day
is_wet2 <- is_wet

is_wet2[is_wet][1:43] <- FALSE # switching half of days to dry


# adjust_all_days_temp ----------------------------------------------------

test_that("adjust_all_days_temp 1", {
  expected_mean <- adjust_all_days_temp(Tdry, Twet, n_all, n_wet, mean_mult = 1)
  expect_equal(all_days1, expected_mean)
})


# multiple tests ------------------------------------------------------------

# tests multiple modifiers together
test_that("test all modifiers", {

  # 'original' modifers
  dry_mod1 <- temp_modifier(all_days1, Tdry)
  wet_mod1 <- temp_modifier(all_days1, Twet)

  names(dry_mod1) <- "1" # use_modifier needs named vectors
  names(wet_mod1) <- "1"

  # Should be true (ie just recreating the mean based on wet/dry days)
  t1 <- use_modifier(x2, week, is_wet, dry_mod1, wet_mod1, run_checks = FALSE)
  expect_equal(mean(t1), all_days1)

  # Note, temp_modifier simplified, it now passes this test
  expect_equal(mean(t1[!is_wet]), Tdry)
  expect_equal(mean(t1[is_wet]), Twet)

  # here are modifiers not accounting for a change in number of wet days
  t2 <- use_modifier(x2, week, is_wet2, dry_mod1, wet_mod1, run_checks = FALSE)
  expect_gt(mean(t2), all_days1) # should be different (this is what we're try to correct)

  # created adjusted modifiers to correct for changes in n_wet
  adjusted_mean <- adjust_all_days_temp(Tdry, Twet, n_all, n_wet, mean_mult = 2)
  dry_mod2 <- temp_modifier(adjusted_mean, Tdry)
  wet_mod2 <- temp_modifier(adjusted_mean, Twet)

  # should be equal with corrected modifiers
  t3 <- use_modifier(x2, week, is_wet2, dry_mod2, wet_mod2, run_checks = FALSE)
  expect_equal(mean(t3), all_days1)

})

