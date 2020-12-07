
test_that("calc_age functions work", {
  calc_age <- calc_age_offspring
  expect_true(abs(calc_age() - 0) < 0.01)
  calc_age <- calc_age_basic
  expect_true(all(is.integer(calc_age(100)))) 
  calc_age <- calc_age_usa
  expect_true(all(is.integer(calc_age(100)))) 
})

test_that("calc_fertility functions work", {
  calc_fertility <- calc_fertility_basic
  expect_true(abs(calc_fertility() - logit(0.001)) < 0.01)
  expect_true(abs(calc_fertility(0.05) - logit(0.05)) < 0.01)
  expect_true(abs(calc_fertility(0.1) - logit(0.1)) < 0.01)
  expect_true(calc_fertility(0) == -Inf)
  calc_fertility <- calc_fertility_usa
  expect_true(all(abs(calc_fertility(ages = 15:19) - (-9.11)) < 0.01))
  expect_true(all(abs(calc_fertility(ages = 15:19, tic_length = 365) - (-3.17)) < 0.01))
  expect_true(all(calc_fertility(ages = c(0:14, 50:119)) == -Inf))
  expect_true(is.na(calc_fertility(ages = 120)))
})

test_that("calc_mortality functions work", {
  calc_mortality <- calc_mortality_basic
  expect_true(abs(calc_mortality() - logit(0.001)) < 0.01)
  expect_true(abs(calc_mortality(0.05) - logit(0.05)) < 0.01)
  expect_true(abs(calc_mortality(0.1) - logit(0.1)) < 0.01)
  expect_true(calc_mortality(0) == -Inf)
  calc_mortality <- calc_mortality_usa
  expect_true(all(abs(calc_mortality(ages = 15:19) - (-13.25)) < 0.01))
  expect_true(all(abs(calc_mortality(ages = 15:19, tic_length = 365) - (-7.35)) < 0.01))
  expect_true(is.na(calc_mortality(ages = 120)))
})

test_that("calc_emigration functions work", {
  calc_emigration <- calc_emigration_basic
  expect_true(abs(calc_emigration() - logit(0.001)) < 0.01)
  expect_true(abs(calc_emigration(0.05) - logit(0.05)) < 0.01)
  expect_true(abs(calc_emigration(0.1) - logit(0.1)) < 0.01)
  expect_true(calc_emigration(0) == -Inf)
})
