
test_that("calc_conception functions work", {
  calc_conception <- calc_conception_basic
  expect_true(abs(calc_conception() - logit(0.001)) < 0.01)
  expect_true(abs(calc_conception(0.05) - logit(0.05)) < 0.01)
  expect_true(abs(calc_conception(0.1) - logit(0.1)) < 0.01)
})

test_that("calc_age functions work", {
  calc_age <- calc_age_offspring
  expect_true(abs(calc_age() - 0) < 0.01)
  calc_age <- calc_age_basic
  expect_true(all(is.integer(calc_age(100)))) 
})


