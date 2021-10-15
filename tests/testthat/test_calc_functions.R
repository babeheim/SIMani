
test_that("calc_pr_event works", {
  ppl <- data.frame(age = c(15, 15, 15))
  sch <- data.frame(age = 15:19, events_perthou = c(0, 10, 10, 1, 1))
  expect_true(all(calc_pr_event(ppl, sch) == 0))

  # error: someone is outside the schedule
  ppl <- data.frame(age = c(15, 19))
  sch <- data.frame(age = 15:19, events_perthou = c(0, 10, 10, 1, 1))
  expect_error(calc_pr_event(ppl, sch))

})

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
  ppl <- data.frame(age = 15:19)
  expect_true(all(abs(calc_fertility(ppl) - 0.001) < 0.01))
  expect_true(all(abs(calc_fertility(ppl, 0.05) - 0.05) < 0.01))
  expect_true(all(abs(calc_fertility(ppl, 0.1) - 0.1) < 0.01))
  expect_true(all(calc_fertility(ppl, 0) == 0))
  calc_fertility <- calc_fertility_usa
  ppl <- data.frame(age = 15:19)
  expect_true(all(abs(calc_fertility(ppl, tic_length = 1) - logistic(-9.11)) < 0.01))
  expect_true(all(abs(calc_fertility(ppl, tic_length = 365) - logistic(-3.17)) < 0.01))
  ppl <- data.frame(age = c(0:14, 60:119))
  expect_true(all(calc_fertility(ppl) == 0))
  expect_true(is.na(calc_fertility(data.frame(age = 120))))
})

test_that("calc_mortality functions work", {
  calc_mortality <- calc_mortality_basic
  ppl <- data.frame(age = 15:19)
  expect_true(all(abs(calc_mortality(ppl) - 0.001) < 0.01))
  expect_true(all(abs(calc_mortality(ppl, 0.05) - 0.05) < 0.01))
  expect_true(all(abs(calc_mortality(ppl, 0.1) - 0.1) < 0.01))
  expect_true(all(calc_mortality(ppl, 0) == 0))
  calc_mortality <- calc_mortality_usa
  expect_true(all(abs(calc_mortality(data.frame(age = 15:19), tic_length = 1) - logistic(-13.25)) < 0.01))
  expect_true(all(abs(calc_mortality(data.frame(age = 15:19), tic_length = 365) - logistic(-7.35)) < 0.01))
  expect_true(is.na(calc_mortality(data.frame(age = 120))))
})

test_that("calc_emigration functions work", {
  calc_emigration <- calc_emigration_basic
  ppl <- data.frame(age = 15:19)
  expect_true(all(abs(calc_emigration(data.frame(ppl, age = 15:19)) - 0.001) < 0.01))
  expect_true(all(abs(calc_emigration(data.frame(ppl, age = 15:19), 0.05) - 0.05) < 0.01))
  expect_true(all(abs(calc_emigration(data.frame(ppl, age = 15:19), 0.1) - 0.1) < 0.01))
  expect_true(all(calc_emigration(data.frame(ppl, age = 15:19), 0) == 0))
})
