
test_that("logistic and logit work correctly", {
  expect_silent(x <- logistic(rnorm(10000, 0, 10)))
  expect_true(all(x >= 0 & x <= 1))
  expect_silent(x <- logit(runif(10000, 0, 1)))
  expect_true(min(x) > -50 & max(x) < 50)
  expect_true(logistic(Inf) == 1)
  expect_true(logistic(-Inf) == 0)
})

test_that("sample_safe works correctly", {
  expect_true(all(sample_safe(c(100), 1000, replace = TRUE) == 100))
})


test_that("inspect_ppl works", {

  test <- list(
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 2, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_silent(inspect_ppl(test))

  test <- list(
    list(father = NA, mother = NA, to_reproduce = NA, female = TRUE, age = 1, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, female = TRUE, age = 2, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, female = TRUE, age = 1, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_warning(inspect_ppl(test))

  test <- list(
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(inspect_ppl(test))

})
