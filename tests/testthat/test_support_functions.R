
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


test_that("age binning works", {

  expect_true(identical(bin_ages(1:5, 1:5), 1:5))
  ages <- runif(100, 1, 5)
  expect_true(all(bin_ages(ages, 1:5) == floor(ages)))

  # binning retains anyone at or above the last bin
  ages <- c(1, 2, 3)
  age_bins <- c(1, 2)
  expect_silent(bin_ages(ages, age_bins))

  ages <- c(1, 2, 3)
  age_bins <- c(2, 3)
  expect_error(bin_ages(ages, age_bins))

})

test_that("inspect_ppl works", {

  test <- list(
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 2, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_silent(inspect_ppl(test))

  # warning: current_partner column is missing
  test <- list(
    list(father = NA, mother = NA, to_reproduce = NA, female = TRUE, age = 1, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, female = TRUE, age = 2, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, female = TRUE, age = 1, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_warning(inspect_ppl(test))

  # error: age column is missing
  test <- list(
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(inspect_ppl(test))

  # error: female listed as father
  test <- list(
    list(father = 2, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 2, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(inspect_ppl(test))

  # error: male listed as mother
  test <- list(
    list(father = NA, mother = 3, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 2, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = FALSE, age = 1, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(inspect_ppl(test))

  # error: father id invalid
  test <- list(
    list(father = 4, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 2, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(inspect_ppl(test))

  # error: mother id invalid
  test <- list(
    list(father = NA, mother = 4, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 2, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, to_reproduce = NA, current_partner = NA, female = TRUE, age = 1, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(inspect_ppl(test))

})
