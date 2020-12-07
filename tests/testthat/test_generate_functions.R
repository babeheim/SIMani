

test_that("generate_people works", {

  test <- list(
    list(age = 1, is_alive = TRUE, is_present = TRUE),
    list(age = 2, is_alive = TRUE, is_present = TRUE),
    list(age = 1, is_alive = FALSE, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame()

  add <- generate_people(100, test, calc_age = calc_age_basic)
  expect_true(nrow(add) == 100)
  expect_true(ncol(add) == 4)

  test2 <- list(
    list(age = 1, is_alive = TRUE, female = TRUE, is_present = TRUE),
    list(age = 2, is_alive = TRUE, female = TRUE, is_present = TRUE),
    list(age = 1, is_alive = FALSE, female = TRUE, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame()
  
  add2 <- generate_people(100, test2, calc_age = calc_age_basic)
  expect_true(nrow(add2) == 100)
  expect_true(ncol(add2) == 4)

})


test_that("generate_population works", {
  new <- generate_population(1, calc_age = calc_age_basic)
  expect_true(nrow(new) == 1)
  new <- generate_population(10, calc_age = calc_age_basic)
  expect_true(nrow(new) == 10)
  new <- generate_population(1000, calc_age = calc_age_basic)
  expect_true(nrow(new) == 1000)
  new <- generate_population(10, calc_age = calc_age_offspring)
  expect_true(nrow(new) == 10)
  expect_true(all(new$age == 0))
})
