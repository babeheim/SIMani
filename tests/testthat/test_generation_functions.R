
test_that("add_immigrants works", {

  tic <- 1
  test <- list(
    list(name = "A", age = 1, is_alive = TRUE),
    list(name = "B", age = 2, is_alive = TRUE),
    list(name = "C", age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test <- add_immigrants(test, tic, 3)
  expect_true(nrow(test) >= 6)

  tic <- 1
  test2 <- list(
    list(name = "A", age = 1, is_alive = TRUE),
    list(name = "B", age = 2, is_alive = TRUE),
    list(name = "C", age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test2 <- add_immigrants(test2, tic, 1)
  expect_true(nrow(test2) > 3)

})

test_that("add_offspring works", {

  tic <- 1
  test <- list(
    list(female = FALSE, mother = NA, father = NA),
    list(female = FALSE, mother = NA, father = NA),
    list(female = TRUE, due_date = 1, current_mate = 1, mother = NA, father = NA),
    list(female = TRUE, due_date = 1, current_mate = 2, mother = NA, father = NA)
  ) %>% bind_rows() %>% as.data.frame()

  test <- add_offspring(test, tic)
  expect_true(nrow(test) == 6)

})


test_that("generate_people works", {

  test <- list(
    list(age = 1, is_alive = TRUE, is_present = TRUE),
    list(age = 2, is_alive = TRUE, is_present = TRUE),
    list(age = 1, is_alive = FALSE, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame()

  add <- generate_people(100, test, calc_age = calc_age_simple)
  expect_true(nrow(add) == 100)
  expect_true(ncol(add) == 4)

  test2 <- list(
    list(age = 1, is_alive = TRUE, female = TRUE, is_present = TRUE),
    list(age = 2, is_alive = TRUE, female = TRUE, is_present = TRUE),
    list(age = 1, is_alive = FALSE, female = TRUE, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame()
  
  add2 <- generate_people(100, test2, calc_age = calc_age_simple)
  expect_true(nrow(add2) == 100)
  expect_true(ncol(add2) == 4)

})


test_that("generate_population works", {
  new <- generate_population(1, calc_age = calc_age_simple)
  expect_true(nrow(new) == 1)
  new <- generate_population(10, calc_age = calc_age_simple)
  expect_true(nrow(new) == 10)
  new <- generate_population(1000, calc_age = calc_age_simple)
  expect_true(nrow(new) == 1000)
  new <- generate_population(10, calc_age = calc_age_offspring)
  expect_true(nrow(new) == 10)
  expect_true(all(new$age == 0))
})
