
test_that("add_immigrants works", {

  test <- list(
    list(name = "A", age = 15, is_alive = TRUE),
    list(name = "B", age = 20, is_alive = TRUE),
    list(name = "C", age = 12, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test <- add_immigrants(test, current_tic = 1, n_immigrants = 3, tic_length = 365)
  expect_true(nrow(test) >= 6)

  tic <- 1
  test2 <- list(
    list(name = "A", age = 15, is_alive = TRUE),
    list(name = "B", age = 20, is_alive = TRUE),
    list(name = "C", age = 12, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test2 <- add_immigrants(test2, current_tic = 1, n_immigrants = 1, tic_length = 365)
  expect_true(nrow(test2) > 3)

})

test_that("add_offspring works", {

  test <- list(
    list(female = FALSE, mother = NA, father = NA),
    list(female = FALSE, mother = NA, father = NA),
    list(female = TRUE, due_date = 1, current_mate = 1, mother = NA, father = NA),
    list(female = TRUE, due_date = 1, current_mate = 2, mother = NA, father = NA)
  ) %>% bind_rows() %>% as.data.frame()

  test <- add_offspring(test, current_tic = 1, tic_length = 365)
  expect_true(nrow(test) == 6)

})
