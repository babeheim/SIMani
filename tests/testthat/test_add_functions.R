
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
