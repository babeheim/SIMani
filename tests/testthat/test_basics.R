
test_that("increment_age function works", {

  test <- list(
    list(name = "A", age_today = 1, is_alive = TRUE),
    list(name = "B", age_today = 2, is_alive = TRUE),
    list(name = "C", age_today = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test <- increment_age(test)
  expect_true(test$age_today[test$name == "A"] == 2)
  expect_true(test$age_today[test$name == "B"] == 3)
  expect_true(test$age_today[test$name == "C"] == 1)

  test2 <- list(
    list(name = "A", age_today = 1, is_alive = FALSE),
    list(name = "B", age_today = 2, is_alive = FALSE),
    list(name = "C", age_today = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test2 <- increment_age(test2)
  expect_true(test2$age_today[test2$name == "A"] == 1)
  expect_true(test2$age_today[test2$name == "B"] == 2)
  expect_true(test2$age_today[test2$name == "C"] == 1)

})


test_that("add_immigrants works", {

  day <- 1
  test <- list(
    list(name = "A", age_today = 1, is_alive = TRUE),
    list(name = "B", age_today = 2, is_alive = TRUE),
    list(name = "C", age_today = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test <- add_immigrants(test, day, 3)
  expect_true(nrow(test) == 6)

  day <- 1
  test2 <- list(
    list(name = "A", age_today = 1, is_alive = TRUE),
    list(name = "B", age_today = 2, is_alive = TRUE),
    list(name = "C", age_today = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test2 <- add_immigrants(test2, day)
  expect_true(nrow(test2) > 3)

})


test_that("select_emigrants works", {

  day <- 1
  test <- list(
    list(name = "A", age_today = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "B", age_today = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "C", age_today = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(select_emigrants(test, 1, 3))
  test <- select_emigrants(test, 1, 1)
  expect_false(test$is_present[1])
  test <- select_emigrants(test, 1, 2)
  expect_false(test$is_present[2])
})


test_that("select_fatalities works", {
  day <- 1
  test <- list(
    list(name = "A", age_today = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "B", age_today = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "C", age_today = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(select_fatalities(test, 1, 3))
  test <- select_fatalities(test, 1, 1)
  expect_false(test$is_alive[1])
  test <- select_fatalities(test, 1, 2)
  expect_false(test$is_alive[2])
})

