
test_that("select_conceptions works", {

  test <- list(
    list(current_mate = 4, female = TRUE, age = 15, due_date = NA),
    list(current_mate = 5, female = TRUE, age = 15, due_date = NA),
    list(current_mate = 6, female = TRUE, age = 15, due_date = NA),
    list(current_mate = 1, female = FALSE, age = 15),
    list(current_mate = 2, female = FALSE, age = 15),
    list(current_mate = 3, female = FALSE, age = 15)
  ) %>% bind_rows() %>% as.data.frame()

  # manual tests
  test <- select_conceptions(test, 1, 1)
  expect_true(!is.na(test$due_date[1]))
  expect_error(select_conceptions(test, 1, 1))
  test <- select_conceptions(test, 1, c(2, 3))
  expect_true(!any(is.na(test$due_date[2:3])))
  expect_error(select_conceptions(test, 1, c(4, 5, 6)))

  # using different calc_conception function

  test <- list(
    list(current_mate = 4, female = TRUE, age = 15, due_date = NA),
    list(current_mate = 5, female = TRUE, age = 15, due_date = NA),
    list(current_mate = 6, female = TRUE, age = 15, due_date = NA),
    list(current_mate = 1, female = FALSE, age = 15),
    list(current_mate = 2, female = FALSE, age = 15),
    list(current_mate = 3, female = FALSE, age = 15)
  ) %>% bind_rows() %>% as.data.frame()

})



test_that("select_emigrants works", {

  test <- list(
    list(name = "A", age = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "B", age = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "C", age = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(select_emigrants(test, 3))
  test <- select_emigrants(test, 1)
  expect_false(test$is_present[1])
  test <- select_emigrants(test, 2)
  expect_false(test$is_present[2])
})


test_that("select_fatalities works", {

  # test manual
  test <- list(
    list(name = "A", age = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "B", age = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "C", age = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(select_fatalities(test, current_tic = 1, manual = 3))
  test <- select_fatalities(test, current_tic = 1, manual = 1)
  expect_false(test$is_alive[1])
  test <- select_fatalities(test, current_tic = 1, manual = 2)
  expect_false(test$is_alive[2])

  expect_silent(select_fatalities(test, current_tic = 1, calc_mortality = calc_mortality_usa))
  expect_silent(select_fatalities(test, current_tic = 1, calc_mortality = calc_mortality_usa, tic_length = 365))

})



test_that("select_mates works", {

  test <- list(
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_mates()
  expect_true(!any(is.na(test$current_mate)))

  test2 <- list(
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_mates()
  expect_true(sum(is.na(test2$current_mate)) == 2)

  test3 <- list(
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_mates()
  expect_true(sum(is.na(test3$current_mate)) == 2)

  test4 <- list(
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 14, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 14, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_mates()
  expect_true(sum(is.na(test4$current_mate)) == 4)
  expect_true(is.na(test4$current_mate[2]))
  expect_true(is.na(test4$current_mate[3]))

  test5 <- list(
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 14, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 14, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_mates()
  expect_true(sum(is.na(test5$current_mate)) == 4)
  expect_true(is.na(test5$current_mate[4]))
  expect_true(is.na(test5$current_mate[5]))

  test6 <- list(
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 14, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 14, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 14, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_mates()
  expect_true(all(is.na(test6$current_mate)))

  test7 <- list(
    list(current_mate = NA, female = TRUE, age = 14, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 14, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 14, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_mates()
  expect_true(all(is.na(test7$current_mate)))

  test8 <- list(
    list(current_mate = 4, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = TRUE, age = 15, is_present = TRUE),
    list(current_mate = 1, female = FALSE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE),
    list(current_mate = NA, female = FALSE, age = 15, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_mates()
  expect_true(!any(is.na(test8$current_mate)))
  expect_true(test8$current_mate[1] == 4)
  expect_true(test8$current_mate[4] == 1)

})
