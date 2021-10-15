

# need some update_trait functions...

test_that("update_age function works", {

  test <- list(
    list(age = 1, is_alive = TRUE),
    list(age = 2, is_alive = TRUE),
    list(age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test <- update_age(test, tic_length = 365)
  expect_true(test$age[1] == 2)
  expect_true(test$age[2] == 3)
  expect_true(test$age[3] == 1)

  for (i in 1:(365)) {
    test <- update_age(test, tic_length = 1)
  }
  expect_true(near(test$age[1], 3))
  expect_true(near(test$age[2], 4))
  expect_true(test$age[3] == 1)

  test <- update_age(test, tic_length = 365 * 10)
  expect_true(near(test$age[1], 13))
  expect_true(near(test$age[2], 14))
  expect_true(test$age[3] == 1)

  # all dead people? no one gets updated

  test2 <- list(
    list(age = 1, is_alive = FALSE),
    list(age = 2, is_alive = FALSE),
    list(age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test2 <- update_age(test2, tic_length = 365)
  expect_true(test2$age[1] == 1)
  expect_true(test2$age[2] == 2)
  expect_true(test2$age[3] == 1)

  test3 <- list(
    list(age = 1, is_alive = FALSE),
    list(age = 2, is_alive = FALSE),
    list(age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  for (i in 1:(365)) {
    test3 <- update_age(test3, tic_length = 1)
  }
  expect_true(test3$age[1] == 1)
  expect_true(test3$age[2] == 2)
  expect_true(test3$age[3] == 1)

})
