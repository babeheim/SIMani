
test_that("increment_age function works", {

  test <- list(
    list(age = 1, is_alive = TRUE),
    list(age = 2, is_alive = TRUE),
    list(age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test <- increment_age(test)
  expect_true(test$age[1] == 2)
  expect_true(test$age[2] == 3)
  expect_true(test$age[3] == 1)

  test2 <- list(
    list(age = 1, is_alive = FALSE),
    list(age = 2, is_alive = FALSE),
    list(age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test2 <- increment_age(test2)
  expect_true(test2$age[1] == 1)
  expect_true(test2$age[2] == 2)
  expect_true(test2$age[3] == 1)

})