
test_that("record_census works", {

  test <- list(
    list(age = 1, is_alive = TRUE),
    list(age = 2, is_alive = TRUE),
    list(age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  censuses <- vector("list", 2)
  censuses %>% record_census(test, current_tic = 5, interval = 5) -> censuses
  censuses %>% record_census(test, current_tic = 10, interval = 5) -> censuses
  expect_true(class(censuses[[1]]) == "data.frame")
  expect_true(class(censuses[[2]]) == "data.frame")

})

