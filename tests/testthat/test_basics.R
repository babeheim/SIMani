
test_that("sample_safe works correctly", {
  expect_true(all(sample_safe(c(100), 1000, replace = TRUE) == 100))
})

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
  tic <- 1
  test <- list(
    list(name = "A", age = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "B", age = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "C", age = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(select_fatalities(test, 1, 3))
  test <- select_fatalities(test, 1, 1)
  expect_false(test$is_alive[1])
  test <- select_fatalities(test, 1, 2)
  expect_false(test$is_alive[2])
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


test_that("generate_new_people works", {

  test <- list(
    list(age = 1, is_alive = TRUE, is_present = TRUE),
    list(age = 2, is_alive = TRUE, is_present = TRUE),
    list(age = 1, is_alive = FALSE, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame()

  add <- generate_new_people(100, test)
  expect_true(nrow(add) == 100)
  expect_true(ncol(add) == 4)

  test2 <- list(
    list(age = 1, is_alive = TRUE, female = TRUE, is_present = TRUE),
    list(age = 2, is_alive = TRUE, female = TRUE, is_present = TRUE),
    list(age = 1, is_alive = FALSE, female = TRUE, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame()
  
  add2 <- generate_new_people(100, test2)
  expect_true(nrow(add2) == 100)
  expect_true(ncol(add2) == 4)

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



test_that("inspect_people works", {

  test <- list(
    list(father = NA, mother = NA, name = "A", due_date = NA, current_mate = NA, female = TRUE, age = 1, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, name = "B", due_date = NA, current_mate = NA, female = TRUE, age = 2, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, name = "C", due_date = NA, current_mate = NA, female = TRUE, age = 1, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_silent(inspect_people(test))

  test <- list(
    list(father = NA, mother = NA, name = "A", due_date = NA, female = TRUE, age = 1, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, name = "B", due_date = NA, female = TRUE, age = 2, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, name = "C", due_date = NA, female = TRUE, age = 1, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_warning(inspect_people(test))

  test <- list(
    list(father = NA, mother = NA, name = "A", due_date = NA, current_mate = NA, female = TRUE, date_of_birth = -5, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, name = "B", due_date = NA, current_mate = NA, female = TRUE, date_of_birth = -3, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, name = "C", due_date = NA, current_mate = NA, female = TRUE, date_of_birth = -3, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(inspect_people(test))

})
