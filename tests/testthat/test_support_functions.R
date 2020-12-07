
test_that("sample_safe works correctly", {
  expect_true(all(sample_safe(c(100), 1000, replace = TRUE) == 100))
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
