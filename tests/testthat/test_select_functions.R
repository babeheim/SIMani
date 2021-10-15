
test_that("select_reproducers works", {

  test <- list(
    list(female = TRUE, age = 15, to_reproduce = FALSE, is_alive = TRUE, is_present = TRUE),
    list(female = TRUE, age = 15, to_reproduce = FALSE, is_alive = TRUE, is_present = TRUE),
    list(female = TRUE, age = 15, to_reproduce = FALSE, is_alive = TRUE, is_present = TRUE),
    list(female = FALSE, age = 15, to_reproduce = FALSE, is_alive = TRUE, is_present = TRUE),
    list(female = FALSE, age = 15, to_reproduce = FALSE, is_alive = TRUE, is_present = TRUE),
    list(female = FALSE, age = 95, to_reproduce = FALSE, is_alive = TRUE, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame()

  # manual tests
  test <- select_reproducers(test, current_tic = 1, manual = 1, calc_fertility = calc_fertility_basic)
  expect_true(test$to_reproduce[1])
  expect_error(select_reproducers(test, 1, 6)) # the 95-year-old can't reproduce
  test <- select_reproducers(test, 1, c(2, 3), calc_fertility = calc_fertility_basic)
  expect_true(all(test$to_reproduce[2:3]))
  expect_error(select_reproducers(test, 1, c(4, 5, 6))) # the 95-year-old can't reproduce

})



test_that("select_emigrants works", {

  test <- list(
    list(name = "A", age = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "B", age = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "C", age = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(select_emigrants(test, 3, calc_emigration = calc_emigration_basic))
  test <- select_emigrants(test, 1)
  expect_false(test$is_present[1])
  test <- select_emigrants(test, 2, calc_emigration = calc_emigration_basic)
  expect_false(test$is_present[2])
})


test_that("select_fatalities works", {

  # test manual
  test <- list(
    list(name = "A", age = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "B", age = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "C", age = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_error(select_fatalities(test, current_tic = 1, manual = 3, calc_mortality = calc_mortality_basic))
  test <- select_fatalities(test, current_tic = 1, manual = 1, calc_mortality = calc_mortality_basic)
  expect_false(test$is_alive[1])
  test <- select_fatalities(test, current_tic = 1, manual = 2, calc_mortality = calc_mortality_basic)
  expect_false(test$is_alive[2])

  expect_silent(select_fatalities(test, current_tic = 1, calc_mortality = calc_mortality_usa))
  expect_silent(select_fatalities(test, current_tic = 1, calc_mortality = calc_mortality_usa, tic_length = 365))

})



test_that("select_partners works", {

  test <- list(
    list(current_partner = NA, female = TRUE, to_reproduce = TRUE),
    list(current_partner = NA, female = TRUE, to_reproduce = TRUE),
    list(current_partner = NA, female = TRUE, to_reproduce = TRUE),
    list(current_partner = NA, female = FALSE, to_reproduce = TRUE),
    list(current_partner = NA, female = FALSE, to_reproduce = TRUE),
    list(current_partner = NA, female = FALSE, to_reproduce = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_partners(calc_dyad_score = calc_dyad_score_random)
  expect_true(!any(is.na(test$current_partner)))
  expect_true(all(test$current_partner[test$current_partner] == 1:nrow(test)))
  
  test <- list(
    list(current_partner = NA, female = TRUE, age = 15, to_reproduce = TRUE),
    list(current_partner = NA, female = TRUE, age = 35, to_reproduce = TRUE),
    list(current_partner = NA, female = TRUE, age = 35, to_reproduce = TRUE),
    list(current_partner = NA, female = FALSE, age = 15, to_reproduce = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_partners(calc_dyad_score = calc_dyad_score_age_hist)
  expect_true(sum(is.na(test$current_partner)) == 2)
  expect_true(test$current_partner[1] == 4)
  expect_true(test$current_partner[4] == 1)

  test <- list(
    list(current_partner = NA, female = TRUE, age = 45, to_reproduce = TRUE),
    list(current_partner = NA, female = FALSE, age = 15, to_reproduce = TRUE)
  ) %>% bind_rows() %>% as.data.frame() %>% select_partners(calc_dyad_score = calc_dyad_score_age_hist)
  expect_true(sum(is.na(test$current_partner)) == 0)

})
