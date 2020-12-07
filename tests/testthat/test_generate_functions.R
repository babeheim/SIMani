
test_that("generate_person works", {

  pop1 <- data.frame(
    father = integer(0),
    mother = integer(0),
    due_date = integer(0),
    current_mate = integer(0),
    female = logical(0),
    age = integer(0),
    date_of_birth = integer(0),
    date_of_death = integer(0),
    is_present = logical(0),
    is_alive = logical(0)
  )

  person <- generate_person(pop1)
  expect_true(length(person) == ncol(pop1))

  pop2 <- data.frame(
    female = logical(0),
    age = integer(0),
    is_present = logical(0),
    is_alive = logical(0)
  )
  person <- generate_person(pop2)
  expect_true(length(person) == ncol(pop2))

  pop2 <- data.frame(
    female = logical(0),
    age = integer(0),
    is_present = logical(0),
    is_alive = logical(0)
  )
  person <- generate_person(pop2, calc_age_offspring)
  expect_true(length(person) == ncol(pop2))
  expect_true(person$age == 0)

})

test_that("generate_people works", {

  test <- list(
    list(age = 1, is_alive = TRUE, is_present = TRUE),
    list(age = 2, is_alive = TRUE, is_present = TRUE),
    list(age = 1, is_alive = FALSE, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame()

  add <- generate_people(100, test, calc_age = calc_age_basic)
  expect_true(nrow(add) == 100)
  expect_true(ncol(add) == 4)

  test2 <- list(
    list(age = 1, is_alive = TRUE, female = TRUE, is_present = TRUE),
    list(age = 2, is_alive = TRUE, female = TRUE, is_present = TRUE),
    list(age = 1, is_alive = FALSE, female = TRUE, is_present = TRUE)
  ) %>% bind_rows() %>% as.data.frame()
  
  add2 <- generate_people(100, test2, calc_age = calc_age_basic)
  expect_true(nrow(add2) == 100)
  expect_true(ncol(add2) == 4)

  add3 <- generate_people(100, test2, calc_age = calc_age_offspring)
  expect_true(nrow(add3) == 100)
  expect_true(ncol(add3) == 4)
  expect_true(all(add3$age == 0))

})


test_that("generate_population works", {
  new <- generate_population(1, calc_age = calc_age_basic)
  expect_true(nrow(new) == 1)
  new <- generate_population(10, calc_age = calc_age_basic)
  expect_true(nrow(new) == 10)
  new <- generate_population(1000, calc_age = calc_age_basic)
  expect_true(nrow(new) == 1000)
  new <- generate_population(1000, calc_age = calc_age_usa)
  expect_true(nrow(new) == 1000)
  new <- generate_population(10, calc_age = calc_age_offspring)
  expect_true(nrow(new) == 10)
  expect_true(all(new$age == 0))
})
