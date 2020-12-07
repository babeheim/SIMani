

test_that("simulations with specific processes disabled still work", {

  expect_silent({
    pop <- generate_population(n = 100, calc_age_usa)
    tic_length <- 365 # each tic increments a year
    n_tics <- 100
    for (i in 1:n_tics) {
      pop %>%
        increment_age() %>%
        select_fatalities(current_tic = i, calc_mortality = calc_mortality_usa, tic_length = tic_length) %>%
        select_emigrants() %>%
        select_mates() %>%
        select_conceptions(current_tic = i, calc_fertility = calc_fertility_usa, tic_length = tic_length) %>%
        add_offspring(current_tic = i, tic_length = tic_length) %>%
        add_immigrants(current_tic = i, tic_length = tic_length) %>%
        identity() -> pop
    }
  })

})



test_that("realistic simulations work", {

  expect_silent({
    pop <- generate_population(n = 100, calc_age_usa)
    inspect_people(pop)
    tic_length <- 1 # each tic increments one day
    n_tics <- 365
    for (i in 1:n_tics) {
      pop %>%
        increment_age(tic_length = tic_length) %>%
        select_fatalities(current_tic = i, calc_mortality = calc_mortality_usa, tic_length = tic_length) %>%
        select_emigrants() %>%
        select_mates() %>%
        select_conceptions(current_tic = i, calc_fertility = calc_fertility_usa, tic_length = tic_length) %>%
        add_offspring(current_tic = i, tic_length = tic_length) %>%
        add_immigrants(current_tic = i, tic_length = tic_length) %>%
        identity() -> pop
    }
  })

})

test_that("one tic works", {

  today <- 1

  people <- list(
    list(father = NA, mother = NA, due_date = NA, current_mate = NA, female = TRUE, age = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, current_mate = NA, female = TRUE, age = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, current_mate = NA, female = TRUE, age = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  people$date_of_birth <- today - people$age

  expect_silent(inspect_people(people))

  expect_silent({
    people %>%
      increment_age() %>%
      select_fatalities(current_tic = today) %>%
      select_emigrants() %>%
      select_mates() %>%
      select_conceptions(current_tic = today) %>%
      add_offspring(current_tic = today) %>%
      add_immigrants(current_tic = today) %>%
      identity() -> people
  })

})


test_that("two hundred tics work", {

  today <- 1

  people <- list(
    list(father = NA, mother = NA, due_date = NA, current_mate = NA, female = TRUE, age = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, current_mate = NA, female = TRUE, age = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, current_mate = NA, female = TRUE, age = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  people$date_of_birth <- today - people$age

  expect_silent(inspect_people(people))

  expect_silent({
    for (today in 1:200) {
      people %>%
        increment_age() %>%
        select_fatalities(current_tic = today) %>%
        select_emigrants() %>%
        select_mates() %>%
        select_conceptions(current_tic = today) %>%
        add_offspring(current_tic = today) %>%
        add_immigrants(current_tic = today) %>%
        identity() -> people
    }
  })

})

test_that("two hundred tics with recording work", {

  today <- 1

  people <- list(
    list(father = NA, mother = NA, due_date = NA, current_mate = NA, female = TRUE, age = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, current_mate = NA, female = TRUE, age = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, current_mate = NA, female = TRUE, age = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  people$date_of_birth <- today - people$age

  expect_silent(inspect_people(people))

  n_tics <- 200
  interval <- 10 # measured in tics between each census
  censuses <- vector("list", floor(n_tics/interval))

  expect_silent({
    for (today in 1:n_tics) {
      people %>%
        increment_age() %>%
        select_fatalities(current_tic = today) %>%
        select_emigrants() %>%
        select_mates() %>%
        select_conceptions(current_tic = today) %>%
        add_offspring(current_tic = today) %>%
        add_immigrants(current_tic = today) %>%
        identity() -> people

      censuses %>% record_census(people, today, interval) -> censuses
    }
  })

  expect_true(class(censuses[[20]]) == "data.frame")

})