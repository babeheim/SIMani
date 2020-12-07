
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
  interval <- 10
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