

test_that("one tic works", {

  today <- 1

  ppl <- list(
    list(father = NA, mother = NA, due_date = NA, to_reproduce = FALSE,
      current_partner = NA, female = TRUE, age = 1, date_of_death = NA,
      is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, to_reproduce = FALSE,
      current_partner = NA, female = TRUE, age = 2, date_of_death = NA,
      is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, to_reproduce = FALSE,
      current_partner = NA, female = TRUE, age = 1, date_of_death = 0,
      is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  ppl$date_of_birth <- today - ppl$age

  expect_silent(inspect_ppl(ppl))

  expect_silent({
    ppl %>%
      update_age() %>%
        select_fatalities(current_tic = today,
          calc_mortality = calc_mortality_basic, base_rate = 0.1) %>%
      select_emigrants() %>%
      select_reproducers(current_tic = today) %>%
      select_partners() %>%
      add_offspring(current_tic = today) %>%
      add_immigrants(current_tic = today) %>%
      identity() -> ppl
  })

})




test_that("two hundred tics work", {

  today <- 1

  ppl <- list(
    list(father = NA, mother = NA, due_date = NA, to_reproduce = FALSE,
      current_partner = NA, female = TRUE, age = 1, date_of_death = NA,
      is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, to_reproduce = FALSE,
      current_partner = NA, female = TRUE, age = 2, date_of_death = NA,
      is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, to_reproduce = FALSE,
      current_partner = NA, female = TRUE, age = 1, date_of_death = 0,
      is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  ppl$date_of_birth <- today - ppl$age

  expect_silent(inspect_ppl(ppl))

  expect_silent({
    for (today in 1:200) {
      ppl %>%
        update_age() %>%
        select_fatalities(current_tic = today,
          calc_mortality = calc_mortality_basic, base_rate = 0.1) %>%
        select_emigrants() %>%
        select_reproducers(current_tic = today) %>%
        select_partners() %>%
        add_offspring(current_tic = today) %>%
        add_immigrants(current_tic = today) %>%
        identity() -> ppl
    }
  })

})


test_that("simulations with default functions work", {
  expect_silent({
    pop <- generate_population(n = 50)
    n_tics <- 100
    for (i in 1:n_tics) {
      pop %>%
        update_age() %>%
        select_fatalities(current_tic = i,
          calc_mortality = calc_mortality_basic, base_rate = 0.1) %>%
        select_emigrants() %>%
        select_reproducers(current_tic = i) %>%
        select_partners() %>%
        add_offspring(current_tic = i) %>%
        add_immigrants(current_tic = i) %>%
        identity() -> pop
    }
  })

  expect_silent({
    pop <- generate_population(n = 500)
    n_tics <- 100
    for (i in 1:n_tics) {
      pop %>%
        update_age() %>%
        select_fatalities(current_tic = i,
          calc_mortality = calc_mortality_basic, base_rate = 0.1) %>%
        select_emigrants() %>%
        select_reproducers(current_tic = i) %>%
        select_partners() %>%
        add_offspring(current_tic = i) %>%
        add_immigrants(current_tic = i) %>%
        identity() -> pop
    }
  })

  expect_silent({
    pop <- generate_population(n = 1)
    n_tics <- 100
    for (i in 1:n_tics) {
      pop %>%
        update_age() %>%
        select_fatalities(current_tic = i,
          calc_mortality = calc_mortality_basic, base_rate = 0.1) %>%
        select_emigrants() %>%
        select_reproducers(current_tic = i) %>%
        select_partners() %>%
        add_offspring(current_tic = i) %>%
        add_immigrants(current_tic = i) %>%
        identity() -> pop
    }
  })

  expect_silent({
    pop <- generate_population(n = 10000)
    n_tics <- 100
    for (i in 1:n_tics) {
      pop %>%
        update_age() %>%
        select_fatalities(current_tic = i,
          calc_mortality = calc_mortality_basic, base_rate = 0.1) %>%
        select_emigrants() %>%
        select_reproducers(current_tic = i) %>%
        select_partners() %>%
        add_offspring(current_tic = i) %>%
        add_immigrants(current_tic = i) %>%
        identity() -> pop
    }
  })

})


test_that("realistic simulations work", {

  expect_silent({
    pop <- generate_population(n = 100, calc_age_usa)
    inspect_ppl(pop)
    tic_length <- 1 # each tic updates one day
    n_tics <- 365
    for (i in 1:n_tics) {
      pop %>%
        update_age() %>%
        select_fatalities(current_tic = i, calc_mortality = calc_mortality_usa) %>%
        select_reproducers(current_tic = i, calc_fertility = calc_fertility_usa) %>%
        select_partners(calc_dyad_score = calc_dyad_score_age_hist) %>%
        add_offspring(current_tic = i) %>%
        identity() -> pop
    }
  })

})

test_that("two hundred tics with recording work", {

  today <- 1

  ppl <- list(
    list(father = NA, mother = NA, due_date = NA, current_partner = NA, to_reproduce = FALSE,
      female = TRUE, age = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, current_partner = NA, to_reproduce = FALSE,
      female = TRUE, age = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(father = NA, mother = NA, due_date = NA, current_partner = NA, to_reproduce = FALSE,
      female = TRUE, age = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  ppl$date_of_birth <- today - ppl$age

  expect_silent(inspect_ppl(ppl))

  n_tics <- 200
  interval <- 10 # measured in tics between each census
  censuses <- vector("list", floor(n_tics/interval))

  expect_silent({
    for (i in 1:n_tics) {
      ppl %>%
        update_age() %>%
        select_fatalities(current_tic = i, calc_mortality = calc_mortality_usa) %>%
        select_reproducers(current_tic = i, calc_fertility = calc_fertility_usa) %>%
        select_partners(calc_dyad_score = calc_dyad_score_age_hist) %>%
        add_offspring(current_tic = i) %>%
        identity() -> ppl
        censuses %>% record_census(ppl, i, interval) -> censuses
    }
  })

  expect_true(class(censuses[[20]]) == "data.frame")

})
