
test_that("a simple simulation works", {

  people <- list(
    list(name = "A", due_date = NA, current_mate = NA, female = TRUE, age_today = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "B", due_date = NA, current_mate = NA, female = TRUE, age_today = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "C", due_date = NA, current_mate = NA, female = TRUE, age_today = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_silent({
    day <- 1
    people %>%
      increment_age() %>%
      select_fatalities(day = day) %>%
      select_emigrants(day = day) %>%
      select_mates() %>%
      select_conceptions(day = day) %>%
      add_offspring(day = day) %>%
      add_immigrants(day = day) -> people
  })

  expect_silent({
    for (day in 2:200) {
      people %>%
        increment_age() %>%
        select_fatalities(day = day) %>%
        select_emigrants(day = day) %>%
        select_mates() %>%
        select_conceptions(day = day) %>%
        add_offspring(day = day) %>%
        add_immigrants(day = day) -> people
    }
  })

})