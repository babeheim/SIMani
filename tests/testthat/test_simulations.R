
test_that("a simple simulation works", {

  people <- list(
    list(name = "A", current_mate = NA, female = TRUE, age_today = 1, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "B", current_mate = NA, female = TRUE, age_today = 2, date_of_death = NA, is_alive = TRUE, is_present = TRUE),
    list(name = "C", current_mate = NA, female = TRUE, age_today = 1, date_of_death = 0, is_alive = FALSE, is_present = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  expect_silent({
    day <- 1
    people %>%
      increment_age() %>%
      select_fatalities(day = day) %>%
      select_emigrants(day = day) %>%
      select_mates() %>%
      # concieve_offspring() %>%
      # add_offspring %>%
      add_immigrants(day = day) -> people
  })

  expect_silent({
    for (day in 2:200) {
      people %>%
        increment_age() %>%
        select_fatalities(day = day) %>%
        select_emigrants(day = day) %>%
        select_mates() %>%
      # concieve_offspring() %>%
      # add_offspring %>%
        add_immigrants(day = day) -> people
    }
  })

})