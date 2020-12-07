
add_immigrants <- function(people, current_tic, n_immigrants = 0,
  calc_age = calc_age_basic, tic_length = 1) {
  # choose n_immigrants probabilistically
  n_immigrants <- n_immigrants + rpois(1, 3)
  if (n_immigrants > 0) {
    immigrants <- generate_people(n_immigrants, people, calc_age)
    for (i in 1:n_immigrants) {
      immigrants$date_of_birth[i] <- current_tic * (tic_length / 365) - immigrants$age[i]
      # age and date_of_birth are always in years
    }
    people <- bind_rows(people, immigrants)
  }
  return(people)
}

add_offspring <- function(people, current_tic,
  calc_age = calc_age_offspring, tic_length = 1) {
  women_in_labor <- which(people$due_date == current_tic)
  n_births <- sum(people$due_date == current_tic, na.rm = TRUE)
  # currently one birth per woman
  if (n_births > 0) {
    births <- generate_people(n_births, people, calc_age)
    for (i in 1:n_births) {
      births$mother[i] <- women_in_labor[i]
      births$father[i] <- people$current_mate[women_in_labor[i]]
      births$date_of_birth[i] <- current_tic * (tic_length / 365) - births$age[i]
      # age and date_of_birth are always in years
      # default recruitment age for offspring is 0 but may be higher
    }
    people <- bind_rows(people, births)
  }
  people$due_date[women_in_labor] <- NA
  return(people)
}
