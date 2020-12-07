
generate_person <- function(people, calc_age) {
  new_person <- vector("list", ncol(people))
  names(new_person) <- names(people)
  for (i in seq_along(new_person)) new_person[[i]] <- NA
  new_person$is_alive <- TRUE
  new_person$is_present <- TRUE
  new_person$female <- rbinom(1, 1, 0.5)
  new_person$age <- calc_age(1, new_person)
  return(new_person)
}

generate_people <- function(n_new, people, calc_age) {
  new_people <- vector("list", n_new)
  for (i in 1:n_new) {
    new_people[[i]] <- generate_person(people, calc_age)
  }
  new_people %>% bind_rows() %>% as.data.frame() -> new_people
  return(new_people)
}

generate_population <- function(n, calc_age) {
  people <- data.frame(
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
  output <- generate_people(n, people, calc_age)
  return(output)
}

add_immigrants <- function(people, current_tic, n_immigrants = 0, calc_age = calc_age_simple) {
  # choose n_immigrants probabilistically
  n_immigrants <- n_immigrants + rpois(1, 3)
  if (n_immigrants > 0) {
    immigrants <- generate_people(n_immigrants, people, calc_age)
    for (i in 1:n_immigrants) {
      immigrants$date_of_birth[i] <- current_tic - immigrants$age[i]
    }
    people <- bind_rows(people, immigrants)
  }
  return(people)
}

add_offspring <- function(people, current_tic, calc_age = calc_age_offspring) {
  women_in_labor <- which(people$due_date == current_tic)
  # currently one birth per woman
  n_births <- sum(people$due_date == current_tic, na.rm = TRUE)
  if (n_births > 0) {
    births <- generate_people(n_births, people, calc_age)
    for (i in 1:n_births) {
      births$mother[i] <- women_in_labor[i]
      births$father[i] <- people$current_mate[women_in_labor[i]]
      births$date_of_birth[i] <- current_tic
    }
    people <- bind_rows(people, births)
  }
  return(people)
}
