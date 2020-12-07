
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
