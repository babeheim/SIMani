
generate_person <- function(ppl, calc_age = calc_age_basic) {
  new_person <- vector("list", ncol(ppl))
  names(new_person) <- names(ppl)
  for (i in seq_along(new_person)) new_person[[i]] <- NA
  new_person$is_alive <- TRUE
  new_person$is_present <- TRUE
  new_person$to_reproduce <- FALSE
  new_person$female <- sample(c(TRUE, FALSE), 1, prob = c(50, 50))
  new_person$age <- calc_age(1, new_person) # in years
  return(new_person)
}

generate_ppl <- function(n_new, ppl, calc_age) {
  new_ppl <- vector("list", n_new)
  for (i in 1:n_new) {
    new_ppl[[i]] <- generate_person(ppl, calc_age)
  }
  new_ppl %>% bind_rows() %>% as.data.frame() -> new_ppl
  return(new_ppl)
}

generate_population <- function(n, calc_age = calc_age_basic) {
  ppl <- data.frame(
    father = integer(0),
    mother = integer(0),
    current_partner = integer(0),
    female = logical(0),
    age = integer(0),
    date_of_birth = integer(0),
    date_of_death = integer(0),
    is_present = logical(0),
    is_alive = logical(0)
  )
  output <- generate_ppl(n, ppl, calc_age)
  output$date_of_birth <- 0 - output$age # initialized at 'year 0'
  return(output)
}
