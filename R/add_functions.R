
add_immigrants <- function(ppl, current_tic, n_immigrants = 0,
  calc_age = calc_age_basic, tic_length = 1) {
  # choose n_immigrants probabilistically
  n_immigrants <- n_immigrants + rpois(1, 3)
  if (n_immigrants > 0) {
    immigrants <- generate_ppl(n_immigrants, ppl, calc_age)
    for (i in 1:n_immigrants) {
      immigrants$date_of_birth[i] <- current_tic * (tic_length / 365) - immigrants$age[i]
      # age and date_of_birth are always in years
    }
    ppl <- bind_rows(ppl, immigrants)
  }
  return(ppl)
}

add_offspring <- function(ppl, current_tic,
  calc_age = calc_age_offspring, tic_length = 1, ...) {

  reproducing_women <- which(ppl$to_reproduce & ppl$female)
  if (length(reproducing_women) > 0) {
    # currently one birth per woman
    births <- generate_ppl(length(reproducing_women), ppl, calc_age)
    births$date_of_birth <- current_tic * (tic_length / 365) - births$age
    births$mother <- reproducing_women
    births$father <- ppl$current_partner[reproducing_women]
    ppl <- bind_rows(ppl, births)
  }

  remaining_reproducing_men <- which(ppl$to_reproduce & !ppl$female & is.na(ppl$current_partner))
  if (length(remaining_reproducing_men) > 0) {
    # currently one birth per man
    births <- generate_ppl(length(remaining_reproducing_men), ppl, calc_age) 
    births$date_of_birth <- current_tic * (tic_length / 365) - births$age
    births$mother <- NA
    births$father <- remaining_reproducing_men
    ppl <- bind_rows(ppl, births)
  }

  ppl$to_reproduce <- FALSE

  return(ppl)
}
