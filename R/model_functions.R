
# sample() when only one item in the bag
sample_safe <- function(x, ...) {
  x[sample.int(length(x), ...)]
}

inspect_people <- function(people) {
  if (!"age" %in% names(people)) stop("age does not exist")
  if (!"date_of_birth" %in% names(people)) stop("dob does not exist")
  if (!"female" %in% names(people)) stop("female does not exist")

  if (!"father" %in% names(people)) warning("father does not exist")
  if (!"mother" %in% names(people)) warning("mother does not exist")
  if (!"current_mate" %in% names(people)) warning("dod does not exist")
  if (!"due_date" %in% names(people)) warning("due_date does not exist")

  if (!"is_alive" %in% names(people)) warning("is_alive does not exist")
  if (!"is_present" %in% names(people)) warning("is_present does not exist")
  if (!"date_of_death" %in% names(people)) warning("dod does not exist")

  if (any(is.na(people$date_of_birth))) stop("date_of_birth has missing values")
  if (any(is.na(people$age))) stop("age has missing values")
  if (any(is.na(people$is_alive))) stop("is_alive has missing values")
}

increment_age <- function(people, days_per_tic = 365) {
  if (!"age" %in% names(people)) stop("age does not exist")
  if (any(is.na(people$age))) stop("age has missing values")
  if (!"is_alive" %in% names(people)) stop("is_alive does not exist")
  if (any(is.na(people$is_alive))) stop("is_alive has missing values")
  if (any(people$is_alive)) {
    people$age[people$is_alive] <- people$age[people$is_alive] + days_per_tic/365
  }
  return(people)
}

calc_emigration_basic <- function(active, people) {
  baseline <- 0.001
  alpha <- log(baseline/(1 - baseline))
  # linear model goes here
  log_odds_death <- alpha
  return(log_odds_death)
}

select_emigrants <- function(people, manual = NA, emi_fun = calc_emigration_basic) {
  active_people <- which(people$is_alive & people$is_present)
  if (length(active_people) > 0) {
    emigrants <- integer(0)
    # specify manual emigrations for testing
    if (!all(is.na(manual))) {
      if (!all(manual %in% active_people)) stop("only active people can emigrate")
      emigrants <- c(emigrants, manual)
    }
    # choose emigrants probabilistically
    logit_pr_emigrate <- emi_fun(active_people, people)
    emigrated <- rbinom(length(active_people), 1, logistic(logit_pr_emigrate))
    emigrants <- c(emigrants, active_people[emigrated]) 
    # now update people table based on emigration decisions
    if (length(emigrants) > 0) {
      people$is_present[emigrants] <- FALSE
      # currently emigrants dont go with relatives, but we can change
      people$current_mate[emigrants] <- NA
      people$current_mate[which(people$current_mate %in% emigrants)] <- NA
    }
  }
  return(people)
}

calc_mortality_basic <- function(women, people) {
  baseline <- 0.001
  alpha <- log(baseline / (1 - baseline))
  # linear model goes here
  log_odds_concieve <- alpha
  return(log_odds_concieve)
}

select_fatalities <- function(people, current_tic, manual = NA, calc_mortality = calc_mortality_basic) {
  active_people <- which(people$is_alive & people$is_present)
  if (length(active_people) > 0) {
    fatalities <- integer(0)
    # specify manual fatalities for testing
    if (!all(is.na(manual))) {
      if (!all(manual %in% active_people)) stop("only active people can die")
      fatalities <- c(fatalities, manual)
    }
    logit_pr_die <- calc_mortality(active_people, people)
    died <- as.logical(rbinom(length(active_people), 1, logistic(logit_pr_die)))
    fatalities <- c(fatalities, active_people[died])
    # add additional deaths to test
    if (length(fatalities) > 0) {
      people$date_of_death[fatalities] <- current_tic
      people$is_alive[fatalities] <- FALSE
      people$current_mate[fatalities] <- NA
      people$current_mate[which(people$current_mate %in% fatalities)] <- NA
    }
  }
  return(people)
}

select_mates <- function(people) {
  # in this version, women choose men
  available_women <- which(is.na(people$current_mate) &
    people$age >= 15 & people$female & people$is_present)
  # random reindex to avoid any kind of order bias in mating
  available_women <- available_women[sample(length(available_women))]
  available_men <- which(is.na(people$current_mate) &
    people$age >= 15 & !people$female & people$is_present)
  if (length(available_women) > 0 & length(available_men) > 0) {
    for (i in seq_along(available_women)) {
      if (length(available_men) > 0) {
        focal_woman <- available_women[i]
        pr_choose_man <- rep(1, length(available_men))
        chosen_man <- sample_safe(available_men, 1, prob = pr_choose_man)
        people$current_mate[focal_woman] <- chosen_man
        people$current_mate[chosen_man] <- focal_woman
        available_men <- setdiff(available_men, chosen_man)
      }
    }
  }
  return(people)
}

calc_conception_basic <- function(women, people) {
  baseline <- 0.001
  alpha <- log(baseline / (1 - baseline))
  # linear model goes here
  log_odds_concieve <- alpha
  return(log_odds_concieve)
}

logistic <- function (x) {
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  return(p)
}

select_conceptions <- function(people, current_tic, manual = NA, calc_conception = calc_conception_basic) {
  candidate_women <- which(people$female & people$age <= 45 &
    people$age >= 15 & !is.na(people$current_mate))
  if (length(candidate_women) > 0) {
    conceptions <- integer(0)
    # specify manual conceptions for testing
    if (!all(is.na(manual))) {
      if (!all(manual %in% active_people)) stop("only active people can die")
      conceptions <- c(conceptions, manual)
    }
    # select conceptions probabilistically
    logit_pr_concieve <- calc_conception(candidate_women, people)
    concieved <- rbinom(length(candidate_women), 1, logistic(logit_pr_concieve))
    conceptions <- c(conceptions, candidate_women[concieved])
    # update people table based on above
    if (length(conceptions) > 0) {
      people$due_date[conceptions] <- current_tic + 30 * 9 # assumes one day per tic
    }
  }
  return(people)
}

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
    # update people table
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
    # update people table
    people <- bind_rows(people, births)
  }
  return(people)
}

update_trait <- function(people, trait, rule) {
  return(people)
}

record_census <- function(censuses, people, current_tic, interval = 365) {
  if (current_tic %% interval == 0) {
    census_number <- floor(current_tic/interval)
    people$current_tic <- current_tic
    censuses[[census_number]] <- people
  }
  return(censuses)
}
