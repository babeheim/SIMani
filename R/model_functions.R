
# sample() when only one item in the bag
sample_safe <- function(x, ...) {
  x[sample.int(length(x), ...)]
}

increment_age <- function(people) {
  if (!"age_today" %in% names(people)) stop("age_today does not exist")
  if (any(is.na(people$age_today))) stop("age_today has missing values")
  if (!"is_alive" %in% names(people)) stop("is_alive does not exist")
  if (any(is.na(people$is_alive))) stop("is_alive has missing values")
  if (any(people$is_alive)) {
    people$age_today[people$is_alive] <- people$age_today[people$is_alive] + 1
  }
  return(people)
}

select_emigrants <- function(people, day, manual = NA) {
  active_people <- which(people$is_alive & people$is_present)
  if (length(active_people) > 0) {
    emigrants <- integer(0)
    # specify manual emigrations for testing
    if (!all(is.na(manual))) {
      if (!all(manual %in% active_people)) stop("only active people can emigrate")
      emigrants <- c(emigrants, manual)
    }
    # choose emigrants probabilistically
    baseline <- 0.001
    alpha <- log(baseline/(1 - baseline))
    daily_pr_emigrate <- (1 - (1/(1 + exp(alpha))))
    emigrated <- as.logical(rbinom(length(active_people), 1, daily_pr_emigrate))
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

select_fatalities <- function(people, day, manual = NA) {
  active_people <- which(people$is_alive & people$is_present)
  if (length(active_people) > 0) {
    fatalities <- integer(0)
    # specify manual fatalities for testing
    if (!all(is.na(manual))) {
      if (!all(manual %in% active_people)) stop("only active people can die")
      fatalities <- c(fatalities, manual)
    }
    # choose fatalities probabilistically
    baseline <- 0.001
    alpha <- log(baseline/(1 - baseline))
    daily_pr_death <- (1 - (1/(1 + exp(alpha))))
    died <- as.logical(rbinom(length(active_people), 1, daily_pr_death))
    fatalities <- c(fatalities, active_people[died])
    # add additional deaths to test
    if (length(fatalities) > 0) {
      people$date_of_death[fatalities] <- day
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
    people$age_today >= 15 & people$female & people$is_present)
  # random reindex to avoid any kind of order bias in mating
  available_women <- available_women[sample(length(available_women))]
  available_men <- which(is.na(people$current_mate) &
    people$age_today >= 15 & !people$female & people$is_present)
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

select_conceptions <- function(people, day, manual = NA) {
  available_women <- which(people$female & people$age_today <= 45 &
    people$age_today >= 15 & !is.na(people$current_mate))
  if (length(available_women) > 0) {
    conceptions <- integer(0)
    # specify manual conceptions for testing
    if (!all(is.na(manual))) {
      if (!all(manual %in% active_people)) stop("only active people can die")
      conceptions <- c(conceptions, manual)
    }
    # select conceptions probabilistically
    baseline <- 0.001
    alpha <- log(baseline / (1 - baseline))
    pr_concieve <- (1 - (1 / (1 + exp(alpha))))
    concieved <- as.logical(rbinom(length(available_women), 1, pr_concieve))
    conceptions <- c(conceptions, available_women[concieved])
    # update people table based on above
    if (length(conceptions) > 0) {
      people$due_date[conceptions] <- day + 30 * 9 # currently in days
    }
  }
  return(people)
}

generate_new_people <- function(n_new, people) {
  new_person <- vector("list", ncol(people))
  names(new_person) <- names(people)
  for (i in seq_along(new_person)) new_person[[i]] <- NA
  new_person$is_alive <- TRUE
  new_person$is_present <- TRUE
  new_people <- vector("list", n_new)
  for (i in 1:n_new) {
    new_people[[i]] <- new_person
  }
  new_people %>% bind_rows() %>% as.data.frame() -> new_people
  return(new_people)
}

add_immigrants <- function(people, day, n_immigrants = 0) {
  # choose n_immigrants probabilistically
  n_immigrants <- n_immigrants + rpois(1, 3)
  if (n_immigrants > 0) {
    immigrants <- generate_new_people(n_immigrants, people)
    for (i in 1:n_immigrants) {
      immigrants$female[i] <- rbinom(1, 1, 0.5)
      immigrants$age_today[i] <- rpois(1, 10)
      immigrants$date_of_birth[i] <- day - immigrants$age_today[i]
    }
    # update people table
    people <- bind_rows(people, immigrants)
  }
  return(people)
}

add_offspring <- function(people, day) {
  women_in_labor <- which(people$due_date == day)
  # currently one birth per woman
  n_births <- sum(people$due_date == day, na.rm = TRUE)
  if (n_births > 0) {
    births <- generate_new_people(n_births, people)
    for (i in 1:n_births) {
      births$female[i] <- rbinom(1, 1, 0.5)
      births$mother_id[i] <- women_in_labor[i]
      births$father_id[i] <- people$current_mate[women_in_labor[i]]
      births$age_today[i] <- 0
      births$date_of_birth[i] <- day
    }
    # update people table
    people <- bind_rows(people, births)
  }
  return(people)
}
