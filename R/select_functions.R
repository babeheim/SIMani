
select_emigrants <- function(people, manual = NULL, calc_emigration = calc_emigration_basic) {
  emigrants <- integer(0)
  active_people <- which(people$is_alive & people$is_present)
  # specify manual emigrations for testing
  if (!is.null(manual)) {
    if (!all(manual %in% active_people)) stop("only active people can emigrate")
    emigrants <- c(emigrants, manual)
  }
  # choose emigrants probabilistically
  if (length(active_people) > 0) {
    logit_pr_emigrate <- calc_emigration()
    emigrated <- rbinom(length(active_people), 1, logistic(logit_pr_emigrate))
    emigrants <- c(emigrants, active_people[emigrated]) 
  }
  # update people table based on all of above
  if (length(emigrants) > 0) {
    people$is_present[emigrants] <- FALSE
    # currently emigrants dont go with relatives, but we can change
    people$current_mate[emigrants] <- NA
    people$current_mate[which(people$current_mate %in% emigrants)] <- NA
  }
  return(people)
}

select_fatalities <- function(people, current_tic, manual = NULL,
  calc_mortality = calc_mortality_basic) {
  fatalities <- integer(0)
  active_people <- which(people$is_alive & people$is_present)
  # specify manual fatalities for testing
  if (!is.null(manual)) {
    if (!all(manual %in% active_people)) stop("only active people can die")
    fatalities <- c(fatalities, manual)
  }
  # select fatalities probabilistically
  if (length(active_people) > 0) {
    logit_pr_die <- calc_mortality()
    died <- as.logical(rbinom(length(active_people), 1, logistic(logit_pr_die)))
    fatalities <- c(fatalities, active_people[died])
  }
  # update people table based on all of above
  if (length(fatalities) > 0) {
    people$date_of_death[fatalities] <- current_tic
    people$is_alive[fatalities] <- FALSE
    people$current_mate[fatalities] <- NA
    people$current_mate[which(people$current_mate %in% fatalities)] <- NA
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

select_conceptions <- function(people, current_tic, manual = NULL,
  calc_fertility = calc_fertility_basic, days_tic = 1) {
  conceptions <- integer(0)
  candidate_women <- which(people$female & people$age < 50 & people$age >= 15 &
    !is.na(people$current_mate) & is.na(people$due_date))
  # select manual conceptions for testing
  if (!is.null(manual)) {
    if (!all(manual %in% candidate_women)) stop("only fertile, mated women can conceive")
    conceptions <- c(conceptions, manual)
  }
  # select conceptions probabilistically
  if (length(candidate_women) > 0) {
    logit_pr_concieve <- calc_fertility()
    concieved <- rbinom(length(candidate_women), 1, logistic(logit_pr_concieve))
    conceptions <- c(conceptions, candidate_women[concieved])
  }
  # update people table based on all of above
  if (length(conceptions) > 0) {
    people$due_date[conceptions] <- current_tic + 270 # assumes one day per tic
  }
  return(people)
}
