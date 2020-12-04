
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

add_immigrants <- function(people, day, n_immigrants = NA) {
  if (is.na(n_immigrants)) n_immigrants <- rpois(1, 3)
  if (n_immigrants > 0) {
    immigrants <- vector("list", n_immigrants)
    new_person <- vector("list", ncol(people))
    names(new_person) <- names(people)
    for (i in seq_along(new_person)) new_person[[i]] <- NA
    new_person$is_alive <- TRUE
    new_person$is_present <- TRUE
    for (i in 1:n_immigrants) {
      immigrants[[i]] <- new_person
      immigrants[[i]]$name <- "A"
      immigrants[[i]]$age_today <- rpois(1, 10)
      immigrants[[i]]$date_of_birth <- day - immigrants[[i]]$age_today
    }
    immigrants %>% bind_rows() %>% as.data.frame() -> immigrants
    people <- bind_rows(people, immigrants)
  }
  return(people)
}


select_emigrants <- function(people, day, emi_rows = NA) {
  # choose emigrants probabilistically
  active_rows <- which(people$is_alive & people$is_present)
  baseline <- 0.001
  alpha <- log(baseline/(1 - baseline))
  daily_pr_emigrate <- (1 - (1/(1 + exp(alpha))))
  emigrated <- as.logical(rbinom(length(active_rows), 1, daily_pr_emigrate))
  emigrated_rows <- active_rows[emigrated]
  # add additional emigrations for testing
  if (!all(is.na(emi_rows))) {
    cannot_emigrate <- setdiff(emi_rows, active_rows)
    if (length(cannot_emigrate) > 0) stop("only active people can emigrate")
    emigrated_rows <- c(emigrated_rows, emi_rows)
  }
  if (length(emigrated_rows) > 0) {
    people$is_present[emigrated_rows] <- FALSE
  }
  return(people)
}

select_fatalities <- function(people, day, kill_rows = NA) {
  # choose deaths probabilistically
  active_rows <- which(people$is_alive & people$is_present)
  if (length(active_rows) > 0) {
    baseline <- 0.001
    alpha <- log(baseline/(1 - baseline))
    daily_pr_death <- (1 - (1/(1 + exp(alpha))))
    died <- as.logical(rbinom(length(active_rows), 1, daily_pr_death))
    died_rows <- active_rows[died]
    # add additional deaths to test
    if (!all(is.na(kill_rows))) {
      cannot_die <- setdiff(kill_rows, active_rows)
      if (length(cannot_die) > 0) stop("only active people can die")
      died_rows <- c(died_rows, kill_rows)
    }
    if (length(died_rows) > 0) {
      people$date_of_death[died_rows] <- day
      people$is_alive[died_rows] <- FALSE
    }
  }
  return(people)
}
