
select_emigrants <- function(ppl, manual = NULL, calc_emigration = calc_emigration_basic, ...) {
  emigrants <- integer(0)
  active <- which(ppl$is_alive & ppl$is_present)
  # specify manual emigrations for testing
  if (!is.null(manual)) {
    if (!all(manual %in% active)) stop("only active ppl can emigrate")
    emigrants <- c(emigrants, manual)
  }
  # choose emigrants probabilistically
  if (length(active) > 0) {
    pr_emigrate <- calc_emigration(ppl[active,], ...)
    emigrated <- rbinom(length(active), 1, pr_emigrate)
    emigrants <- c(emigrants, active[emigrated])
  }
  # update ppl table based on all of above
  if (length(emigrants) > 0) {
    ppl$is_present[emigrants] <- FALSE
    # currently emigrants dont go with relatives, but we can change
    ppl$current_partner[emigrants] <- NA
    ppl$current_partner[which(ppl$current_partner %in% emigrants)] <- NA
  }
  return(ppl)
}

select_fatalities <- function(ppl, current_tic, manual = NULL,
  calc_mortality = calc_pr_event, tic_length = 365, ...) {
  fatalities <- integer(0)
  active <- which(ppl$is_alive & ppl$is_present)
  # specify manual fatalities for testing
  if (!is.null(manual)) {
    if (!all(manual %in% active)) stop("only active ppl can die")
    fatalities <- c(fatalities, manual)
  }
  # select fatalities probabilistically
  if (length(active) > 0) {
    pr_die <- calc_mortality(ppl = ppl[active,], ...)
    died <- as.logical(rbinom(length(active), 1, pr_die))
    fatalities <- c(fatalities, active[died])
  }
  # update ppl table based on all of above
  if (length(fatalities) > 0) {
    ppl$date_of_death[fatalities] <- current_tic * (tic_length / 365)
    ppl$is_present[fatalities] <- FALSE
    ppl$is_alive[fatalities] <- FALSE
    ppl$age[fatalities] <- NA
    ppl$current_partner[fatalities] <- NA
    ppl$current_partner[which(ppl$current_partner %in% fatalities)] <- NA
  }
  return(ppl)
}

select_reproducers <- function(ppl, current_tic, manual = NULL,
  calc_fertility = calc_pr_event, tic_length = 365, ...) {
  reproducers <- integer(0)
  candidates <- which(ppl$is_alive & ppl$is_present & ppl$age >= 10 & ppl$age < 65)
  # select manually for testing
  if (!is.null(manual)) {
    if (!all(manual %in% candidates)) stop("only living, present adults can reproduce!")
    reproducers <- c(reproducers, manual)
  }
  # select births probabilistically
  if (length(candidates) > 0) {
    pr_reproduce <- calc_fertility(ppl = ppl[candidates,], ...)
    reproduced <- as.logical(rbinom(length(candidates), 1, pr_reproduce))
    reproducers <- c(reproducers, candidates[reproduced])
  }
  ppl$to_reproduce[reproducers] <- TRUE
  return(ppl)
}

select_partners <- function(ppl, calc_dyad_score = calc_dyad_score_random, ...) {
  # rows are women
  repro_women <- which(ppl$to_reproduce & ppl$female)
  n_repro_women <- length(repro_women)
  # columns are men
  repro_men <- which(ppl$to_reproduce & !ppl$female)
  n_repro_men <- length(repro_men)
  if (n_repro_women > 0 & n_repro_men > 0) {
    # scramble order to prevent order effects
    repro_women <- sample_safe(repro_women)
    repro_men <- sample_safe(repro_men)
    if (any(ppl$is_present[c(repro_women, repro_men)] == FALSE)) stop("reproductive people cannot be missing")
    if (any(ppl$is_alive[c(repro_women, repro_men)] == FALSE)) stop("reproductive people cannot be dead")
    dyad_scores <- matrix(NA, nrow = n_repro_women, ncol = n_repro_men)
    # now fill out the dyad scores
    for (i in 1:n_repro_women) {
      for (j in 1:n_repro_men) {
        dyad_scores[i, j] <- calc_dyad_score(repro_women[i], repro_men[j], ppl, ...)
      }
    }
    if (any(is.na(dyad_scores))) stop("some dyad scores are missing!")
    # now pair up according to the scores
    for (i in 1:min(n_repro_men, n_repro_women)) {
      dyad_highest_score <- max(dyad_scores, na.rm = TRUE)
      dyad_highest_score_index <- sample_safe(which(dyad_scores == dyad_highest_score), 1) # indexing from top left to bottom right
      dyad_highest_score_column <- ceiling(dyad_highest_score_index / n_repro_women)
      dyad_highest_score_row <- dyad_highest_score_index - n_repro_women * (dyad_highest_score_column - 1)
      dyad_scores[dyad_highest_score_row, ] <- NA
      dyad_scores[, dyad_highest_score_column] <- NA
      paired_woman <- repro_women[dyad_highest_score_row]
      paired_man <- repro_men[dyad_highest_score_column]
      ppl$current_partner[paired_man] <- paired_woman
      ppl$current_partner[paired_woman] <- paired_man
    }
  }
  # note: if n_repro_men != n_repro_women, some reproducing people will not pair up
  return(ppl)
}
