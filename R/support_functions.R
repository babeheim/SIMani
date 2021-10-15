
# sample() when only one item in the bag
sample_safe <- function(x, ...) {
  x[sample.int(length(x), ...)]
}

logistic <- function (x) {
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  return(p)
}

logit <- function(x) log(x) - log(1 - x)

inspect_ppl <- function(ppl) {
  # these variables must always be present
  if (!"age" %in% names(ppl)) stop("age does not exist")
  if (!"date_of_birth" %in% names(ppl)) stop("dob does not exist")
  if (!"female" %in% names(ppl)) stop("female does not exist")

  # these variables should generally be present (but don't have to be)
  if (!"father" %in% names(ppl)) warning("father does not exist")
  if (!"mother" %in% names(ppl)) warning("mother does not exist")
  if (!"current_partner" %in% names(ppl)) warning("current_partner does not exist")

  if (!"is_alive" %in% names(ppl)) warning("is_alive does not exist")
  if (!"is_present" %in% names(ppl)) warning("is_present does not exist")
  if (!"date_of_death" %in% names(ppl)) warning("dod does not exist")

  if (any(!is.na(ppl$father) & ppl$father > nrow(ppl))) stop("invalid father id")
  if (any(!is.na(ppl$mother) & ppl$mother > nrow(ppl))) stop("invalid mother id")
  if (any(!is.na(ppl$father) & ppl$female[ppl$father])) stop("some females listed as fathers!")
  if (any(!is.na(ppl$mother) & !ppl$female[ppl$mother])) stop("some males listed as mothers!")
  if (any(is.na(ppl$date_of_birth))) stop("date_of_birth has missing values")
  if (any(is.na(ppl$is_alive))) stop("is_alive has missing values")
}

record_census <- function(censuses, ppl, current_tic, census_interval = 365) {
  if (current_tic %% census_interval == 0) {
    census_number <- floor(current_tic/census_interval)
    censuses[[census_number]] <- ppl
    censuses[[census_number]]$id <- 1:nrow(ppl)
    censuses[[census_number]]$current_tic <- current_tic
  }
  return(censuses)
}

bin_ages <- function(ages, age_bins) {
  age_bins <- sort(unique(age_bins))
  bindex <- cut(ages, age_bins, right = FALSE, labels = FALSE)
  if (any(ages == max(age_bins))) {
    bindex[ages == max(age_bins)] <- length(age_bins)
  }
  if (any(is.na(bindex))) stop("some ages not covered by the age bins")
  return(age_bins[bindex])
}
