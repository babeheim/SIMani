
update_trait <- function(people, trait, rule) {
  return(people)
}

increment_age <- function(people, tic_length = 1) {
  if (!"is_alive" %in% names(people)) stop("is_alive does not exist")
  if (any(is.na(people$is_alive))) stop("is_alive has missing values")
  if (!"age" %in% names(people)) stop("age does not exist")
  living <- which(people$is_alive)
  if (any(people$is_alive)) {
    if (any(is.na(people$age[living]))) stop("living age values are missing")
    if (any(people$age[living] < 0)) stop("living age values are negative")
    people$age[living] <- people$age[living] + (tic_length / 365)
  }
  active <- which(people$is_alive & people$is_present)
  if (length(active) > 0) {
    if (any(people$age[active] > 120)) warning("active age values are implausibly high")
  }
  return(people)
}