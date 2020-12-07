
update_trait <- function(people, trait, rule) {
  return(people)
}

increment_age <- function(people, days_per_tic = 365) {
  if (!"age" %in% names(people)) stop("age does not exist")
  if (any(is.na(people$age))) stop("age has missing values")
  if (!"is_alive" %in% names(people)) stop("is_alive does not exist")
  if (any(is.na(people$is_alive))) stop("is_alive has missing values")
  if (any(people$is_alive)) {
    people$age[people$is_alive] <- people$age[people$is_alive] + (days_per_tic / 365)
  }
  return(people)
}