
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
