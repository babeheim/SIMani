
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

  if (any(is.na(ppl$date_of_birth))) stop("date_of_birth has missing values")
  if (any(is.na(ppl$is_alive))) stop("is_alive has missing values")
}
