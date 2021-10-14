
update_trait <- function(ppl, trait, rule) {
  return(ppl)
}

update_age <- function(ppl, tic_length = 365) {
  if (!"is_alive" %in% names(ppl)) stop("is_alive does not exist")
  if (any(is.na(ppl$is_alive))) stop("is_alive has missing values")
  if (!"age" %in% names(ppl)) stop("age does not exist")
  living <- which(ppl$is_alive)
  if (any(ppl$is_alive)) {
    if (any(is.na(ppl$age[living]))) stop("living age values are missing")
    if (any(ppl$age[living] < 0)) stop("living age values are negative")
    ppl$age[living] <- ppl$age[living] + (tic_length / 365)
  }
  active <- which(ppl$is_alive & ppl$is_present)
  if (length(active) > 0) {
    if (any(ppl$age[active] > 120)) warning("active age values are implausibly high")
  }
  return(ppl)
}