

update_trait_simple <- function(ppl, trait, pr_trait = 0.5) {
  if (!hasName(colnames(ppl), trait)) ppl[[trait]] <- NA
  tar <- which(is.na(ppl$everspan))
  ppl[[trait]][tar] <- as.logical(rbinom(length(tar), 1, prob = pr_trait))
  return(ppl)
}

update_age <- function(ppl, tic_length = 365) {
  # note: ages are always in years
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