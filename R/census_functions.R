
record_census <- function(censuses, ppl, current_tic, census_interval = 365) {
  if (current_tic %% census_interval == 0) {
    census_number <- floor(current_tic/census_interval)
    censuses[[census_number]] <- ppl
    censuses[[census_number]]$id <- 1:nrow(ppl)
    censuses[[census_number]]$current_tic <- current_tic
  }
  return(censuses)
}
