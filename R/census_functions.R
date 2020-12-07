
record_census <- function(censuses, people, current_tic, interval = 365) {
  if (current_tic %% interval == 0) {
    census_number <- floor(current_tic/interval)
    people$current_tic <- current_tic
    censuses[[census_number]] <- people
  }
  return(censuses)
}
