

# what do I want this to do exactly?
calc_fertility_usa <- function(n_draws, ...) {
  ages <- c(15, 20, 25, 30, 35, 40, 45)
  annual_births_per_1000 <- c(40.47, 102.5, 115.92, 96.1, 46.36, 9.12, 0.58)
}


calc_age_usa <- function(n_draws, ...) {
  list(
    list(age = 0, weight = 57),
    list(age = 1, weight = 74),
    list(age = 2, weight = 68),
    list(age = 3, weight = 77),
    list(age = 4, weight = 65),
    list(age = 5, weight = 75),
    list(age = 6, weight = 79),
    list(age = 7, weight = 83),
    list(age = 8, weight = 66),
    list(age = 9, weight = 60),
    list(age = 10, weight = 80),
    list(age = 11, weight = 72),
    list(age = 12, weight = 59),
    list(age = 13, weight = 87),
    list(age = 14, weight = 78),
    list(age = 15, weight = 73),
    list(age = 16, weight = 58),
    list(age = 17, weight = 73),
    list(age = 18, weight = 84),
    list(age = 19, weight = 67),
    list(age = 20, weight = 83),
    list(age = 21, weight = 76),
    list(age = 22, weight = 71),
    list(age = 23, weight = 74),
    list(age = 24, weight = 72),
    list(age = 25, weight = 62),
    list(age = 26, weight = 91),
    list(age = 27, weight = 64),
    list(age = 28, weight = 63),
    list(age = 29, weight = 76),
    list(age = 30, weight = 80),
    list(age = 31, weight = 79),
    list(age = 32, weight = 71),
    list(age = 33, weight = 73),
    list(age = 34, weight = 76),
    list(age = 35, weight = 72),
    list(age = 36, weight = 70),
    list(age = 37, weight = 82),
    list(age = 38, weight = 80),
    list(age = 39, weight = 70),
    list(age = 40, weight = 82),
    list(age = 41, weight = 72),
    list(age = 42, weight = 78),
    list(age = 43, weight = 75),
    list(age = 44, weight = 75),
    list(age = 45, weight = 83),
    list(age = 46, weight = 73),
    list(age = 47, weight = 70),
    list(age = 48, weight = 79),
    list(age = 49, weight = 71),
    list(age = 50, weight = 77),
    list(age = 51, weight = 81),
    list(age = 52, weight = 80),
    list(age = 53, weight = 71),
    list(age = 54, weight = 64),
    list(age = 55, weight = 70),
    list(age = 56, weight = 62),
    list(age = 57, weight = 65),
    list(age = 58, weight = 73),
    list(age = 59, weight = 76),
    list(age = 60, weight = 70),
    list(age = 61, weight = 73),
    list(age = 62, weight = 84),
    list(age = 63, weight = 74),
    list(age = 64, weight = 64),
    list(age = 65, weight = 63),
    list(age = 66, weight = 76),
    list(age = 67, weight = 72),
    list(age = 68, weight = 55),
    list(age = 69, weight = 81),
    list(age = 70, weight = 78),
    list(age = 71, weight = 74),
    list(age = 72, weight = 68),
    list(age = 73, weight = 69),
    list(age = 74, weight = 71),
    list(age = 75, weight = 63),
    list(age = 76, weight = 56),
    list(age = 77, weight = 50),
    list(age = 78, weight = 55),
    list(age = 79, weight = 12),
    list(age = 80, weight = 41),
    list(age = 81, weight = 38),
    list(age = 82, weight = 56),
    list(age = 83, weight = 30),
    list(age = 84, weight = 27),
    list(age = 85, weight = 22),
    list(age = 86, weight = 38),
    list(age = 87, weight = 30),
    list(age = 88, weight = 8),
    list(age = 89, weight = 22),
    list(age = 90, weight = 11),
    list(age = 91, weight = 12),
    list(age = 92, weight = 12),
    list(age = 93, weight = 10),
    list(age = 94, weight = 4),
    list(age = 95, weight = 9),
    list(age = 96, weight = 8),
    list(age = 97, weight = 3),
    list(age = 98, weight = 2),
    list(age = 99, weight = 4)
  ) %>% bind_rows -> age_dist

  out <- sample(age_dist$age, n_draws, prob = age_dist$weight, replace = TRUE)
  return(out)

}

calc_age_offspring <- function(n_draws, ...) {
  0
}

calc_age_simple <- function(n_draws, ...) {
  rpois(n_draws, 10)
}
