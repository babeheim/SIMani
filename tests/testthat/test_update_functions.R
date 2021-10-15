

# need some update_trait functions...
# [x] can initialize an arbitary number of traits on ppl with _simple and _vertical

test_that("update_trait_vertical works", {

  sch <- list(
    list(female =  TRUE, father_trait =    NA, mother_trait =    NA, pr_trait = 0.50),
    list(female = FALSE, father_trait =    NA, mother_trait =    NA, pr_trait = 0.50),
    list(female =  TRUE, father_trait =  TRUE, mother_trait =    NA, pr_trait = 0.50),
    list(female =  TRUE, father_trait = FALSE, mother_trait =    NA, pr_trait = 0.50),
    list(female = FALSE, father_trait =  TRUE, mother_trait =    NA, pr_trait = 0.50),
    list(female = FALSE, father_trait = FALSE, mother_trait =    NA, pr_trait = 0.50),
    list(female =  TRUE, father_trait =    NA, mother_trait =  TRUE, pr_trait = 0.50),
    list(female =  TRUE, father_trait =    NA, mother_trait = FALSE, pr_trait = 0.50),
    list(female = FALSE, father_trait =    NA, mother_trait =  TRUE, pr_trait = 0.50),
    list(female = FALSE, father_trait =    NA, mother_trait = FALSE, pr_trait = 0.50),
    list(female =  TRUE, father_trait = FALSE, mother_trait = FALSE, pr_trait = 0.50),
    list(female =  TRUE, father_trait = FALSE, mother_trait =  TRUE, pr_trait = 0.50),
    list(female =  TRUE, father_trait =  TRUE, mother_trait = FALSE, pr_trait = 0.50),
    list(female =  TRUE, father_trait =  TRUE, mother_trait =  TRUE, pr_trait = 0.50),
    list(female = FALSE, father_trait = FALSE, mother_trait = FALSE, pr_trait = 0.50),
    list(female = FALSE, father_trait = FALSE, mother_trait =  TRUE, pr_trait = 0.50),
    list(female = FALSE, father_trait =  TRUE, mother_trait = FALSE, pr_trait = 0.50),
    list(female = FALSE, father_trait =  TRUE, mother_trait =  TRUE, pr_trait = 0.50)
  ) %>% bind_rows %>% as.data.frame()

  ppl <- data.frame(
    age = rnorm(10000),
    female = TRUE,
    father = NA,
    mother = NA
  )
  
  expect_silent(ppl <- update_trait_vertical(ppl, trait = "snoob", schedule = sch))
  expect_true(mean(ppl$snoob) - 0.5 < 0.01)

})

test_that("update_trait_simple works", {

  ppl <- data.frame(age = rnorm(10000))
  expect_silent(ppl <- update_trait_simple(ppl, "snoob"))
  expect_true(mean(ppl$snoob) - 0.5 < 0.01)

  ppl <- data.frame(age = rnorm(10000))
  expect_silent(ppl <- update_trait_simple(ppl, "snoob", 0.2))
  expect_true(mean(ppl$snoob) - 0.2 < 0.01)

  ppl <- data.frame(age = rnorm(10))
  expect_silent(ppl <- update_trait_simple(ppl, "snoob", 0.2))
  expect_silent(ppl <- update_trait_simple(ppl, "tall", 0.6))
  expect_silent(ppl <- update_trait_simple(ppl, "bilingual", 0.1))
  expect_true(all(hasName(ppl, c("snoob", "tall", "bilingual"))))

})

test_that("update_age function works", {

  test <- list(
    list(age = 1, is_alive = TRUE),
    list(age = 2, is_alive = TRUE),
    list(age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test <- update_age(test, tic_length = 365)
  expect_true(test$age[1] == 2)
  expect_true(test$age[2] == 3)
  expect_true(test$age[3] == 1)

  for (i in 1:(365)) {
    test <- update_age(test, tic_length = 1)
  }
  expect_true(near(test$age[1], 3))
  expect_true(near(test$age[2], 4))
  expect_true(test$age[3] == 1)

  test <- update_age(test, tic_length = 365 * 10)
  expect_true(near(test$age[1], 13))
  expect_true(near(test$age[2], 14))
  expect_true(test$age[3] == 1)

  # all dead people? no one gets updated

  test2 <- list(
    list(age = 1, is_alive = FALSE),
    list(age = 2, is_alive = FALSE),
    list(age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  test2 <- update_age(test2, tic_length = 365)
  expect_true(test2$age[1] == 1)
  expect_true(test2$age[2] == 2)
  expect_true(test2$age[3] == 1)

  test3 <- list(
    list(age = 1, is_alive = FALSE),
    list(age = 2, is_alive = FALSE),
    list(age = 1, is_alive = FALSE)
  ) %>% bind_rows() %>% as.data.frame()

  for (i in 1:(365)) {
    test3 <- update_age(test3, tic_length = 1)
  }
  expect_true(test3$age[1] == 1)
  expect_true(test3$age[2] == 2)
  expect_true(test3$age[3] == 1)

})
