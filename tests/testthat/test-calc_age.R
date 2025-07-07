test_that("calc_age computes correct age for known dates", {
  birth <- as.Date(c("2000-01-01", "2000-12-31", "1985-06-15"))
  event <- as.Date(c("2020-01-01", "2020-12-30", "2020-06-14"))
  expected <- c(20, 19, 34)
  
  result <- calc_age(birth, event)
  expect_equal(result, expected)
})

test_that("calc_age subtracts 1 if birthday hasn't occurred yet", {
  birth <- as.Date("2000-12-31")
  event <- as.Date("2020-12-30")
  expect_equal(calc_age(birth, event), 19)
})

test_that("calc_age returns NA for NA inputs", {
  birth <- as.Date(c(NA, "1990-01-01"))
  event <- as.Date(c("2020-01-01", NA))
  result <- calc_age(birth, event)
  expected <- as.numeric(c(NA, NA))
  expect_equal(result, expected)
})

test_that("calc_age handles leap years and birthdays correctly", {
  birth <- as.Date("2004-02-29")
  event1 <- as.Date("2021-02-28")
  event2 <- as.Date("2021-03-01")
  expect_equal(calc_age(birth, event1), 16)
  expect_equal(calc_age(birth, event2), 17)
})

test_that("calc_age errors on length mismatch", {
  birth <- as.Date(c("2000-01-01", "2000-01-02"))
  event <- as.Date("2020-01-01")
  expect_error(calc_age(birth, event),
               "Length of 'birth_date' and 'onset_date' must be equal.")
})
