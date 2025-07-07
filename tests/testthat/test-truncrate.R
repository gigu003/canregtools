test_that("truncrate works with stdpop", {
  pop <- c(
    20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
    163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909
  )
  cases <- c(
    156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
    891, 831, 926, 731, 269
  )
  stdpop <- c(
    2.4, 9.6, 10, 9, 9, 8, 8, 6, 6, 6, 6, 5, 4, 4, 3, 2, 1, 0.5, 0.5
  )
  
  result <- truncrate(cases, pop, stdpop, trunc_age = c(35, 64))
  expect_type(result, "double")
  expect_equal(round(result, 2), 0.62)
})

test_that("truncrate works with stdpop = NULL (uniform weights)", {
  pop <- rep(10000, 19)
  cases <- 1:19
  result <- truncrate(cases, pop, stdpop = NULL,
                      trunc_age = c(5, 84),
                      sep_zero = FALSE)
  expect_equal(result, round(mean(cases / pop) * 100, 2))
})

test_that("truncrate throws error for mismatched lengths", {
  expect_error(truncrate(1:10, 1:9), "Length of 'cases' and 'pop' must be the same")
})

test_that("truncrate works with default parameters", {
  pop <- rep(10000, 19)
  cases <- rep(10, 19)
  stdpop <- rep(1, 19)
  res <- truncrate(cases, pop, stdpop, trunc_age = c(0, 84))
  expect_equal(res, 0.1)
})
