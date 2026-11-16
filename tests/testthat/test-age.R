test_that("calc_age preserves birthday and leap-day boundaries", {
  birth <- as.Date(c(
    "2000-02-29",
    "2000-02-29",
    "2000-02-29",
    "1980-12-31",
    "1980-12-31",
    "2021-01-01",
    NA
  ))
  event <- as.Date(c(
    "2021-02-28",
    "2021-03-01",
    "2020-02-29",
    "2021-12-30",
    "2021-12-31",
    "2020-12-31",
    "2021-01-01"
  ))
  expected <- c(20, 21, 20, 40, 41, -1, NA_real_)
  expect_identical(calc_age(birth, event), expected)
  expect_identical(calc_age(as.character(birth), as.character(event)), expected)
  expect_identical(calc_age(as.POSIXct(birth), as.POSIXct(event)), expected)
  expect_identical(
    calc_age(as.Date(character()), as.Date(character())),
    numeric()
  )
  expect_identical(
    calc_age(as.Date(c(NA, NA)), as.Date(c(NA, NA))),
    rep(NA_real_, 2)
  )
  expect_snapshot(error = TRUE, calc_age(birth, event[-1]))
})

test_that("calc_age matches the previous date-component calculation", {
  set.seed(123)
  birth <- as.Date("1850-01-01") + sample.int(70000, 1000, replace = TRUE)
  event <- birth + sample(-1000:40000, 1000, replace = TRUE)
  birth[1] <- NA
  event[2] <- NA
  components <- function(x, field) as.numeric(format(x, field))
  before <- components(event, "%m") < components(birth, "%m") |
    (components(event, "%m") == components(birth, "%m") &
      components(event, "%d") < components(birth, "%d"))
  expected <- components(event, "%Y") - components(birth, "%Y") - before
  expect_identical(calc_age(birth, event), expected)
})
