test_that("get_pop summarizes canregs population by sex", {
  pop <- get_pop(canregs, sum_by = "sex", collapse = TRUE)

  expect_s3_class(pop, "data.frame")
  expect_named(pop, c("regicode", "sex", "rks"))
  expect_setequal(pop$sex, c(1L, 2L))
  expect_equal(nrow(pop), length(canregs) * 2)
  expect_gt(sum(pop$rks), 0)
})

test_that("get_pop returns lists for canregs when collapse is FALSE", {
  pop <- get_pop(canregs, sum_by = "sex", collapse = FALSE)

  expect_type(pop, "list")
  expect_length(pop, length(canregs))
  expect_named(pop, names(canregs))
  expect_equal(
    unname(purrr::map_lgl(pop, ~ identical(names(.x), c("sex", "rks")))),
    rep(TRUE, length(pop))
  )
})

test_that("get_stdpop includes common European and US standards", {
  expected <- c(
    "cn64",
    "cn82",
    "cn2000",
    "wld85",
    "wld2000",
    "eur76",
    "eur2013",
    "usa2000"
  )
  expect_identical(names(canregtools:::dict_maps$std_pop)[-1], expected)
  expect_equal(sum(get_stdpop("eur76")), 100000)
  expect_equal(sum(get_stdpop("eur2013")), 100000)
  expect_equal(sum(get_stdpop("usa2000")), 274633642)
  expect_equal(
    get_stdpop("eur2013")[c(1, 2, 18, 19)],
    c(1000, 4000, 2500, 2500)
  )
})

test_that("get_stdpop exactly combines compatible age groups", {
  original <- get_stdpop("wld85")
  breaks <- c(0, 1, 15, 45, 65, 85)

  expect_equal(
    get_stdpop("wld85", sep_zero = FALSE),
    c(sum(original[1:2]), original[3:19])
  )
  expect_equal(
    get_stdpop("wld85", age_breaks = breaks),
    c(
      original[1],
      sum(original[2:4]),
      sum(original[5:10]),
      sum(original[11:14]),
      sum(original[15:18]),
      original[19]
    )
  )
  expect_equal(
    get_stdpop("wld85", age_breaks = c(0, 1, 15, 45, 65)),
    c(
      original[1],
      sum(original[2:4]),
      sum(original[5:10]),
      sum(original[11:14]),
      sum(original[15:19])
    )
  )
})

test_that("get_stdpop interpolates only when requested", {
  original <- get_stdpop("wld85")
  breaks <- c(0, 1, 3, 5, seq(10, 85, by = 5))
  interpolated <- get_stdpop(
    "wld85",
    age_breaks = breaks,
    method = "interpolate"
  )

  expect_length(interpolated, length(breaks))
  expect_equal(sum(interpolated), sum(original), tolerance = 1e-5)
})

test_that("Sprague standard-population interpolation combines 0 and 1--4", {
  original <- get_stdpop("wld85")
  breaks <- c(0, seq(5, 85, by = 5))
  interpolated <- get_stdpop(
    "wld85",
    age_breaks = breaks,
    method = "interpolate",
    interpolation_method = "sprague"
  )

  expect_equal(interpolated, c(sum(original[1:2]), original[3:19]))
  expect_error(
    get_stdpop(
      "wld85",
      age_breaks = c(0, 1, 5, seq(10, 85, by = 5)),
      method = "interpolate",
      interpolation_method = "sprague"
    ),
    "cannot split 0--4"
  )
})
