test_that("zero-event intervals are finite for every method", {
  for (method in c("gamma", "normal", "lognormal")) {
    result <- ageadjust(c(0, 0), c(100, 100), stdpop = c(1, 1), method = method)
    expect_equal(result$cr_lci, 0)
    expect_equal(result$asr_lci, 0)
    expect_equal(result$cr_uci, -log(0.025) / 200 * 1e5)
    expect_equal(result$asr_uci, -log(0.025) / 200 * 1e5)
    expect_equal(all(is.finite(unlist(result))), TRUE)
    zero_weight <- ageadjust(
      c(3, 0),
      c(100, 100),
      stdpop = c(0, 1),
      method = method
    )
    expect_equal(zero_weight$asr_lci, 0)
    expect_equal(zero_weight$asr_uci, -log(0.025) / 100 * 1e5)
  }
})

test_that("positive-event rates and alternate inputs agree with hand calculations", {
  result <- ageadjust(c(2, 4), c(100, 200), stdpop = c(1, 1))
  expect_equal(result$cr, 2000)
  expect_equal(result$asr, 2000)
  expect_equal(result$cr_var, 6 / 300^2)
  expect_equal(result$asr_var, 0.25 * (2 / 100^2 + 4 / 200^2))
  expect_equal(result$cr_lci, qgamma(0.025, 6) / 300 * 1e5)
  expect_equal(
    ageadjust(pop = c(100, 200), rate = c(.02, .02)),
    ageadjust(c(2, 4), c(100, 200))
  )
  expect_equal(
    ageadjust(count = c(2, 4), rate = c(.02, .02)),
    ageadjust(c(2, 4), c(100, 200))
  )
})

test_that("ageadjust rejects invalid inputs instead of recycling or returning NaN", {
  expect_snapshot(error = TRUE, ageadjust(c(1, 2), 100))
  expect_snapshot(error = TRUE, ageadjust(1, 0))
  expect_snapshot(error = TRUE, ageadjust(0, 0))
  expect_snapshot(error = TRUE, ageadjust(1, NA_real_))
  expect_snapshot(error = TRUE, ageadjust(-1, 100))
  expect_snapshot(error = TRUE, ageadjust(numeric(), numeric()))
  expect_snapshot(error = TRUE, ageadjust(c(1, 2), c(100, 100), stdpop = 1))
  expect_snapshot(error = TRUE, ageadjust(1, 100, stdpop = 0))
  expect_snapshot(error = TRUE, ageadjust(1, 100, conf_level = 1))
  expect_snapshot(error = TRUE, ageadjust(1, 100, rate = 2))
})

test_that("truncated rates exclude the next age group", {
  for (separate in c(FALSE, TRUE)) {
    ages <- if (separate) c(0, 1, seq(5, 85, 5)) else seq(0, 85, 5)
    cases <- as.numeric(ages == 65) * 100
    pop <- rep(100, length(ages))
    expect_equal(truncrate(cases, pop, sep_zero = separate), 0)
    cases[ages == 35] <- 6
    expect_equal(truncrate(cases, pop, sep_zero = separate), 1)
    weights <- seq_along(ages)
    selected <- ages >= 35 & ages < 65
    expected <- round(
      sum(cases[selected] / pop[selected] * weights[selected]) /
        sum(weights[selected]) *
        100,
      2
    )
    expect_equal(truncrate(cases, pop, weights, sep_zero = separate), expected)
    expect_equal(
      truncrate(cases, pop, trunc_age = c(65, 69), sep_zero = separate),
      100
    )
  }
  expect_equal(
    truncrate(c(1, 4, rep(0, 17)), rep(100, 19), trunc_age = c(0, 0)),
    1
  )
})

test_that("truncated rates reject unaligned boundaries and weights", {
  expect_snapshot(
    error = TRUE,
    truncrate(rep(0, 18), rep(100, 18), trunc_age = c(36, 64), sep_zero = FALSE)
  )
  expect_snapshot(
    error = TRUE,
    truncrate(rep(0, 18), rep(100, 18), trunc_age = c(35, 63), sep_zero = FALSE)
  )
  expect_snapshot(error = TRUE, truncrate(rep(0, 18), rep(100, 18), stdpop = 1))
})
