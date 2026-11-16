test_that("cumulative rates stop at the exclusive upper age bound", {
  rates <- rep(0.001, 19)
  rates[16:19] <- NA_real_
  expect_equal(unname(cumrate(rate = rates, eage = 70)), 0.07)
  expect_equal(unname(cumrate(rate = rates, eage = 70, sep_zero = FALSE)), 0.07)
  expect_equal(unname(cumrate(rate = rep(0.001, 19), eage = 72)), 0.072)
  expect_equal(unname(cumrate(rate = 0.001, eage = 1)), 0.001)
  expect_equal(unname(cumrate(rate = rates, eage = 0)), 0)
  expect_equal(unname(cumrate(rate = rep(0.001, 19), eage = 90, mp = 100)), 9)
  expect_equal(unname(cumrate(count = rep(1, 19), pop = rep(1000, 19))), 0.07)
})

test_that("cumulative rate age intervals are validated", {
  expect_snapshot(error = TRUE, cumrate(rate = numeric()))
  expect_snapshot(error = TRUE, cumrate(rate = rep(0.001, 19), eage = 100))
  expect_snapshot(error = TRUE, cumrate(rate = rep(0.001, 19), agewidth = 0))
})
