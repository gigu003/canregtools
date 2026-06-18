test_that("estimate_pop returns correct structure", {
  pop1 <- c(
    59546, 294129, 472511, 552549, 821119, 996436, 805635, 1004506,
    989357, 1056612, 986559, 792270, 544544, 452297, 473579, 350802,
    212614, 109598, 61990
  )
  pop2 <- c(
    75641, 377276, 327116, 380338, 539034, 1158852, 1152329, 881443,
    903484, 1011164, 1238871, 1137832, 1022787, 645441, 464777,
    482941, 406144, 227977, 144526
  )
  period <- c(2000, 2010)
  
  est <- esti_pop(pop1, pop2, period)
  
  expect_s3_class(est, "data.frame")
  expect_equal(ncol(est), length(period[1]:period[2]))
  expect_equal(nrow(est), length(pop1))
  expect_equal(colnames(est), as.character(2000:2010))
})

test_that("population proportions sum to ~1 per year", {
  pop1 <- rep(1000, 10)
  pop2 <- rep(2000, 10)
  period <- c(2000, 2005)
  est <- esti_pop(pop1, pop2, period)
  
  yearly_sums <- colSums(est)
  expect_true(all(abs(yearly_sums - 1) < 1e-6))
})

test_that("estimate_pop handles edge cases", {
  pop1 <- rep(1, 5)
  pop2 <- rep(1, 5)
  period <- c(2000, 2002)
  
  est <- esti_pop(pop1, pop2, period)
  
  expect_equal(nrow(est), 5)
  expect_equal(ncol(est), 3)
  expect_true(all(abs(colSums(est) - 1) < 1e-6))
})
