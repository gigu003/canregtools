test_that("create_quality.canreg works with default parameters", {
  qua <- create_quality(fbsws, year, sex)
  expect_s3_class(qua, "quality")
  expect_true(all(c("year", "sex", "cancer", "rks",
                    "fbs", "inci", "sws", "mort", "mi",
                    "mv", "dco", "ub", "sub") %in% names(qua)))
})


