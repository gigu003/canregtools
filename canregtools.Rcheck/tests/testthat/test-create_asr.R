test_that("create_asr works correctly for canreg and canregs", {
  data("canregs")
  # Test with canreg object
  data <- canregs[[1]]
  expect_s3_class(data, "canreg")
  expect_s3_class(canregs, "canregs")
  # Test with show_pop, show_var, show_ci
  asr1 <- create_asr(fbsws, year, sex, cancer)
  expect_s3_class(asr1, "asr")
  expect_true("asr_cn2000" %in% names(asr1))
  expect_true("asr_wld85" %in% names(asr1))
  expect_false(any(grepl("pop", names(asr1))))
  expect_false(any(grepl("_var", names(asr1))))
  expect_false(any(grepl("_lower", names(asr1))))
  expect_false(any(grepl("_upper", names(asr1))))
  expect_true("rank" %in% names(asr1))
})
