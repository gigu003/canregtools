test_that("create_asr works correctly for canreg and canregs", {
  data("canregs")
  
  # Test with canreg object
  data1 <- canregs[[1]]
  expect_s3_class(data1, "canreg")
  
  asr1 <- create_asr(data1, year, sex, cancer)
  expect_s3_class(asr1, "asr")
  expect_true("asr_cn2000" %in% names(asr1))
  expect_true("asr_wld85" %in% names(asr1))
  expect_false(any(grepl("pop_", names(asr1))))  # default show_pop = FALSE
  
  # Test with show_pop, show_var, show_ci
  asr2 <- create_asr(data1, year, sex, cancer,
                     show_pop = TRUE,
                     show_var = TRUE,
                     show_ci = TRUE)
  expect_true(any(grepl("pop", names(asr2))))
  expect_true(any(grepl("_var", names(asr2))))
  expect_true(any(grepl("_lower", names(asr2))))
  expect_true(any(grepl("_upper", names(asr2))))
  
  # Test with multiple std
  asr3 <- create_asr(data1, year, sex, cancer, std = c("cn82", "cn2000", "wld85"))
  expect_true(all(c("asr_cn82", "asr_cn2000", "asr_wld85") %in% names(asr3)))
  
  # Test with canregs
  asr4 <- create_asr(canregs, year, sex, cancer)
  expect_s3_class(asr4, "tbl_df")
  expect_true(all(c("asr_cn2000", "asr_wld85") %in% names(asr4)))
  
  # Confirm filtering when sex not passed
  asr5 <- create_asr(data1, year, cancer)  # no sex passed
  expect_true(all(asr5$sex == 0L))
  
  # Check that rank column exists if cancer included
  expect_true("rank" %in% names(asr1))
})
