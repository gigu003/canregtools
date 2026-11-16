test_that("CI5-XII China reference data are bundled", {
  data("ci5_xii_china", package = "canregtools")

  expect_named(
    ci5_xii_china,
    c(
      "registry",
      "asr",
      "asr_registry",
      "mv",
      "mi",
      "availability",
      "metadata"
    )
  )
  expect_gt(nrow(ci5_xii_china$registry), 0L)
  expect_gt(nrow(ci5_xii_china$asr), 0L)
  expect_equal(anyDuplicated(ci5_xii_china$asr[c("sex", "cancer")]), 0L)
  expect_true(all(is.finite(ci5_xii_china$asr$asr_wld85)))
  expect_true(all(
    ci5_xii_china$availability$available == c(TRUE, FALSE, FALSE)
  ))
  expect_equal(nrow(ci5_xii_china$mv), 0L)
  expect_equal(nrow(ci5_xii_china$mi), 0L)
  expect_identical(ci5_xii_china$metadata$variance_scale, "display")
  expect_gte(min(ci5_xii_china$asr$asr_var_wld85), 0)
})

test_that("CI5-XII pooled ASR is usable as a rate reference", {
  data("ci5_xii_china", package = "canregtools")
  reference <- ci5_xii_china$asr[1, , drop = FALSE]
  target <- reference
  target$regicode <- "target"
  target$asr_wld85 <- target$asr_wld85 * 1.1
  class(target) <- c("asr", "data.frame")

  result <- compare_rate(
    target,
    event = "inci",
    reference = reference
  )

  expect_identical(result$status, "ok")
  expect_identical(result$event, "inci")
  expect_equal(result$reference_n, 1L)
})
