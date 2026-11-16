make_cr_test_quality <- function() {
  structure(
    dplyr::tibble(
      regicode = c("A", "B", "C", "D"),
      year = 2024L,
      sex = 1L,
      cancer = "101",
      fbs = c(100, 100, 150, 200),
      sws = c(80, 40, 75, 80),
      mv = c(95, 70, 80, 75)
    ),
    class = c("quality", "tbl_df", "tbl", "data.frame")
  )
}

test_that("cr_test runs both quality tests by default", {
  result <- cr_test(make_cr_test_quality())

  expect_s3_class(result, "cr_tests")
  expect_named(result, c("mi", "mv"))
  expect_equal(result$mi, compare_mi(make_cr_test_quality()))
  expect_equal(result$mv, compare_mv(make_cr_test_quality()))
})

test_that("cr_test dispatches to an individual quality test", {
  quality <- make_cr_test_quality()

  expect_equal(cr_test(quality, test = "mi"), compare_mi(quality))
  expect_equal(cr_test(quality, test = "mv"), compare_mv(quality))
})

test_that("cr_test forwards an external reference table", {
  quality <- make_cr_test_quality()
  reference <- dplyr::tibble(
    regicode = c("R1", "R2", "R3"),
    year = 2024L,
    sex = 1L,
    cancer = "101",
    fbs = c(100, 150, 200),
    sws = c(40, 75, 80),
    mv = c(70, 80, 75)
  )

  result <- cr_test(quality[1, ], reference = reference)

  expect_s3_class(result, "cr_tests")
  expect_equal(result$mi$reference_n, 3L)
  expect_equal(result$mv$reference_n, 3L)
})

test_that("cr_test dispatches incidence and mortality rate tests", {
  target <- structure(
    dplyr::tibble(
      regicode = "A",
      year = 2024L,
      sex = 1L,
      cancer = "101",
      asr_wld85 = 100,
      asr_var_wld85 = 25
    ),
    class = c("asr", "tbl_df", "tbl", "data.frame")
  )
  reference <- dplyr::tibble(
    year = 2024L,
    sex = 1L,
    cancer = "101",
    asr_wld85 = 80,
    asr_var_wld85 = 9
  )

  expect_equal(
    cr_test(target, test = "inci", reference = reference)$event,
    "inci"
  )
  expect_equal(
    cr_test(target, test = "mort", reference = reference)$event,
    "mort"
  )

  all_tests <- cr_test(target, reference = reference)
  expect_named(all_tests, c("inci", "mort"))
})

test_that("cr_test forwards the legacy variance scale", {
  target <- structure(
    dplyr::tibble(
      regicode = "A",
      year = 2024L,
      sex = 1L,
      cancer = "101",
      asr_wld85 = 100,
      asr_var_wld85 = 25 / 100000^2
    ),
    class = c("asr", "tbl_df", "tbl", "data.frame")
  )
  reference <- dplyr::tibble(
    year = 2024L,
    sex = 1L,
    cancer = "101",
    asr_wld85 = 80,
    asr_var_wld85 = 9 / 100000^2
  )

  result <- cr_test(
    target,
    test = "inci",
    reference = reference,
    variance_scale = "unmultiplied"
  )

  expect_equal(result$se_log_rate_ratio, sqrt(25 / 100^2 + 9 / 80^2))
})

test_that("cr_test forwards alpha and validates the test name", {
  quality <- make_cr_test_quality()

  expect_equal(
    cr_test(quality, test = "mi", alpha = 0.01),
    compare_mi(quality, alpha = 0.01)
  )
  expect_snapshot(error = TRUE, cr_test(quality, test = "invalid"))
})

test_that("cr_test preserves boundary statuses from both quality tests", {
  quality <- make_cr_test_quality()
  quality$fbs[1] <- 0
  quality$sws[1] <- 0
  quality$mv[1] <- NA_real_

  result <- cr_test(quality)

  expect_identical(result$mi$status[1], "zero_target_incidence")
  expect_identical(result$mv$status[1], "zero_target_cases")
})
