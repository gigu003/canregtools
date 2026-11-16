make_rate_data <- function() {
  structure(
    dplyr::tibble(
      regicode = c("A", "B"),
      year = 2024L,
      sex = 1L,
      cancer = "101",
      asr_wld85 = c(100, 50),
      asr_var_wld85 = c(25, 16)
    ),
    class = c("asr", "tbl_df", "tbl", "data.frame")
  )
}

make_rate_reference <- function() {
  dplyr::tibble(
    regicode = "CI5-reference",
    year = 2024L,
    sex = 1L,
    cancer = "101",
    asr_wld85 = 80,
    asr_var_wld85 = 9
  )
}

test_that("compare_rate applies the CI5 rate-ratio test", {
  result <- compare_rate(
    make_rate_data(),
    event = "inci",
    reference = make_rate_reference()
  )
  target <- result[result$regicode == "A", ]
  se <- sqrt(25 / 100^2 + 9 / 80^2)
  z <- log(100 / 80) / se

  expect_equal(target$event, "inci")
  expect_equal(target$rate_difference, 20)
  expect_equal(target$rate_ratio, 1.25)
  expect_equal(target$se_log_rate_ratio, se)
  expect_equal(target$z, z)
  expect_equal(target$p_value, 2 * stats::pnorm(abs(z), lower.tail = FALSE))
  expect_identical(target$unusual, TRUE)
  expect_identical(target$status, "ok")
})

test_that("compare_rate accepts displayed-scale standard errors", {
  target <- make_rate_data()
  target$asr_se_wld85 <- sqrt(target$asr_var_wld85)
  target$asr_var_wld85 <- NULL
  reference <- make_rate_reference()
  reference$asr_se_wld85 <- sqrt(reference$asr_var_wld85)
  reference$asr_var_wld85 <- NULL

  result <- compare_rate(target, reference = reference)

  expect_equal(
    result$se_log_rate_ratio,
    sqrt(
      (target$asr_se_wld85 / target$asr_wld85)^2 +
        (reference$asr_se_wld85 / reference$asr_wld85)^2
    )
  )
})

test_that("compare_rate supports a single-registry ASR without regicode", {
  target <- make_rate_data()[1, ]
  target$regicode <- NULL

  result <- compare_rate(target, reference = make_rate_reference())

  expect_identical(result$regicode, "target")
  expect_identical(result$status, "ok")
})

test_that("compare_rate matches external references by strata", {
  target <- dplyr::bind_rows(
    make_rate_data(),
    transform(make_rate_data(), cancer = "102", asr_wld85 = c(30, 20))
  )
  class(target) <- class(make_rate_data())
  reference <- dplyr::bind_rows(
    make_rate_reference(),
    transform(make_rate_reference(), cancer = "102", asr_wld85 = 25)
  )

  result <- compare_rate(target, event = "mort", reference = reference)

  expect_equal(nrow(result), 4L)
  expect_setequal(result$event, "mort")
  expect_setequal(result$reference_n, 1L)
  expect_setequal(result$status, "ok")
})

test_that("compare_rate requires rate variances", {
  target <- make_rate_data()
  target$asr_var_wld85 <- NULL

  expect_snapshot(
    error = TRUE,
    compare_rate(
      target,
      reference = make_rate_reference()
    )
  )
})

test_that("compare_rate can use legacy unmultiplied variances", {
  result <- compare_rate(
    transform(
      make_rate_data(),
      asr_var_wld85 = asr_var_wld85 / 100000^2
    ),
    reference = transform(
      make_rate_reference(),
      asr_var_wld85 = asr_var_wld85 / 100000^2
    ),
    variance_scale = "unmultiplied"
  )

  expect_identical(result$status, rep("ok", 2L))
})

test_that("compare_rate reports rate and variance boundary statuses", {
  target <- make_rate_data()[1, ]
  reference <- make_rate_reference()

  target$asr_wld85 <- 0
  expect_identical(
    compare_rate(target, reference = reference)$status,
    "nonpositive_rate"
  )

  target$asr_wld85 <- 100
  target$asr_var_wld85 <- 0
  reference$asr_var_wld85 <- 0
  expect_identical(
    compare_rate(target, reference = reference)$status,
    "zero_rate_variance"
  )

  reference$asr_wld85 <- NA_real_
  expect_identical(
    compare_rate(target, reference = reference)$status,
    "missing_rate"
  )

  reference$asr_wld85 <- 80
  reference$asr_var_wld85 <- NA_real_
  expect_identical(
    compare_rate(target, reference = reference)$status,
    "missing_rate_variance"
  )

  reference$asr_var_wld85 <- 9
  multiple <- dplyr::bind_rows(
    reference,
    transform(reference, regicode = "CI5-reference-2", asr_wld85 = 81)
  )
  expect_identical(
    compare_rate(target, reference = multiple)$status,
    "multiple_reference_rates"
  )
})
