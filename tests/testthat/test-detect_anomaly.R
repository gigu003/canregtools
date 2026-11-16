make_anomaly_asr <- function() {
  structure(
    dplyr::tibble(
      regicode = rep("A", 5),
      year = as.character(2018:2022),
      sex = "0",
      cancer = "60",
      cr = c(10, 10.1, 10.2, 20, 10.3),
      cr_var = rep(1, 5),
      asr_wld85 = c(10, 10.1, 10.2, 20, 10.3),
      asr_var_wld85 = rep(1, 5)
    ),
    class = c("asr", "tbl_df", "tbl", "data.frame")
  )
}

test_that("detect_anomaly detects an uncertainty-based rate jump", {
  result <- detect_anomaly(
    make_anomaly_asr(),
    metrics = "asr_wld85",
    method = "uncertainty"
  )

  jump <- result[result$year == "2021", ]
  expect_identical(jump$direction, "high")
  expect_gt(jump$score, 3)
  expect_identical(jump$anomaly, TRUE)
  expect_identical(jump$status, "ok")
  expect_equal(jump$se, 1)
})

test_that("detect_anomaly accepts ASR standard-error columns", {
  data <- make_anomaly_asr()
  data$asr_se_wld85 <- sqrt(data$asr_var_wld85)
  data$asr_var_wld85 <- NULL

  result <- detect_anomaly(
    data,
    metrics = "asr_wld85",
    method = "uncertainty"
  )

  expect_equal(result$se, rep(1, 5))
  expect_identical(result$status[result$year == "2021"], "ok")
})

test_that("detect_anomaly accepts legacy unmultiplied ASR variances", {
  data <- make_anomaly_asr()
  data$asr_var_wld85 <- data$asr_var_wld85 / 100000^2

  result <- detect_anomaly(
    data,
    metrics = "asr_wld85",
    method = "uncertainty",
    variance_scale = "unmultiplied"
  )

  expect_equal(result$se, rep(1, 5))
})

test_that("detect_anomaly supports robust historical scores", {
  result <- detect_anomaly(
    make_anomaly_asr(),
    metrics = "cr",
    method = "robust"
  )

  jump <- result[result$year == "2021", ]
  expect_identical(jump$direction, "high")
  expect_gt(jump$score, 3)
  expect_identical(jump$anomaly, TRUE)
  expect_true(all(is.na(result$p_value)))
})

test_that("detect_anomaly keeps zero values out of comparisons by default", {
  data <- make_anomaly_asr()
  data$asr_wld85[3] <- 0

  result <- detect_anomaly(data, metrics = "asr_wld85", method = "robust")
  zero <- result[result$year == "2020", ]

  expect_identical(zero$status, "zero_value")
  expect_identical(zero$anomaly, FALSE)
})

test_that("detect_anomaly estimates quality-indicator standard errors", {
  quality <- structure(
    dplyr::tibble(
      regicode = rep("A", 5),
      year = as.character(2018:2022),
      sex = "0",
      cancer = "60",
      rks = rep(100000, 5),
      fbs = c(100, 102, 98, 101, 99),
      inci = c(100, 102, 98, 101, 99),
      sws = c(50, 51, 49, 50, 50),
      mort = c(50, 51, 49, 50, 50),
      mi = c(0.5, 0.5, 0.5, 0.5, 0.51),
      mv = c(80, 81, 79, 80, 80),
      mvs = c(80, 81, 79, 80, 80)
    ),
    class = c("quality", "tbl_df", "tbl", "data.frame")
  )

  result <- detect_anomaly(
    quality,
    metrics = c("inci", "mort", "mv", "mi"),
    method = "uncertainty"
  )

  expect_equal(nrow(result), 20L)
  expect_true(all(is.finite(result$se[result$metric != "mi"])))
  expect_true(all(result$status[result$year != "2018"] == "ok"))
})

test_that("detect_anomaly rejects period labels", {
  data <- make_anomaly_asr()
  data$year[1] <- "2003-2007"

  expect_snapshot(error = TRUE, detect_anomaly(data, metrics = "cr"))
})

test_that("detect_anomaly validates duplicate years", {
  data <- make_anomaly_asr()
  data <- dplyr::bind_rows(data, data[1, ])

  expect_snapshot(error = TRUE, detect_anomaly(data, metrics = "cr"))
})
