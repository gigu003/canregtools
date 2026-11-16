make_quality <- function() {
  structure(
    dplyr::tibble(
      regicode = c("A", "B", "C", "D"),
      year = 2024L,
      sex = 1L,
      cancer = "101",
      fbs = c(100, 100, 150, 200),
      sws = c(80, 40, 75, 80),
      mi = sws / fbs
    ),
    class = c("quality", "tbl_df", "tbl", "data.frame")
  )
}

test_that("compare_mi reproduces the Parkin-Bray statistic", {
  quality <- make_quality()
  result <- compare_mi(quality)
  target <- result[result$regicode == "A", ]

  deaths <- c(40, 75, 80)
  cases <- c(100, 150, 200)
  theta <- sum(deaths) / sum(cases)
  phi <- sum(
    (deaths - theta * cases)^2 / ((deaths + cases) * theta)
  ) /
    2
  z2 <- (80 - theta * 100)^2 / (phi * 180 * theta)

  expect_equal(target$reference_n, 3L)
  expect_equal(target$theta, theta)
  expect_equal(target$phi, phi)
  expect_equal(target$z2, z2)
  expect_equal(target$p_value, stats::pchisq(z2, 1, lower.tail = FALSE))
  expect_equal(target$critical_value, stats::qchisq(0.95, 1))
  expect_identical(target$unusual, TRUE)
  expect_identical(target$status, "ok")
})

test_that("compare_mi accepts collapsed and uncollapsed quality results", {
  quality <- make_quality()
  qualities <- split(quality, quality$regicode)
  qualities <- lapply(qualities, function(x) {
    structure(x, class = class(quality))
  })
  class(qualities) <- c("qualities", "list")

  expect_equal(compare_mi(qualities), compare_mi(quality))
})

test_that("compare_mi accepts an external reference table", {
  target <- make_quality()[1, ]
  reference <- dplyr::tibble(
    registry = c("R1", "R2", "R3"),
    year = 2024L,
    sex = 1L,
    cancer = "101",
    cases = c(100, 150, 200),
    sws = c(40, 75, 80)
  )

  result <- compare_mi(target, reference = reference)
  expected <- compare_mi(make_quality())
  expected <- expected[expected$regicode == "A", ]

  expect_equal(result$reference_n, 3L)
  expect_equal(result$theta, expected$theta)
  expect_equal(result$phi, expected$phi)
  expect_equal(result$z2, expected$z2)
  expect_identical(result$status, "ok")
})

test_that("compare_mi matches external references by strata", {
  target <- dplyr::bind_rows(
    make_quality(),
    transform(make_quality(), cancer = "102", sws = c(30, 20, 15, 10))
  )
  class(target) <- class(make_quality())
  reference <- dplyr::bind_rows(
    transform(make_quality()[-1, ], regicode = paste0("R", 1:3)),
    transform(
      make_quality()[-1, ],
      regicode = paste0("S", 1:3),
      cancer = "102",
      sws = c(20, 15, 10)
    )
  )

  result <- compare_mi(target, reference = reference)

  expect_equal(nrow(result), 8L)
  expect_setequal(result$reference_n, 3L)
  expect_setequal(result$status, "ok")
})

test_that("compare_mi detects strata automatically", {
  first <- make_quality()
  second <- first
  second$cancer <- "102"
  second$sws <- c(30, 20, 15, 10)
  second$mi <- second$sws / second$fbs
  quality <- dplyr::bind_rows(first, second)
  class(quality) <- class(first)

  result <- compare_mi(quality)

  expect_equal(nrow(result), 8L)
  expect_equal(length(unique(result$theta)), 8L)
  expect_setequal(result$cancer, c("101", "102"))
})

test_that("compare_mi uses the requested significance level", {
  result <- compare_mi(make_quality(), alpha = 0.01)

  expect_equal(unique(result$critical_value), stats::qchisq(0.99, 1))
  expect_identical(
    result$unusual,
    result$z2 >= stats::qchisq(0.99, 1)
  )
})

test_that("compare_mi reports groups that cannot be evaluated", {
  insufficient <- make_quality()[1:2, ]
  zero_target <- make_quality()
  zero_target$fbs[1] <- 0
  zero_reference <- make_quality()
  zero_reference$fbs[1] <- 0
  zero_reference_mi <- make_quality()
  zero_reference_mi$sws[] <- 0
  zero_dispersion <- make_quality()
  zero_dispersion$sws <- zero_dispersion$fbs / 2

  expect_setequal(
    compare_mi(insufficient)$status,
    "insufficient_reference_registries"
  )
  expect_identical(compare_mi(zero_target)$status[1], "zero_target_incidence")
  expect_setequal(
    compare_mi(zero_reference)$status[-1],
    "zero_reference_incidence"
  )
  expect_setequal(compare_mi(zero_reference_mi)$status, "zero_reference_mi")
  expect_setequal(
    compare_mi(zero_dispersion)$status,
    "zero_reference_dispersion"
  )
})

test_that("compare_mi evaluates exactly two reference registries", {
  result <- compare_mi(make_quality()[1:3, ])

  expect_equal(result$reference_n, rep(2L, 3L))
  expect_setequal(result$status, "ok")
  expect_equal(
    result$critical_value,
    rep(stats::qchisq(0.95, 1), 3L)
  )
})

test_that("compare_mi validates its inputs", {
  quality <- make_quality()

  expect_snapshot(error = TRUE, compare_mi(data.frame()))
  expect_snapshot(error = TRUE, compare_mi(quality, alpha = 1))
  expect_snapshot(error = TRUE, compare_mi(quality[0, ]))
  expect_snapshot(
    error = TRUE,
    compare_mi(structure(quality[, -1], class = class(quality)))
  )

  missing_count <- quality
  missing_count$fbs[1] <- NA
  expect_snapshot(error = TRUE, compare_mi(missing_count))

  fractional_count <- quality
  fractional_count$sws[1] <- 1.5
  expect_snapshot(error = TRUE, compare_mi(fractional_count))

  duplicate <- dplyr::bind_rows(quality, quality[1, ])
  class(duplicate) <- class(quality)
  expect_snapshot(error = TRUE, compare_mi(duplicate))
})

test_that("compare_mi rejects negative counts", {
  negative_count <- make_quality()
  negative_count$fbs[1] <- -1

  expect_snapshot(error = TRUE, compare_mi(negative_count))
})
