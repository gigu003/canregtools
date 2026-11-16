make_mv_quality <- function() {
  cases <- c(100, 100, 150, 200)
  mv_cases <- c(95, 70, 120, 150)
  structure(
    dplyr::tibble(
      regicode = c("A", "B", "C", "D"),
      year = 2024L,
      sex = 1L,
      cancer = "101",
      fbs = cases,
      mv = mv_cases / cases * 100
    ),
    class = c("quality", "tbl_df", "tbl", "data.frame")
  )
}

test_that("compare_mv reproduces the Bray-Parkin statistic", {
  quality <- make_mv_quality()
  result <- compare_mv(quality)
  target <- result[result$regicode == "A", ]

  verified <- c(70, 120, 150)
  cases <- c(100, 150, 200)
  p <- sum(verified) / sum(cases)
  phi <- sum(
    (verified - p * cases)^2 / (cases * p * (1 - p))
  ) /
    2
  z2 <- (95 - p * 100)^2 / (phi * p * (1 - p) * 100)

  expect_equal(target$mv_cases, 95)
  expect_equal(target$reference_n, 3L)
  expect_equal(target$p, p)
  expect_equal(target$phi, phi)
  expect_equal(target$z2, z2)
  expect_equal(target$p_value, stats::pchisq(z2, 1, lower.tail = FALSE))
  expect_equal(target$critical_value, stats::qchisq(0.95, 1))
  expect_identical(target$unusual, TRUE)
  expect_identical(target$status, "ok")
})

test_that("compare_mv accepts collapsed and uncollapsed quality results", {
  quality <- make_mv_quality()
  qualities <- split(quality, quality$regicode)
  qualities <- lapply(qualities, function(x) {
    structure(x, class = class(quality))
  })
  class(qualities) <- c("qualities", "list")

  expect_equal(compare_mv(qualities), compare_mv(quality))
})

test_that("compare_mv accepts an external reference table", {
  target <- make_mv_quality()[1, ]
  reference <- dplyr::tibble(
    registry = c("R1", "R2", "R3"),
    year = 2024L,
    sex = 1L,
    cancer = "101",
    cases = c(100, 150, 200),
    mv = c(70, 80, 75)
  )

  result <- compare_mv(target, reference = reference)
  expected <- compare_mv(make_mv_quality())
  expected <- expected[expected$regicode == "A", ]

  expect_equal(result$reference_n, 3L)
  expect_equal(result$p, expected$p)
  expect_equal(result$phi, expected$phi)
  expect_equal(result$z2, expected$z2)
  expect_identical(result$status, "ok")
})

test_that("compare_mv accepts exact external MV counts", {
  target <- make_mv_quality()[1, ]
  reference <- dplyr::tibble(
    regicode = c("R1", "R2", "R3"),
    fbs = c(100, 150, 200),
    mv_cases = c(70, 120, 150)
  )

  result <- compare_mv(target, reference = reference)

  expect_equal(result$reference_n, 3L)
  expect_identical(result$status, "ok")
})

test_that("compare_mv rejects an incidence-only external standard", {
  reference <- dplyr::tibble(
    registry = c("R1", "R2", "R3"),
    cases = c(100, 150, 200),
    person_years = c(100000, 150000, 200000)
  )

  expect_snapshot(
    error = TRUE,
    compare_mv(
      make_mv_quality()[1, ],
      reference = reference
    )
  )
})

test_that("compare_mv detects strata automatically", {
  first <- make_mv_quality()
  second <- first
  second$cancer <- "102"
  second$mv <- c(60, 50, 40, 30)
  quality <- dplyr::bind_rows(first, second)
  class(quality) <- class(first)

  result <- compare_mv(quality)

  expect_equal(nrow(result), 8L)
  expect_setequal(result$cancer, c("101", "102"))
  expect_equal(length(unique(result$p)), 8L)
})

test_that("compare_mv uses exact MV counts when supplied", {
  quality <- make_mv_quality()
  quality$mv_cases <- c(90, 70, 120, 150)

  result <- compare_mv(quality)

  expect_equal(result$mv_cases, quality$mv_cases)
  expect_equal(result$mv, quality$mv)
})

test_that("compare_mv does not use the mvs quality indicator", {
  quality <- make_mv_quality()
  expected <- compare_mv(quality)
  quality$mvs <- c(0, 25, 50, 100)

  expect_equal(compare_mv(quality), expected)
})

test_that("compare_mv uses the requested significance level", {
  result <- compare_mv(make_mv_quality(), alpha = 0.01)

  expect_equal(unique(result$critical_value), stats::qchisq(0.99, 1))
  expect_identical(
    result$unusual,
    result$z2 >= stats::qchisq(0.99, 1)
  )
})

test_that("compare_mv reports groups that cannot be evaluated", {
  insufficient <- make_mv_quality()[1:2, ]
  zero_target <- make_mv_quality()
  zero_target$fbs[1] <- 0
  zero_target$mv[1] <- NA
  zero_reference <- zero_target
  boundary <- make_mv_quality()
  boundary$mv[] <- 100
  zero_dispersion <- make_mv_quality()
  zero_dispersion$mv[] <- 50

  expect_setequal(
    compare_mv(insufficient)$status,
    "insufficient_reference_registries"
  )
  expect_identical(compare_mv(zero_target)$status[1], "zero_target_cases")
  expect_equal(compare_mv(zero_target)$mv_cases[1], 0)
  expect_setequal(compare_mv(zero_reference)$status[-1], "zero_reference_cases")
  expect_setequal(
    compare_mv(boundary)$status,
    "boundary_reference_proportion"
  )
  expect_setequal(
    compare_mv(zero_dispersion)$status,
    "zero_reference_dispersion"
  )

  zero_boundary <- make_mv_quality()
  zero_boundary$mv[] <- 0
  expect_setequal(
    compare_mv(zero_boundary)$status,
    "boundary_reference_proportion"
  )
})

test_that("compare_mv validates its inputs", {
  quality <- make_mv_quality()

  expect_snapshot(error = TRUE, compare_mv(data.frame()))
  expect_snapshot(error = TRUE, compare_mv(quality, alpha = 0))
  expect_snapshot(error = TRUE, compare_mv(quality[0, ]))
  expect_snapshot(
    error = TRUE,
    compare_mv(structure(quality[, -6], class = class(quality)))
  )

  invalid_percent <- quality
  invalid_percent$mv[1] <- 101
  expect_snapshot(error = TRUE, compare_mv(invalid_percent))

  invalid_count <- quality
  invalid_count$mv_cases <- c(101, 70, 120, 150)
  expect_snapshot(error = TRUE, compare_mv(invalid_count))

  duplicate <- dplyr::bind_rows(quality, quality[1, ])
  class(duplicate) <- class(quality)
  expect_snapshot(error = TRUE, compare_mv(duplicate))
})
