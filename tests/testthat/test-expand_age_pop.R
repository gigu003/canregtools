aggregate_expanded_population <- function(
  expanded,
  breaks = attr(expanded, "source_breaks")
) {
  vapply(
    seq_along(breaks),
    function(i) {
      in_group <- if (i == length(breaks)) {
        expanded$x >= breaks[i]
      } else {
        expanded$x >= breaks[i] & expanded$x < breaks[i + 1L]
      }
      sum(expanded$y[in_group])
    },
    numeric(1)
  )
}

test_that("expand_age_pop supports arbitrary source breaks and retains the open group", {
  grouped <- c(50, 120, 80, 40)
  breaks <- c(0, 1, 5, 15)
  result <- expand_age_pop(grouped, source_breaks = breaks)

  expect_named(
    result,
    c("x", "y", "age_end", "open", "source_group", "lower", "upper")
  )
  expect_equal(result$x, 0:15)
  expect_equal(result$age_end, c(0:14, Inf))
  expect_equal(result$open, c(rep(FALSE, 15), TRUE))
  expect_equal(aggregate_expanded_population(result), grouped)
  expect_equal(sum(result$y < 0), 0)
})

test_that("cubic interpolation falls back before producing negative estimates", {
  grouped <- c(
    2780,
    11168,
    17051,
    17275,
    16128,
    16137,
    15106,
    15892,
    15370,
    15623,
    14477,
    13623,
    13925,
    13769,
    12922,
    10071,
    5841,
    1186,
    811
  )

  result <- expect_no_warning(expand_age_pop(grouped, method = "natural"))
  linear <- expand_age_pop(grouped, method = "linear")

  expect_equal(result$y, linear$y)
  expect_equal(aggregate_expanded_population(result), grouped)
  expect_equal(sum(result$y < 0), 0)
})

test_that("expand_age_pop accepts the legacy positional method argument", {
  grouped <- c(1000, 4000, seq(5000, 12500, by = 500), 2500)

  expect_equal(
    expand_age_pop(grouped, "sprague"),
    expand_age_pop(grouped, method = "sprague")
  )
})

test_that("reference allocation uses reference proportions and preserves source totals", {
  reference <- data.frame(
    age = 0:14,
    population = c(5, 15, 10, 20, 25, rep(10, 10))
  )
  grouped <- c(75, 100, 30)
  result <- expand_age_pop(
    grouped,
    source_breaks = c(0, 5, 10),
    method = "reference",
    reference = reference
  )

  expect_equal(result$y[1:5], c(5, 15, 10, 20, 25))
  expect_equal(aggregate_expanded_population(result), grouped)
  expect_equal(sum(result$y < 0), 0)
})

test_that("Sprague automatically combines age 0 and 1--4", {
  grouped <- c(1000, 4000, seq(5000, 12500, by = 500), 2500)
  result <- expand_age_pop(grouped, method = "sprague")

  expect_equal(attr(result, "source_breaks"), c(0, seq(5, 85, by = 5)))
  expect_equal(
    aggregate_expanded_population(result),
    c(sum(grouped[1:2]), grouped[3:19])
  )
  expect_equal(result$age_end[nrow(result)], Inf)
  expect_equal(result$y[nrow(result)], grouped[19])
  expect_equal(sum(result$y < 0), 0)
})

test_that("Sprague rejects non-regular closed age groups", {
  expect_snapshot(
    error = TRUE,
    expand_age_pop(
      c(100, 100, 100, 100),
      source_breaks = c(0, 4, 9, 14),
      method = "sprague"
    )
  )
})

test_that("open-age expansion keeps or reference-splits the open group", {
  kept <- expand_open_age_pop(100, lower = 85)
  reference <- setNames(seq(20, 1, length.out = 16), 85:100)
  split <- expand_open_age_pop(
    100,
    lower = 85,
    method = "reference",
    reference = reference,
    max_age = 95
  )

  expect_equal(
    kept,
    data.frame(
      x = 85,
      y = 100,
      age_end = Inf,
      open = TRUE,
      lower = NA_real_,
      upper = NA_real_
    )
  )
  expect_equal(sum(split$y), 100)
  expect_equal(split$age_end[nrow(split)], Inf)
  expect_equal(split$open[nrow(split)], TRUE)
})

test_that("age splitting handles zero and minimal closed groups", {
  result <- expand_age_pop(
    c(0, 25),
    source_breaks = c(0, 5),
    method = "linear"
  )

  expect_equal(result$x, 0:5)
  expect_equal(result$y, c(rep(0, 5), 25))
  expect_equal(result$age_end, c(0:4, Inf))
  expect_equal(result$open, c(rep(FALSE, 5), TRUE))
  expect_equal(sum(result$y), 25)
})

test_that("reference age splitting preserves closed and open boundary totals", {
  reference <- setNames(c(rep(1, 85), 3, 2, 1), 0:87)
  result <- expand_age_pop(
    c(50, 100, 40),
    source_breaks = c(0, 5, 85),
    method = "reference",
    reference = reference,
    high_age = list(
      method = "reference",
      reference = reference,
      max_age = 87
    )
  )

  expect_equal(
    aggregate_expanded_population(result),
    c(50, 100, 40)
  )
  expect_equal(result$x[c(1, 5, 85, 86, 87)], c(0, 4, 84, 85, 86))
  expect_equal(
    result$age_end[c(5, 84, 85, nrow(result))],
    c(4, 83, 84, Inf)
  )
  expect_equal(result$open[nrow(result)], TRUE)
})

test_that("age splitting rejects invalid boundary definitions", {
  expect_snapshot(
    error = TRUE,
    expand_age_pop(c(1, 2), source_breaks = c(1, 5))
  )
  expect_snapshot(
    error = TRUE,
    expand_age_pop(c(1, 2), source_breaks = c(0, 5.5))
  )
  expect_snapshot(
    error = TRUE,
    expand_open_age_pop(1, lower = 85, method = "reference", max_age = 85)
  )
})

test_that("benchmark_age_split reports blind-restoration diagnostics", {
  truth <- setNames(round(5000 * exp(-(0:100) / 45)), 0:100)
  cases <- setNames(round(100 + (0:100)^2 / 20), 0:100)
  result <- benchmark_age_split(
    population = truth,
    source_breaks = c(0, seq(5, 85, by = 5)),
    methods = c("linear", "sprague"),
    cases = cases
  )

  expect_named(
    result,
    c(
      "method",
      "mae",
      "rmse",
      "total_variation",
      "asr_true",
      "asr_estimate",
      "asr_bias",
      "max_group_error",
      "n_negative",
      "uncertainty_method",
      "interval_coverage",
      "mean_interval_width"
    )
  )
  expect_equal(result$max_group_error, c(0, 0))
  expect_equal(result$n_negative, c(0, 0))
  expect_equal(all(is.finite(result$asr_bias)), TRUE)
  expect_type(attr(result, "estimates"), "list")
  expect_equal(attr(result, "group_errors")$error, rep(0, 36))
})

test_that("PCLM is an optional dependency", {
  if (requireNamespace("ungroup", quietly = TRUE)) {
    skip("ungroup is installed")
  }
  expect_snapshot(
    error = TRUE,
    expand_age_pop(
      c(100, 200, 300),
      source_breaks = c(0, 5, 10),
      method = "pclm"
    )
  )
})
