test_that("basic barchart works without group and facet", {
  data <- dplyr::tibble(
    cancer = LETTERS[1:5],
    cr = c(10, 30, 50, 20, 40)
  )
  expect_silent(draw_barchart(data, x = cancer, y = cr))
})

test_that("barchart with grouping works", {
  data <- dplyr::tibble(
    cancer = rep(LETTERS[1:5], 2),
    cr = c(10, 30, 50, 20, 40, 15, 25, 45, 10, 35),
    year = rep(c(2020, 2021), each = 5)
  )
  expect_silent(draw_barchart(data, x = cancer, y = cr, group = year, legend = TRUE))
})

test_that("barchart with grouping works", {
  data <- dplyr::tibble(
    cancer = rep(LETTERS[1:5], 2),
    cr = c(10, 30, 50, 20, 40, 15, 25, 45, 10, 35),
    year = rep(c(2020, 2021), each = 5)
  )
  expect_silent(draw_barchart(data, x = cancer, y = cr, group = year, legend = TRUE, rev_group = TRUE))
})

test_that("barchart with facetting works", {
  data <- dplyr::tibble(
    cancer = rep(LETTERS[1:4], 3),
    cr = sample(20:80, 12),
    year = rep(2020:2022, each = 4)
  )
  expect_silent(draw_barchart(data, x = cancer, y = cr, facet = year))
})

test_that("topn filter works correctly", {
  data <- dplyr::tibble(
    cancer = rep(LETTERS[1:10], 2),
    cr = sample(1:100, 20),
    year = rep(c(2020, 2021), each = 10)
  )
  expect_silent(draw_barchart(data, x = cancer, y = cr, group = year, topn = 5,
                              bar_side = 2, rev_group = TRUE))
})

test_that("custom colors and legend labels are applied", {
  data <- dplyr::tibble(
    cancer = rep(LETTERS[1:3], 2),
    cr = sample(10:50, 6),
    year = rep(c(2020, 2021), each = 3)
  )
  expect_silent(draw_barchart(
    data, x = cancer, y = cr, group = year,
    cols = c("red", "blue"),
    legend_label = c("Year 2020", "Year 2021"),
    legend = TRUE
  ))
})
