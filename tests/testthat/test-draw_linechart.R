test_that("draw_linechart basic plot works", {
  fbsw <- count_canreg(canregs[[1]], label_tail = "yrs")
  agerate <- create_age_rate(fbsw, year, sex)
  agerate <- add_labels(agerate, lang = "en")

  result <- with_test_pdf_device(
    draw_linechart(agerate, agegrp, rate, sex)
  )

  expect_null(result)
})

test_that("draw_linechart with facet variable works", {
  fbsw <- count_canreg(canregs[[1]], label_tail = "yrs")
  agerate <- create_age_rate(fbsw, year, sex, cancer)
  agerate <- add_labels(agerate, lang = "en")
  agerate <- dplyr::filter(agerate, cancer %in% as.character(c(103:104)))

  result <- with_test_pdf_device(
    draw_linechart(agerate, agegrp, rate, sex, facet = cancer, grid = c(1, 2))
  )

  expect_null(result)
})


test_that("custom axes and labels are handled correctly", {
  agegrp <- factor(
    c(
      "0-4",
      "5-9",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75+"
    ),
    levels = c(
      "0-4",
      "5-9",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75+"
    )
  )
  sex <- c("Male", "Female")
  cancer <- c("Lung", "Stomach", "Breast", "Liver")
  df <- expand.grid(
    agegrp = agegrp,
    sex = sex,
    cancer = cancer
  )
  df$rate <- seq_len(nrow(df))

  result <- with_test_pdf_device(
    draw_linechart(
      df,
      agegrp,
      rate,
      cancer,
      sex,
      y_axis = c(0, 160),
      axis_label = c("Custom X", "Custom Y"),
      grid = c(1, 2)
    )
  )

  expect_null(result)
})

test_that("draw_linechart validates input data and variables", {
  expect_snapshot(error = TRUE, {
    draw_linechart(list(x = 1:3, y = 1:3), x, y)
  })

  expect_snapshot(error = TRUE, {
    draw_linechart(data.frame(x = 1:3, y = 1:3), missing_x, y)
  })
})
