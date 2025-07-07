
test_that("draw_linechart basic plot works", {
  data("canregs", package = "canregtools")
  fbsw <- count_canreg(canregs[[1]], label_tail = "yrs")
  agerate <- create_age_rate(fbsw, year, sex)
  agerate <- add_labels(agerate, lang = "en")
  
  expect_silent(
    draw_linechart(agerate, agegrp, rate, sex)
  )
})

test_that("draw_linechart with facet variable works", {
  data("canregs", package = "canregtools")
  fbsw <- count_canreg(canregs[[1]], label_tail = "yrs")
  agerate <- create_age_rate(fbsw, year, sex, cancer)
  agerate <- add_labels(agerate, lang = "en")
  agerate <- dplyr::filter(agerate, cancer %in% as.character(c(103:104)))
  
  expect_silent(
    draw_linechart(agerate, agegrp, rate, sex,
                   facet = cancer,
                   grid = c(1, 2))
  )
})


test_that("custom axes and labels are handled correctly", {
  data <- data.frame(
    age = c(0, 5, 10),
    rate = c(10, 15, 12),
    sex = c("M", "F", "M")
  )
  expect_silent(
    draw_linechart(
      data, age, rate, sex,
      x_axis = c(0, 10), y_axis = c(0, 20),
      x_label = c("0y", "10y"), y_label = c("low", "high"),
      axis_label = c("Custom X", "Custom Y")
    )
  )
})

