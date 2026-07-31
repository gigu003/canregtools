test_that("draw_pyramid draws faceted population pyramids", {
  age_groups <- c(
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
    "75-79",
    "80+"
  )
  pop <- expand.grid(
    agegrp = age_groups,
    sex = c("Male", "Female"),
    year = c(2020, 2025)
  )
  pop$rks <- seq(5000, by = 1000, length.out = nrow(pop))

  result <- with_test_pdf_device(
    draw_pyramid(
      pop,
      agegrp,
      rks,
      sex,
      year,
      cgap = 0.2,
      show_value = TRUE
    )
  )

  expect_null(result)
})

test_that("draw_pyramid supports raw counts without facets", {
  pop <- expand.grid(
    agegrp = c("0-4", "5-9", "10-14"),
    sex = c("Male", "Female")
  )
  pop$rks <- c(10, 20, 30, 12, 22, 32)

  result <- with_test_pdf_device(
    draw_pyramid(pop, agegrp, rks, sex, show_prop = FALSE, show_value = TRUE)
  )

  expect_null(result)
})
