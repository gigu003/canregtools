test_that("draw_pyramid", {
  set.seed(123)
  age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24",
                  "25-29", "30-34", "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64", "65-69", "70-74",
                  "75-79", "80+")
  pop <- expand.grid(
    agegrp = age_groups,
    sex = c("Male", "Female"),
    year = c(2020, 2025)
  )
  pop$rks <- round(runif(nrow(pop), min = 5000, max = 50000))
  expect_silent(draw_pyramid(pop, agegrp, rks, sex, year, cgap = 0.2, show_value = TRUE))
})


