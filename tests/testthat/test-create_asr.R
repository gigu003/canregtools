test_that("create_asr works correctly for canreg and canregs", {
  # Test with canreg object
  data <- canregs[[1]]
  expect_s3_class(data, "canreg")
  expect_s3_class(canregs, "canregs")
  # Test with optional population, variance, standard error, and CI columns
  asr1 <- create_asr(fbsws, year, sex, cancer)
  expect_s3_class(asr1, "asr")
  expect_contains(
    names(asr1),
    c(
      "regicode",
      "year",
      "sex",
      "cancer",
      "no_cases",
      "cr",
      "asr_cn2000",
      "asr_wld85",
      "rank"
    )
  )
  expect_equal(grep("pop", names(asr1), value = TRUE), character())
  expect_equal(grep("_var", names(asr1), value = TRUE), character())
  expect_equal(grep("_se", names(asr1), value = TRUE), character())
  expect_equal(grep("_lower", names(asr1), value = TRUE), character())
  expect_equal(grep("_upper", names(asr1), value = TRUE), character())
  expect_gt(nrow(asr1), 0)
  expect_setequal(asr1$sex, c("0", "1", "2"))
})

test_that("create_asr accepts added standard populations", {
  asr <- suppressWarnings(
    create_asr(canregs[[1]], year, sex, cancer, std = "eur2013")
  )

  expect_contains(names(asr), "asr_eur2013")
})

test_that("create_asr can include uncertainty and population columns", {
  asr <- create_asr(
    fbsws[[1]],
    year,
    sex,
    cancer,
    show_pop = TRUE,
    show_var = TRUE,
    show_se = TRUE,
    show_ci = TRUE
  )

  expect_s3_class(asr, "asr")
  expect_contains(
    names(asr),
    c(
      "pop",
      "cr_var",
      "cr_se",
      "cr_lower",
      "cr_upper",
      "asr_var_cn2000",
      "asr_se_cn2000",
      "asr_lower_cn2000",
      "asr_upper_cn2000"
    )
  )
  expect_gt(sum(asr$pop), 0)
  expect_equal(all(asr$cr_lower <= asr$cr_upper), TRUE)
  expect_equal(asr$cr_se, round(sqrt(asr$cr_var), 2))
  expect_equal(
    asr$asr_se_cn2000,
    round(sqrt(asr$asr_var_cn2000), 2)
  )
})

test_that("show_se does not change show_var output", {
  with_se <- create_asr(fbsws[[1]], year, show_se = TRUE)
  with_var <- create_asr(fbsws[[1]], year, show_var = TRUE)

  expect_contains(names(with_se), c("cr_se", "asr_se_cn2000", "asr_se_wld85"))
  expect_equal(grep("_var", names(with_se), value = TRUE), character())
  expect_contains(
    names(with_var),
    c("cr_var", "asr_var_cn2000", "asr_var_wld85")
  )
  expect_equal(grep("_se", names(with_var), value = TRUE), character())
})

test_that("show_var uses the displayed rate scale", {
  result <- create_asr(
    fbsws[[1]],
    year,
    sex,
    cancer,
    std = "wld85",
    show_var = TRUE
  ) |>
    dplyr::filter(year == "2021", sex == "1", cancer == "103")
  cases <- fbsws[[1]]$fbswicd |>
    dplyr::filter(year == "2021", sex == "1", cancer == "103") |>
    dplyr::arrange(agegrp)
  population <- fbsws[[1]]$pop |>
    dplyr::filter(year == "2021", sex == "1") |>
    dplyr::arrange(agegrp)
  model <- ageadjust(
    cases$fbs,
    population$rks,
    stdpop = get_stdpop("wld85")
  )

  expect_equal(result$cr_var, model$cr_var * 100000^2)
  expect_equal(result$asr_var_wld85, model$asr_var * 100000^2)
})

test_that("create_asr skips registries without ASR population denominators", {
  input <- fbsws
  missing_population <- names(input)[[1]]
  input[[1]]$pop <- input[[1]]$pop[0, , drop = FALSE]

  result <- suppressWarnings(
    create_asr(input, year, collapse = FALSE)
  )

  expect_s3_class(result, "asrs")
  expect_length(result, length(input) - 1L)
  expect_false(missing_population %in% names(result))
})

test_that("create_asr supports five-year age groups with 0-4 combined", {
  fbsw <- count_canreg(canregs[[1]], age_breaks = seq(0, 85, by = 5))
  asr <- create_asr(fbsw, year, sex, cancer, std = "wld85", show_pop = TRUE)

  expect_equal(nlevels(fbsw$fbswicd$agegrp), 18L)
  expect_equal(nlevels(fbsw$pop$agegrp), 18L)

  observed <- asr |>
    dplyr::filter(year == "2021", sex == "1", cancer == "103")
  age_data <- fbsw$fbswicd |>
    dplyr::filter(year == "2021", sex == "1", cancer == "103") |>
    dplyr::arrange(agegrp)
  age_pop <- fbsw$pop |>
    dplyr::filter(year == "2021", sex == "1") |>
    dplyr::arrange(agegrp)
  expected <- ageadjust(
    age_data$fbs,
    age_pop$rks,
    stdpop = get_stdpop("wld85", sep_zero = FALSE)
  )

  expect_equal(observed$pop, sum(age_pop$rks))
  expect_equal(observed$asr_wld85, round(expected$asr, 2))
  expect_equal(
    observed$truncr_wld85,
    truncrate(
      age_data$fbs,
      age_pop$rks,
      stdpop = get_stdpop("wld85", sep_zero = FALSE),
      sep_zero = FALSE,
      mp = 100000,
      decimal = 2
    )
  )
  expect_equal(
    observed$cumur,
    round(
      sum(age_data$fbs[1:14] / age_pop$rks[1:14] * 5) * 100,
      2
    )
  )
})

test_that("create_asr uses exact standard-population regrouping", {
  breaks <- c(0, 1, seq(10, 80, by = 10), 85)
  fbsw <- count_canreg(canregs[[1]], age_breaks = breaks)
  asr <- suppressWarnings(
    create_asr(
      canregs[[1]],
      year,
      sex,
      cancer,
      std = "wld85",
      age_breaks = breaks
    )
  )
  observed <- asr |>
    dplyr::filter(year == "2021", sex == "1", cancer == "103")
  age_data <- fbsw$fbswicd |>
    dplyr::filter(year == "2021", sex == "1", cancer == "103") |>
    dplyr::arrange(agegrp)
  age_pop <- fbsw$pop |>
    dplyr::filter(year == "2021", sex == "1") |>
    dplyr::arrange(agegrp)
  expected <- ageadjust(
    age_data$fbs,
    age_pop$rks,
    stdpop = get_stdpop("wld85", age_breaks = breaks)
  )

  expect_equal(observed$asr_wld85, round(expected$asr, 2))
  expect_equal(is.na(observed$truncr_wld85), TRUE)
  expect_equal(is.na(observed$cumur), TRUE)
})
