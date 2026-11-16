sparse_age_fixture <- function() {
  ages <- levels(fbsws[[1]]$fbswicd$agegrp)
  counts <- expand.grid(
    year = "2021",
    sex = c("1", "2"),
    cancer = "101",
    agegrp = ages,
    stringsAsFactors = FALSE
  )
  counts$agegrp <- factor(counts$agegrp, levels = ages)
  counts$fbs <- as.numeric(counts$agegrp == ages[1])
  counts$mv <- counts$fbs
  for (name in c("sws", "dco", "mvs", "ub", "sub")) {
    counts[[name]] <- 0
  }
  population <- unique(counts[c("year", "sex", "agegrp")])
  population$rks <- 1000
  list(
    dense = as_fbswicd(counts, population, regicode = "TEST"),
    sparse = as_fbswicd(counts[counts$fbs > 0, ], population, regicode = "TEST")
  )
}

test_that("sparse age tables give the same rates and uncertainty as explicit zeros", {
  x <- sparse_age_fixture()
  expected <- create_asr(
    x$dense,
    year,
    sex,
    cancer,
    show_pop = TRUE,
    show_var = TRUE,
    show_se = TRUE,
    show_ci = TRUE
  )
  result <- create_asr(
    x$sparse,
    year,
    sex,
    cancer,
    show_pop = TRUE,
    show_var = TRUE,
    show_se = TRUE,
    show_ci = TRUE
  )
  expect_equal(result, expected)
  expect_equal(result$no_cases[result$sex == "0"], 2)
  expect_equal(result$pop[result$sex == "0"], 38000)
  expect_equal(result$cr[result$sex == "0"], 5.26)
  expect_equal(result$asr_wld85[result$sex == "0"], 2.4)
  quality <- create_quality(x$sparse, year, sex, cancer)
  expect_equal(quality, create_quality(x$dense, year, sex, cancer))
  expect_equal(quality$rks[quality$sex == "0"], 38000)
  expect_equal(quality$inci[quality$sex == "0"], 5.26)
  for (format in c("long", "wide")) {
    expect_equal(
      create_age_rate(
        x$sparse,
        year,
        sex,
        cancer,
        format = format,
        show_pop = TRUE
      ),
      create_age_rate(
        x$dense,
        year,
        sex,
        cancer,
        format = format,
        show_pop = TRUE
      )
    )
  }
  wide <- create_age_rate(
    x$sparse,
    year,
    sex,
    cancer,
    format = "wide",
    show_pop = TRUE
  )
  expect_equal(wide$p0[wide$sex == "0"], 38000)
  expect_equal(wide$r0[wide$sex == "0"], 5.263158)
})

test_that("completion precedes collapsing years and preserves unobserved strata", {
  x <- sparse_age_fixture()
  second <- x$sparse
  second$fbswicd$year <- second$pop$year <- "2022"
  combined <- as_fbswicd(
    rbind(x$sparse$fbswicd, second$fbswicd),
    rbind(x$sparse$pop, second$pop),
    regicode = "TEST"
  )
  quality <- create_quality(combined, sex, cancer)
  expect_equal(quality$fbs[quality$sex == "0"], 4)
  expect_equal(quality$rks[quality$sex == "0"], 76000)
  expect_equal(quality$inci[quality$sex == "0"], 5.26)
  missing_year <- combined
  missing_year$fbswicd <- missing_year$fbswicd[
    missing_year$fbswicd$year == "2021",
  ]
  expect_identical(
    unique(create_quality(missing_year, year, sex, cancer)$year),
    "2021"
  )
})

test_that("completion respects population age ordering for character inputs", {
  x <- sparse_age_fixture()
  x$sparse$fbswicd$agegrp <- as.character(x$sparse$fbswicd$agegrp)
  x$sparse$pop$agegrp <- as.character(x$sparse$pop$agegrp)
  x$sparse$pop <- x$sparse$pop[rev(seq_len(nrow(x$sparse$pop))), ]
  result <- create_asr(x$sparse, year, sex, cancer, show_pop = TRUE)
  expected <- create_asr(x$dense, year, sex, cancer, show_pop = TRUE)
  expect_equal(result, expected)
})

test_that("explicit missing counts and absent population ages are not zero-filled", {
  x <- sparse_age_fixture()$sparse
  missing_count <- x
  missing_count$fbswicd$fbs[1] <- NA_real_
  expect_snapshot(
    error = TRUE,
    create_quality(missing_count, year, sex, cancer)
  )
  expect_snapshot(error = TRUE, create_asr(missing_count, year, sex, cancer))
  missing_population <- x
  missing_population$pop <- missing_population$pop[
    as.integer(missing_population$pop$agegrp) != 10L,
  ]
  expect_snapshot(
    error = TRUE,
    create_asr(missing_population, year, sex, cancer)
  )
})
