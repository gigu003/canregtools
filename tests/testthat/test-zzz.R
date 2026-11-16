sex_total_fixture <- function() {
  ages <- levels(fbsws[[1]]$fbswicd$agegrp)
  counts <- expand.grid(
    year = "2021",
    sex = c("1", "2"),
    cancer = "101",
    agegrp = ages,
    stringsAsFactors = FALSE
  )
  counts$agegrp <- factor(counts$agegrp, levels = ages)
  counts$fbs <- 1
  counts$mv <- 1
  for (name in c("sws", "dco", "mvs", "ub", "sub")) {
    counts[[name]] <- 0
  }
  pop <- unique(counts[c("year", "sex", "agegrp")])
  pop$rks <- 1000
  overall <- counts[counts$sex == "1", ]
  overall$sex <- "0"
  overall$fbs <- overall$mv <- 2
  overall_pop <- pop[pop$sex == "1", ]
  overall_pop$sex <- "0"
  overall_pop$rks <- 2000
  list(
    details = as_fbswicd(counts, pop, regicode = "TEST"),
    both = as_fbswicd(
      rbind(counts, overall),
      rbind(pop, overall_pop),
      regicode = "TEST"
    ),
    overall = as_fbswicd(overall, overall_pop, regicode = "TEST")
  )
}

test_that("overall-sex rows do not change rates, counts, or uncertainty", {
  fixtures <- sex_total_fixture()
  baseline <- create_asr(
    fixtures$details,
    year,
    sex,
    cancer,
    show_pop = TRUE,
    show_var = TRUE,
    show_se = TRUE,
    show_ci = TRUE
  )
  result <- create_asr(
    fixtures$both,
    year,
    sex,
    cancer,
    show_pop = TRUE,
    show_var = TRUE,
    show_se = TRUE,
    show_ci = TRUE
  )
  expect_equal(result, baseline)
  total <- result[result$sex == "0", ]
  expect_equal(total$no_cases, 38)
  expect_equal(total$pop, 38000)
  expect_equal(total$cr, 100)
  expect_equal(total$asr_wld85, 100)
  expect_equal(
    create_quality(fixtures$both, year, sex, cancer),
    create_quality(fixtures$details, year, sex, cancer)
  )
  for (format in c("long", "wide")) {
    expect_equal(
      create_age_rate(
        fixtures$both,
        year,
        sex,
        cancer,
        format = format,
        show_pop = TRUE
      ),
      create_age_rate(
        fixtures$details,
        year,
        sex,
        cancer,
        format = format,
        show_pop = TRUE
      )
    )
  }
})

test_that("overall-only summaries remain overall-only without duplication", {
  fixtures <- sex_total_fixture()
  total <- fixtures$overall
  asr <- create_asr(total, year, sex, cancer, show_pop = TRUE)
  expected <- create_asr(fixtures$details, year, sex, cancer, show_pop = TRUE)
  expected <- expected[expected$sex == "0", ]
  attr(asr, "groups") <- attr(expected, "groups") <- NULL
  expect_equal(asr, expected)
  expect_identical(asr$sex, "0")
  quality <- create_quality(total, year, sex, cancer)
  expect_identical(quality$sex, "0")
  expect_equal(quality$fbs, 38)
  expect_equal(quality$rks, 38000)
  rate <- create_age_rate(
    total,
    year,
    sex,
    cancer,
    format = "wide",
    show_pop = TRUE
  )
  expect_identical(rate$sex, "0")
  expect_equal(rate$f0, 38)
  expect_equal(rate$p0, 38000)
  expect_equal(rate$r0, 100)
})

test_that("overall rows are resolved before collapsing original year strata", {
  fixtures <- sex_total_fixture()
  second <- fixtures$details
  second$fbswicd$year <- second$pop$year <- "2022"
  complete <- as_fbswicd(
    rbind(fixtures$details$fbswicd, second$fbswicd),
    rbind(fixtures$details$pop, second$pop),
    regicode = "TEST"
  )
  mixed <- as_fbswicd(
    rbind(fixtures$both$fbswicd, second$fbswicd),
    rbind(fixtures$both$pop, second$pop),
    regicode = "TEST"
  )
  expect_equal(
    create_asr(mixed, sex, cancer, show_pop = TRUE),
    create_asr(complete, sex, cancer, show_pop = TRUE)
  )
  expect_equal(
    create_quality(mixed, sex, cancer),
    create_quality(complete, sex, cancer)
  )
  expect_equal(
    create_age_rate(mixed, sex, cancer),
    create_age_rate(complete, sex, cancer)
  )
  counts <- pre_deal_fbswicd(mixed, year, sex, cancer)
  population <- pre_deal_pop(mixed, year, sex)
  expect_identical(
    anyDuplicated(counts[c("year", "sex", "cancer", "agegrp")]),
    0L
  )
  expect_identical(anyDuplicated(population[c("year", "sex", "agegrp")]), 0L)
})

test_that("supplied totals can coexist with only one available sex", {
  fixtures <- sex_total_fixture()
  partial <- fixtures$both
  partial$fbswicd <- partial$fbswicd[partial$fbswicd$sex != "2", ]
  partial$pop <- partial$pop[partial$pop$sex != "2", ]
  quality <- create_quality(partial, year, sex, cancer)
  expect_equal(quality$fbs[quality$sex == "0"], 38)
  expect_equal(quality$rks[quality$sex == "0"], 38000)
  expect_setequal(quality$sex, c("0", "1"))
})

test_that("inconsistent supplied totals fail before calculation", {
  fixtures <- sex_total_fixture()
  bad_counts <- fixtures$both
  bad_counts$fbswicd$fbs[bad_counts$fbswicd$sex == "0"] <- 3
  bad_pop <- fixtures$both
  bad_pop$pop$rks[bad_pop$pop$sex == "0"] <- 3000
  expect_snapshot(error = TRUE, create_asr(bad_counts, year, sex, cancer))
  expect_snapshot(error = TRUE, create_quality(bad_pop, year, sex, cancer))
  expect_snapshot(error = TRUE, create_age_rate(bad_counts, year, sex, cancer))
})

test_that("recognized sex labels are normalized before resolving totals", {
  fixtures <- sex_total_fixture()
  labelled <- fixtures$both
  labels <- c("0" = "total", "1" = "male", "2" = "female")
  labelled$fbswicd$sex <- unname(labels[labelled$fbswicd$sex])
  labelled$pop$sex <- unname(labels[labelled$pop$sex])
  expect_equal(
    create_quality(labelled, year, sex, cancer),
    create_quality(fixtures$details, year, sex, cancer)
  )
})
