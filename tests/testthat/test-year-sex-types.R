expect_year_sex_integer <- function(data) {
  if (!is.data.frame(data)) {
    return(invisible(NULL))
  }
  if ("year" %in% names(data)) {
    expect_type(data$year, "integer")
  }
  if ("sex" %in% names(data)) {
    expect_type(data$sex, "integer")
  }
  invisible(NULL)
}

expect_fbswicd_year_sex_integer <- function(x) {
  expect_year_sex_integer(x$fbswicd)
  expect_year_sex_integer(x$sitemorp)
  expect_year_sex_integer(x$pop)
  expect_year_sex_integer(x$exclude)
}

test_that("as_canreg standardizes population year and sex as integer", {
  data("canregs")
  src <- canregs[[1]]
  pop <- src$POP
  pop$year <- as.character(pop$year)
  pop$sex <- as.numeric(pop$sex)

  obj <- as_canreg(
    incidence = src$FBcases,
    mortality = src$SWcases,
    population = pop,
    areacode = src$areacode
  )

  expect_year_sex_integer(obj$POP)
})

test_that("cr_clean outputs integer year and sex columns", {
  data("canregs")
  cleaned <- cr_clean(canregs[[1]])

  expect_year_sex_integer(cleaned$FBcases)
  expect_year_sex_integer(cleaned$SWcases)
  expect_year_sex_integer(cleaned$POP)
})

test_that("count_canreg outputs integer year and sex columns", {
  data("canregs")
  fbsw <- count_canreg(canregs[[1]])

  expect_fbswicd_year_sex_integer(fbsw)
})

test_that("cr_merge outputs integer year and sex columns", {
  data("canregs")
  merged <- cr_merge(canregs)

  expect_year_sex_integer(merged$FBcases)
  expect_year_sex_integer(merged$SWcases)
  expect_year_sex_integer(merged$POP)

  merged_fbsw <- cr_merge(count_canreg(canregs))
  expect_fbswicd_year_sex_integer(merged_fbsw)
})

test_that("derived workflows output integer year and sex columns", {
  data("canregs")
  fbsw <- count_canreg(canregs[[1]])

  expect_year_sex_integer(create_asr(fbsw, year, sex, cancer))
  expect_year_sex_integer(create_age_rate(fbsw, year, sex, cancer))
  expect_year_sex_integer(create_quality(fbsw, year, sex, cancer))
  expect_year_sex_integer(create_site_morp(fbsw, year, sex, cancer))
})

test_that("canreg report data outputs integer year and sex columns", {
  data("canregs")
  report <- create_canreg_report_data(
    canregs,
    year = 2021,
    province = "河南省",
    include_age_registry = FALSE
  )

  purrr::walk(report$pop, expect_year_sex_integer)
  purrr::walk(report$quality, expect_year_sex_integer)
  purrr::walk(report$inci, expect_year_sex_integer)
  purrr::walk(report$mort, expect_year_sex_integer)
})
