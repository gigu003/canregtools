expect_year_sex_character <- function(data) {
  if (!is.data.frame(data)) {
    return(invisible(NULL))
  }
  if ("year" %in% names(data)) {
    expect_type(data$year, "character")
  }
  if ("sex" %in% names(data)) {
    expect_type(data$sex, "character")
  }
  invisible(NULL)
}

expect_fbswicd_year_sex_character <- function(x) {
  expect_year_sex_character(x$fbswicd)
  expect_year_sex_character(x$sitemorp)
  expect_year_sex_character(x$pop)
  expect_year_sex_character(x$exclude)
}

test_that("as_canreg standardizes population year and sex as character", {
  data("canregs")
  src <- canregs[[1]]
  pop <- src$POP
  pop$year <- as.character(pop$year)
  pop$sex <- as.numeric(pop$sex)

  obj <- as_canreg(
    inci = src$FBcases,
    mort = src$SWcases,
    pop = pop,
    regicode = src$regicode
  )

  expect_year_sex_character(obj$POP)
})

test_that("cr_clean outputs character year and sex columns", {
  data("canregs")
  cleaned <- cr_clean(canregs[[1]])

  expect_year_sex_character(cleaned$FBcases)
  expect_year_sex_character(cleaned$SWcases)
  expect_year_sex_character(cleaned$POP)
})

test_that("count_canreg outputs character year and sex columns", {
  data("canregs")
  fbsw <- count_canreg(canregs[[1]])

  expect_fbswicd_year_sex_character(fbsw)
})

test_that("cr_merge outputs character year and sex columns", {
  data("canregs")
  merged <- cr_merge(canregs)

  expect_year_sex_character(merged$FBcases)
  expect_year_sex_character(merged$SWcases)
  expect_year_sex_character(merged$POP)

  merged_fbsw <- cr_merge(count_canreg(canregs))
  expect_fbswicd_year_sex_character(merged_fbsw)
})

test_that("derived workflows output character year and sex columns", {
  data("canregs")
  fbsw <- count_canreg(canregs[[1]])

  expect_year_sex_character(create_asr(fbsw, year, sex, cancer))
  expect_year_sex_character(create_age_rate(fbsw, year, sex, cancer))
  expect_year_sex_character(create_quality(fbsw, year, sex, cancer))
  expect_year_sex_character(create_site_morp(fbsw, year, sex, cancer))
})

test_that("canreg report data outputs character year and sex columns", {
  data("canregs")
  report <- create_canreg_report_data(
    canregs,
    year = 2021,
    province = "河南省",
    include_age_registry = FALSE
  )

  purrr::walk(report$pop, expect_year_sex_character)
  purrr::walk(report$quality, expect_year_sex_character)
  purrr::walk(report$inci, expect_year_sex_character)
  purrr::walk(report$mort, expect_year_sex_character)
})

test_that("population periods remain intact through registry workflows", {
  data("canregs")
  src <- canregs[[1]]
  pop <- src$POP
  pop$year <- "2003-2007"

  obj <- as_canreg(
    inci = src$FBcases,
    mort = src$SWcases,
    pop = pop,
    regicode = src$regicode
  )
  fbsw <- count_canreg(obj)

  expect_setequal(unique(obj$POP$year), "2003-2007")
  expect_setequal(unique(fbsw$pop$year), "2003-2007")
  expect_equal(nrow(validate_population(obj$POP)), 0L)
})
