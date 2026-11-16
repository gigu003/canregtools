make_valid_canreg <- function() {
  incidence <- data.frame(
    sex = c("1", "2"),
    birthda = as.Date(c("1980-01-01", "1970-01-01")),
    inciden = as.Date(c("2021-02-01", "2021-03-01")),
    deathda = as.Date(c(NA, "2022-01-01")),
    basi = c(7L, 5L),
    icd10 = c("C34.1", "C50.9"),
    morp = c("8140", "8500"),
    topo = c("C34.1", "C50.9")
  )
  mortality <- data.frame(
    sex = c("1", "2"),
    birthda = as.Date(c("1980-01-01", "1970-01-01")),
    deathda = as.Date(c("2021-06-01", "2022-01-01")),
    icd10 = c("C34.1", "C50.9")
  )
  population <- data.frame(
    year = rep(2021L, 4),
    sex = rep(c(1L, 2L), each = 2),
    agegrp = factor(rep(c(0, 5), 2)),
    rks = c(1000L, 900L, 950L, 850L)
  )

  as_canreg(
    inci = incidence,
    mort = mortality,
    pop = population,
    regicode = "TEST"
  )
}

test_that("validate_canreg accepts a valid registry", {
  registry <- make_valid_canreg()

  result <- validate_canreg(registry)

  expect_s3_class(result, "canreg_validation")
  expect_named(
    result,
    c("registry", "component", "row", "variable", "severity", "rule", "message")
  )
  expect_equal(nrow(result), 0)
  expect_identical(is_valid_canreg(result), TRUE)
  expect_identical(is_valid_canreg(registry), TRUE)
  expect_equal(summary(result)$n, c(0L, 0L))
})

test_that("merged registries satisfy the scalar identifier contract", {
  a <- make_valid_canreg()
  b <- a
  b$regicode <- "B+1"
  regs <- structure(list(a = a, b = b), class = c("canregs", "list"))
  merged <- cr_merge(regs)
  expect_identical(merged$regicode, "merged:TEST+B%2B1")
  expect_equal(nrow(validate_canreg(merged)), 0L)
  expect_identical(merged$source_regicodes, c("TEST", "B+1"))
  nested <- cr_merge(structure(
    list(merged = merged),
    class = c("canregs", "list")
  ))
  expect_identical(nested$regicode, merged$regicode)
  expect_identical(nested$source_regicodes, merged$source_regicodes)
  expect_equal(nested$FBcases$regicode, merged$FBcases$regicode)
  single <- cr_merge(regs[1] |> structure(class = c("canregs", "list")))
  expect_identical(single$regicode, a$regicode)
})

test_that("validate_cases returns all detectable case issues", {
  cases <- make_valid_canreg()$FBcases
  cases$sex[[1]] <- "unknown"
  cases$birthda[[1]] <- as.Date("2022-01-01")
  cases$icd10[[2]] <- "invalid"
  cases$morp[[2]] <- "81"
  cases$topo <- NULL

  result <- validate_cases(cases, type = "incidence")

  expect_s3_class(result, "canreg_validation")
  expect_setequal(
    unique(result$rule),
    c("required_column", "valid_sex", "date_order", "code_format")
  )
  expect_setequal(result$row[!is.na(result$row)], c(1L, 2L))
  expect_identical(is_valid_canreg(result), FALSE)
})

test_that("validate_population detects invalid and duplicate strata", {
  population <- make_valid_canreg()$POP
  population <- rbind(population, population[1, ])
  population$rks[[2]] <- -1L
  population$sex[[3]] <- 0L

  result <- validate_population(population)

  expect_setequal(
    unique(result$rule),
    c("valid_sex", "nonnegative", "duplicate_stratum")
  )
  duplicate_rows <- result$row[result$rule == "duplicate_stratum"]
  expect_setequal(duplicate_rows, c(1L, 5L))
})

test_that("validate_population accepts inclusive period labels", {
  population <- make_valid_canreg()$POP
  population$year <- "2003-2007"

  expect_equal(nrow(validate_population(population)), 0L)

  population$year[[1]] <- "2007-2003"
  expect_true(any(validate_population(population)$rule == "valid_year"))
})

test_that("validate_canreg keeps registry names for canregs", {
  registries <- structure(
    list(first = make_valid_canreg(), second = make_valid_canreg()),
    class = c("canregs", "list")
  )
  registries$second$POP$rks[[1]] <- -1L

  result <- validate_canreg(registries)

  expect_s3_class(result, "canreg_validation")
  expect_equal(unique(result$registry), "second")
  expect_equal(result$component, "POP")
})

test_that("canreg_validation has concise print and summary methods", {
  registry <- make_valid_canreg()
  registry$POP$rks[[1]] <- -1L
  result <- validate_canreg(registry)

  expect_snapshot(print(result))
  expect_equal(
    summary(result),
    dplyr::tibble(severity = c("error", "warning"), n = c(1L, 0L))
  )
})

test_that("validate_fbswicd accepts summary objects and collections", {
  single <- validate_fbswicd(fbsws[[1]])
  collection <- validate_fbswicd(fbsws)

  expect_s3_class(single, "canreg_validation")
  expect_equal(nrow(single), 0L)
  expect_equal(nrow(collection), 0L)
  expect_named(
    single,
    c("registry", "component", "row", "variable", "severity", "rule", "message")
  )
})

test_that("validate_fbswicd checks summary population coverage", {
  object <- fbsws[[1]]
  missing_key <- object$fbswicd[1, c("year", "sex", "agegrp")]
  object$pop <- object$pop[
    !(object$pop$year == missing_key$year &
      object$pop$sex == missing_key$sex &
      object$pop$agegrp == missing_key$agegrp),
  ]

  result <- validate_fbswicd(object)

  expect_equal(unique(result$rule), "population_coverage")
  expect_equal(unique(result$severity), "error")
  expect_setequal(
    result$row,
    which(
      object$fbswicd$year == missing_key$year &
        object$fbswicd$sex == missing_key$sex &
        object$fbswicd$agegrp == missing_key$agegrp
    )
  )
})

test_that("validate_fbswicd detects duplicate summary strata", {
  object <- fbsws[[1]]
  object$fbswicd <- rbind(object$fbswicd, object$fbswicd[1, ])

  result <- validate_fbswicd(object)

  expect_equal(unique(result$rule), "duplicate_stratum")
  expect_setequal(result$row, c(1L, nrow(object$fbswicd)))
})

test_that("validate_fbswicd preserves optional component semantics", {
  object <- fbsws[[1]]
  object$sitemorp <- object$sitemorp[0, ]
  object$exclude <- object$exclude[0, ]

  expect_equal(nrow(validate_fbswicd(object)), 0L)

  object$sitemorp <- fbsws[[1]]$sitemorp[1, ]
  object$sitemorp$year <- "2099"
  result <- validate_fbswicd(object)

  expect_equal(unique(result$rule), "summary_coverage")
  expect_equal(unique(result$severity), "warning")
})
