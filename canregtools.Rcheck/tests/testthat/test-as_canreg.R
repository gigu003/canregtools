test_that("as_canreg constructs a standard canreg object from separate inputs", {
  data("canregs")
  src <- canregs[[1]]

  obj <- as_canreg(
    incidence = src$FBcases,
    mortality = src$SWcases,
    population = src$POP,
    areacode = src$areacode
  )

  expect_s3_class(obj, "canreg")
  expect_named(obj, c("areacode", "FBcases", "SWcases", "POP"))
  expect_s3_class(obj$FBcases, "FBcases")
  expect_s3_class(obj$SWcases, "SWcases")
  expect_s3_class(obj$POP, "POP")

  fbsw <- count_canreg(obj)
  expect_s3_class(fbsw, "fbswicd")

  asr <- create_asr(obj, year, sex, cancer)
  expect_s3_class(asr, "asr")
  expect_true("asr_cn2000" %in% names(asr))

  qua <- create_quality(obj, year, sex, cancer)
  expect_s3_class(qua, "quality")
  expect_true(all(c("fbs", "sws", "mi", "mv", "dco") %in% names(qua)))
})

test_that("as_canreg derives incidence and mortality from registry data", {
  data("canregs")
  src <- canregs[[1]]
  regi_data <- src$FBcases

  obj <- as_canreg(
    regi_data = regi_data,
    population = src$POP,
    year = 2021,
    areacode = src$areacode
  )

  expect_s3_class(obj, "canreg")
  expect_s3_class(obj$FBcases, "FBcases")
  expect_s3_class(obj$SWcases, "SWcases")
  expect_s3_class(obj$POP, "POP")
  expect_false("f_year" %in% names(obj$FBcases))
  expect_false("s_year" %in% names(obj$FBcases))
  expect_true(all(format(obj$FBcases$inciden, "%Y") == "2021"))
  expect_true(all(format(obj$SWcases$deathda, "%Y") == "2021"))
})

test_that("as_canreg validates required variables early", {
  data("canregs")
  src <- canregs[[1]]

  bad_incidence <- src$FBcases[, setdiff(names(src$FBcases), "icd10")]
  expect_error(
    as_canreg(
      incidence = bad_incidence,
      mortality = src$SWcases,
      population = src$POP,
      areacode = src$areacode
    ),
    "Missing required variables in `incidence`: icd10",
    fixed = TRUE
  )

  bad_population <- src$POP[, setdiff(names(src$POP), "rks")]
  expect_error(
    as_canreg(
      incidence = src$FBcases,
      mortality = src$SWcases,
      population = bad_population,
      areacode = src$areacode
    ),
    "Missing required variables in `population`: rks",
    fixed = TRUE
  )
})