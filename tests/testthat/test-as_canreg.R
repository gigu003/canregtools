test_that("as_canreg constructs a standard canreg object from separate inputs", {
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
  expect_contains(names(asr), "asr_cn2000")

  qua <- create_quality(obj, year, sex, cancer)
  expect_s3_class(qua, "quality")
  quality_cols <- c("fbs", "sws", "mi", "mv", "dco")
  expect_setequal(intersect(quality_cols, names(qua)), quality_cols)
})

test_that("as_canreg preserves non-numeric areacode as character", {
  src <- canregs[[1]]

  obj <- as_canreg(
    incidence = src$FBcases,
    mortality = src$SWcases,
    population = src$POP,
    areacode = "REG_A"
  )

  expect_identical(obj$areacode, "REG_A")

  fbsw <- count_canreg(obj)
  expect_identical(fbsw$areacode, "REG_A")
})

test_that("as_canreg derives incidence and mortality from registry data", {
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
  expect_equal(
    intersect(names(obj$FBcases), c("f_year", "s_year")),
    character()
  )
  expect_equal(unique(format(obj$FBcases$inciden, "%Y")), "2021")
  expect_equal(unique(format(obj$SWcases$deathda, "%Y")), "2021")
})

test_that("as_canreg validates required variables early", {
  src <- canregs[[1]]

  bad_incidence <- src$FBcases[, setdiff(names(src$FBcases), "icd10")]
  expect_snapshot(error = TRUE, {
    as_canreg(
      incidence = bad_incidence,
      mortality = src$SWcases,
      population = src$POP,
      areacode = src$areacode
    )
  })

  bad_population <- src$POP[, setdiff(names(src$POP), "rks")]
  expect_snapshot(error = TRUE, {
    as_canreg(
      incidence = src$FBcases,
      mortality = src$SWcases,
      population = bad_population,
      areacode = src$areacode
    )
  })
})
