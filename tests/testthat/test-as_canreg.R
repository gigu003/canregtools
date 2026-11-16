test_that("year selects incidence by diagnosis and mortality by death", {
  src <- canregs[[1]]
  records <- src$FBcases[1:3, ]
  records$registr <- c("died-in-2021", "diagnosed-in-2021", "missing-dates")
  records$inciden <- as.Date(c("2020-01-01", "2021-01-01", NA))
  records$deathda <- as.Date(c("2021-02-01", "2022-02-01", NA))
  for (selected in list(2021, "2021-2021")) {
    direct <- as_canreg(
      inci = records,
      mort = records,
      pop = src$POP,
      year = selected,
      regicode = "A"
    )
    raw <- as_canreg(
      regi_data = records,
      pop = src$POP,
      year = selected,
      regicode = "A"
    )
    expect_equal(direct, raw)
    expect_equal(direct$FBcases$registr, "diagnosed-in-2021")
    expect_equal(direct$SWcases$registr, "died-in-2021")
    expect_equal(direct$POP, src$POP)
  }
  outside <- as_canreg(
    inci = records,
    mort = records,
    pop = src$POP,
    year = 1900
  )
  expect_equal(nrow(outside$FBcases), 0L)
  expect_equal(nrow(outside$SWcases), 0L)
  records$registry_id <- "A"
  multiple <- rbind(records, transform(records, registry_id = "B"))
  population <- rbind(
    transform(src$POP, registry_id = "A"),
    transform(src$POP, registry_id = "B")
  )
  grouped <- as_canreg(
    inci = multiple,
    mort = multiple,
    pop = population,
    year = 2021,
    regicode_var = "registry_id"
  )
  expect_named(grouped, c("A", "B"))
  for (x in grouped) {
    expect_equal(x$FBcases$registr, "diagnosed-in-2021")
    expect_equal(x$SWcases$registr, "died-in-2021")
  }
})

test_that("as_canreg constructs a standard canreg object from separate inputs", {
  src <- canregs[[1]]

  obj <- as_canreg(
    inci = src$FBcases,
    mort = src$SWcases,
    pop = src$POP,
    regicode = src$regicode
  )

  expect_s3_class(obj, "canreg")
  expect_named(obj, c("regicode", "FBcases", "SWcases", "POP"))
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

test_that("as_canreg splits separate inputs into canregs by regicode_var", {
  src <- canregs[[1]]
  add_registry_id <- function(data, codes = c("REG_A", "REG_B")) {
    data$registry_id <- rep(codes, length.out = nrow(data))
    data
  }

  result <- as_canreg(
    inci = add_registry_id(src$FBcases),
    mort = add_registry_id(src$SWcases),
    pop = add_registry_id(src$POP),
    regicode_var = "registry_id"
  )

  expect_s3_class(result, "canregs")
  expect_named(result, c("REG_A", "REG_B"))
  expect_identical(
    vapply(result, function(x) x$regicode, character(1)),
    c(REG_A = "REG_A", REG_B = "REG_B")
  )
  expect_setequal(
    names(result[["REG_A"]]),
    c("regicode", "FBcases", "SWcases", "POP")
  )
  expect_setequal(
    intersect("registry_id", names(result[["REG_A"]]$FBcases)),
    character()
  )
  expect_setequal(
    intersect("registry_id", names(result[["REG_A"]]$SWcases)),
    character()
  )
  expect_setequal(
    intersect("registry_id", names(result[["REG_A"]]$POP)),
    character()
  )
})

test_that("as_canreg returns canreg for one regicode_var value", {
  src <- canregs[[1]]
  add_registry_id <- function(data) {
    data$registry_id <- "REG_A"
    data
  }

  result <- as_canreg(
    inci = add_registry_id(src$FBcases),
    mort = add_registry_id(src$SWcases),
    pop = add_registry_id(src$POP),
    regicode_var = "registry_id"
  )

  expect_s3_class(result, "canreg")
  expect_identical(result$regicode, "REG_A")
})

test_that("as_canreg splits regi_data before applying year filters", {
  src <- canregs[[1]]
  regi_data <- src$FBcases
  regi_data$registry_id <- rep(
    c("REG_A", "REG_B"),
    length.out = nrow(regi_data)
  )
  population <- src$POP
  population$registry_id <- rep(
    c("REG_A", "REG_B"),
    length.out = nrow(population)
  )

  result <- as_canreg(
    regi_data = regi_data,
    pop = population,
    year = 2021,
    regicode_var = "registry_id"
  )

  expect_s3_class(result, "canregs")
  expect_named(result, c("REG_A", "REG_B"))
  for (registry in names(result)) {
    expect_setequal(
      unique(format(result[[registry]]$FBcases$inciden, "%Y")),
      "2021"
    )
    expect_setequal(
      unique(format(result[[registry]]$SWcases$deathda, "%Y")),
      "2021"
    )
  }
})

test_that("as_canreg preserves non-numeric regicode as character", {
  src <- canregs[[1]]

  obj <- as_canreg(
    inci = src$FBcases,
    mort = src$SWcases,
    pop = src$POP,
    regicode = "REG_A"
  )

  expect_identical(obj$regicode, "REG_A")

  fbsw <- count_canreg(obj)
  expect_identical(fbsw$regicode, "REG_A")
})

test_that("as_canreg derives incidence and mortality from registry data", {
  src <- canregs[[1]]
  regi_data <- src$FBcases

  obj <- as_canreg(
    regi_data = regi_data,
    pop = src$POP,
    year = 2021,
    regicode = src$regicode
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

test_that("as_canreg accepts inclusive year periods for event filtering", {
  expect_equal(as_canreg_year_filter("2003-2007"), 2003:2007)
  expect_equal(
    as_canreg_year_filter(c("2003", "2005-2006")),
    c(2003, 2005, 2006)
  )
})

test_that("as_canreg validates regicode_var grouping inputs", {
  src <- canregs[[1]]
  add_registry_id <- function(data, codes = c("REG_A", "REG_B")) {
    data$registry_id <- rep(codes, length.out = nrow(data))
    data
  }

  expect_snapshot(error = TRUE, {
    as_canreg(
      inci = src$FBcases,
      mort = src$SWcases,
      pop = src$POP,
      regicode_var = "registry_id"
    )
  })

  expect_snapshot(error = TRUE, {
    as_canreg(
      inci = add_registry_id(src$FBcases),
      mort = add_registry_id(src$SWcases),
      pop = add_registry_id(src$POP, "REG_A"),
      regicode_var = "registry_id"
    )
  })

  incidence <- add_registry_id(src$FBcases)
  incidence$registry_id[[1]] <- NA_character_
  expect_snapshot(error = TRUE, {
    as_canreg(
      inci = incidence,
      mort = add_registry_id(src$SWcases),
      pop = add_registry_id(src$POP),
      regicode_var = "registry_id"
    )
  })

  incidence_empty <- add_registry_id(src$FBcases)
  incidence_empty$registry_id[[1]] <- ""
  expect_snapshot(error = TRUE, {
    as_canreg(
      inci = incidence_empty,
      mort = add_registry_id(src$SWcases),
      pop = add_registry_id(src$POP),
      regicode_var = "registry_id"
    )
  })

  expect_snapshot(error = TRUE, {
    as_canreg(
      inci = add_registry_id(src$FBcases),
      mort = add_registry_id(src$SWcases),
      pop = add_registry_id(src$POP),
      regicode = "REG_A",
      regicode_var = "registry_id"
    )
  })
})

test_that("as_canreg validates required variables early", {
  src <- canregs[[1]]

  bad_incidence <- src$FBcases[, setdiff(names(src$FBcases), "icd10")]
  expect_snapshot(error = TRUE, {
    as_canreg(
      inci = bad_incidence,
      mort = src$SWcases,
      pop = src$POP,
      regicode = src$regicode
    )
  })

  bad_population <- src$POP[, setdiff(names(src$POP), "rks")]
  expect_snapshot(error = TRUE, {
    as_canreg(
      inci = src$FBcases,
      mort = src$SWcases,
      pop = bad_population,
      regicode = src$regicode
    )
  })
})
