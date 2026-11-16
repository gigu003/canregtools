test_that("cr_merge.canregs preserves character regicodes in merged data", {
  reg_a <- canregs[[1]]
  reg_b <- canregs[[2]]
  reg_a$regicode <- "REG_A"
  reg_b$regicode <- "SITE-02"

  regs <- structure(
    list(first = reg_a, second = reg_b),
    class = c("canregs", "list")
  )

  merged <- cr_merge(regs)
  expect_s3_class(merged, "canreg")
  expect_identical(merged$regicode, "merged:REG_A+SITE-02")
  expect_identical(merged$source_regicodes, c("REG_A", "SITE-02"))
  expect_type(merged$FBcases$regicode, "character")
  expect_setequal(unique(merged$FBcases$regicode), c("REG_A", "SITE-02"))
  expect_s3_class(merged$POP, "POP")

  counted <- count_canreg(regs)
  merged_fbsw <- cr_merge(counted)
  expect_s3_class(merged_fbsw, "fbswicd")
  expect_type(merged_fbsw$regicode, "character")
  expect_setequal(merged_fbsw$regicode, c("REG_A", "SITE-02"))
  expect_type(merged_fbsw$exclude$regicode, "character")
})

test_that("cr_merge combines derived list classes into data frames", {
  asrs <- create_asr(fbsws, year, sex, collapse = FALSE)
  qualities <- create_quality(fbsws, year, sex, collapse = FALSE)
  age_rates <- create_age_rate(fbsws, year, sex, collapse = FALSE)

  expect_s3_class(cr_merge(asrs), "asr")
  expect_s3_class(cr_merge(qualities), "quality")
  expect_s3_class(cr_merge(age_rates), "age_rate")
  expect_contains(names(cr_merge(asrs)), "regicode")
})

test_that("cr_merge preserves optional fbswicd component availability", {
  counted <- count_canreg(canregs[[1]])
  absent <- as_fbswicd(counted$fbswicd, counted$pop, regicode = "A")
  absent_set <- structure(list(A = absent), class = c("fbswicds", "list"))
  merged_absent <- cr_merge(absent_set)
  expect_named(merged_absent, c("regicode", "fbswicd", "pop"))

  empty <- as_fbswicd(
    counted$fbswicd,
    counted$pop,
    sitemorp = counted$sitemorp[FALSE, ],
    exclude = counted$exclude[FALSE, ],
    regicode = "B"
  )
  mixed <- structure(
    list(A = absent, B = empty),
    class = c("fbswicds", "list")
  )
  merged_mixed <- cr_merge(mixed)
  expect_named(
    merged_mixed,
    c("regicode", "fbswicd", "sitemorp", "pop", "exclude")
  )
  expect_equal(nrow(merged_mixed$sitemorp), 0L)
  expect_equal(nrow(merged_mixed$exclude), 0L)
})

test_that("optional survival cases are preserved independently of registry order", {
  a <- canregs[[1]]
  b <- canregs[[2]]
  a$SFcases <- NULL
  b$SFcases <- b$FBcases
  merge_regs <- function(regs) {
    cr_merge(structure(regs, class = c("canregs", "list")))
  }
  forward <- merge_regs(list(a, b))
  reverse <- merge_regs(list(b, a))
  expect_equal(forward$SFcases, reverse$SFcases)
  expect_equal(nrow(forward$SFcases), nrow(b$SFcases))
  expect_identical(unique(forward$SFcases$regicode), b$regicode)
  b$SFcases <- b$SFcases[FALSE, ]
  expect_equal(nrow(merge_regs(list(a, b))$SFcases), 0L)
})

test_that("fbswicd merges can be nested without changing counts or provenance", {
  merge_counted <- function(regs) {
    cr_merge(structure(regs, class = c("fbswicds", "list")))
  }
  direct <- merge_counted(unclass(fbsws))
  partial <- merge_counted(unclass(fbsws)[1:2])
  nested <- merge_counted(list(partial, fbsws[[3]]))
  reverse <- merge_counted(list(fbsws[[3]], partial))
  doubled <- merge_counted(list(partial, partial))
  expect_equal(nested$fbswicd, direct$fbswicd)
  expect_equal(nested$pop, direct$pop)
  expect_equal(reverse$fbswicd, direct$fbswicd)
  expect_setequal(nested$regicode, direct$regicode)
  expect_equal(nested$exclude, direct$exclude)
  expect_equal(
    dplyr::arrange(reverse$exclude, regicode),
    dplyr::arrange(direct$exclude, regicode)
  )
  expect_equal(sum(doubled$fbswicd$fbs), 2 * sum(partial$fbswicd$fbs))
})
