make_fbswicd_components <- function() {
  counted <- count_canreg(canregs[[1]])
  unclass(counted)
}

test_that("as_fbswicd reconstructs the canonical count_canreg object", {
  x <- make_fbswicd_components()
  result <- as_fbswicd(
    fbsw = x$fbswicd,
    pop = x$pop,
    sitemorp = x$sitemorp,
    exclude = x$exclude,
    regicode = x$regicode
  )
  expect_s3_class(result, "fbswicd")
  expect_named(result, c("regicode", "fbswicd", "sitemorp", "pop", "exclude"))
  expect_equal(unclass(result), x)
  expect_s3_class(result$pop, "POP")
  expect_s3_class(create_asr(result, year, sex, cancer), "asr")
  expect_s3_class(create_quality(result, year, sex, cancer), "quality")
})

test_that("as_fbswicd distinguishes omitted and explicitly empty components", {
  x <- make_fbswicd_components()
  x$fbswicd$fbs <- as.character(x$fbswicd$fbs)
  x$pop$rks <- as.character(x$pop$rks)
  result <- as_fbswicd(x$fbswicd, x$pop, regicode = "REG_A")
  expect_identical(result$regicode, "REG_A")
  expect_type(result$fbswicd$fbs, "double")
  expect_type(result$pop$rks, "double")
  expect_named(result, c("regicode", "fbswicd", "pop"))
  pair <- structure(list(A = result, B = result), class = c("fbswicds", "list"))
  merged <- cr_merge(pair)
  expect_named(merged, c("regicode", "fbswicd", "pop"))

  empty_sitemorp <- x$sitemorp[FALSE, ]
  empty_exclude <- x$exclude[FALSE, ]
  explicit <- as_fbswicd(
    x$fbswicd,
    x$pop,
    sitemorp = empty_sitemorp,
    exclude = empty_exclude,
    regicode = "REG_A"
  )
  expect_named(explicit, c("regicode", "fbswicd", "sitemorp", "pop", "exclude"))
  expect_equal(nrow(explicit$sitemorp), 0L)
  expect_equal(nrow(explicit$exclude), 0L)
  expect_equal(nrow(create_site_morp(explicit)), 0L)
})

test_that("as_fbswicd requires only fbs among statistic columns", {
  x <- make_fbswicd_components()
  keys <- c("year", "sex", "cancer", "agegrp", "fbs")
  minimal <- x$fbswicd[keys]
  result <- as_fbswicd(minimal, x$pop, regicode = "REG_A")
  expect_named(result$fbswicd, keys)
  expect_s3_class(create_asr(result, year, sex, cancer), "asr")
  expect_snapshot(error = TRUE, create_quality(result, year, sex, cancer))

  pair <- structure(list(A = result, B = result), class = c("fbswicds", "list"))
  merged <- cr_merge(pair)
  expect_setequal(names(merged$fbswicd), keys)
  expect_equal(sum(merged$fbswicd$fbs), 2 * sum(result$fbswicd$fbs))
})

test_that("as_fbswicd splits registries and permits sparse optional components", {
  x <- make_fbswicd_components()
  add_code <- function(data, code) transform(data, registry_id = code)
  fbsw <- rbind(add_code(x$fbswicd, "REG_B"), add_code(x$fbswicd, "REG_A"))
  pop <- rbind(add_code(x$pop, "REG_B"), add_code(x$pop, "REG_A"))
  sitemorp <- add_code(x$sitemorp, "REG_A")
  exclude <- add_code(x$exclude, "REG_B")
  result <- as_fbswicd(
    fbsw,
    pop,
    sitemorp,
    exclude,
    regicode_var = "registry_id"
  )
  expect_s3_class(result, "fbswicds")
  expect_named(result, c("REG_B", "REG_A"))
  expect_identical(
    unname(purrr::map_chr(result, "regicode")),
    c("REG_B", "REG_A")
  )
  expect_equal(nrow(result$REG_B$sitemorp), 0L)
  expect_equal(nrow(result$REG_A$exclude), 0L)
  expect_equal(nrow(result$REG_A$sitemorp), nrow(x$sitemorp))
  expect_equal(nrow(result$REG_B$exclude), nrow(x$exclude))
  for (registry in result) {
    expect_equal(
      any(vapply(
        registry[-1],
        function(component) {
          "registry_id" %in% names(component)
        },
        logical(1)
      )),
      FALSE
    )
  }
})

test_that("one regicode_var value returns one fbswicd", {
  x <- make_fbswicd_components()
  x$fbswicd$registry_id <- "REG_A"
  x$pop$registry_id <- "REG_A"
  result <- as_fbswicd(x$fbswicd, x$pop, regicode_var = "registry_id")
  expect_s3_class(result, "fbswicd")
  expect_identical(result$regicode, "REG_A")
})

test_that("as_fbswicd reports malformed components and grouping", {
  x <- make_fbswicd_components()
  expect_snapshot(error = TRUE, as_fbswicd(x$fbswicd, x$pop))
  expect_snapshot(
    error = TRUE,
    as_fbswicd(x$fbswicd[, -1], x$pop, regicode = "A")
  )
  bad <- x$fbswicd
  bad$fbs[1] <- -1
  expect_snapshot(error = TRUE, as_fbswicd(bad, x$pop, regicode = "A"))
  bad_site <- x$sitemorp
  bad_site$icd10[[1]] <- "C34"
  expect_snapshot(
    error = TRUE,
    as_fbswicd(x$fbswicd, x$pop, bad_site, regicode = "A")
  )

  x$fbswicd$registry_id <- "A"
  x$pop$registry_id <- "B"
  expect_snapshot(
    error = TRUE,
    as_fbswicd(x$fbswicd, x$pop, regicode_var = "registry_id")
  )
  expect_snapshot(
    error = TRUE,
    as_fbswicd(x$fbswicd, x$pop, regicode = "A", regicode_var = "registry_id")
  )

  x$pop$registry_id <- "A"
  unknown <- transform(x$sitemorp, registry_id = "B")
  expect_snapshot(
    error = TRUE,
    as_fbswicd(
      x$fbswicd,
      x$pop,
      sitemorp = unknown,
      regicode_var = "registry_id"
    )
  )
})

test_that("as_fbswicd rejects non-finite optional counts", {
  x <- make_fbswicd_components()
  bad <- x$pop
  bad$death <- NA_real_
  bad$death[1] <- NaN
  expect_snapshot(error = TRUE, as_fbswicd(x$fbswicd, bad, regicode = "A"))
})

test_that("as_fbswicd still requires fbs and matching merge schemas", {
  x <- make_fbswicd_components()
  expect_snapshot(
    error = TRUE,
    as_fbswicd(
      x$fbswicd[setdiff(names(x$fbswicd), "fbs")],
      x$pop,
      regicode = "A"
    )
  )
  full <- as_fbswicd(x$fbswicd, x$pop, regicode = "A")
  minimal <- as_fbswicd(
    x$fbswicd[c("year", "sex", "cancer", "agegrp", "fbs")],
    x$pop,
    regicode = "B"
  )
  mixed <- structure(list(A = full, B = minimal), class = c("fbswicds", "list"))
  expect_snapshot(error = TRUE, cr_merge(mixed))
})
