test_that("duplicate IDs preserve grouped semantics across strata", {
  x <- canregs[[1]]
  rows <- rep(1L, 8)
  x$FBcases <- x$FBcases[rows, ]
  x$FBcases$sex <- rep(c("1", "2"), 4)
  x$FBcases$icd10 <- "C34"
  x$FBcases$birthda <- as.Date("1950-01-01")
  x$FBcases$inciden <- as.Date("2021-01-01")
  for (ids in list(
    c("a", "a", NA, NA, "b", "c", "c", ""),
    c(1, 1, NA, NaN, 2, 3, 3, 4),
    factor(c("a", "a", NA, NA, "b", "c", "c", ""))
  )) {
    x$FBcases$id <- ids
    result <- count_canreg(x)$fbswicd
    result <- result[result$cancer == "60", ]
    reference <- data.frame(id = ids, sex = x$FBcases$sex) |>
      dplyr::group_by(id) |>
      dplyr::mutate(ids = as.integer(!is.na(id) & dplyr::n() > 1)) |>
      dplyr::ungroup()
    for (sex in c("1", "2")) {
      expect_equal(
        sum(result$ids[result$sex == sex]),
        sum(reference$ids[reference$sex == sex])
      )
    }
    expect_equal(sum(result$ids), 4L)
  }
  x$FBcases$id <- rep("", 8)
  result <- count_canreg(x)$fbswicd
  expect_equal(sum(result$ids[result$cancer == "60"]), 8L)
})

test_that("excluded mortality records retain mortality identifiers", {
  x <- canregs[[1]]
  x$FBcases$registr <- paste0("FB", seq_len(nrow(x$FBcases)))
  x$SWcases$registr <- paste0("SW", seq_len(nrow(x$SWcases)))
  x$FBcases$inciden[1] <- as.Date(NA)
  x$SWcases$deathda[2] <- as.Date(NA)
  excluded <- count_canreg(x)$exclude
  for (source in c("FBcases", "SWcases")) {
    rows <- excluded[excluded$source == source, ]
    expect_equal(rows$registr, x[[source]]$registr[rows$regi_num])
  }
})

test_that("count_canreg excludes missing IDs from duplicate counts", {
  registry <- canregs[[1]]
  registry$FBcases <- registry$FBcases[1:3, ]
  registry$FBcases$id <- c("duplicate", "duplicate", NA_character_)

  result <- count_canreg(registry)
  total <- result$fbswicd |>
    dplyr::filter(cancer == "60")

  expect_equal(sum(total$fbs), 3)
  expect_equal(sum(total$ids), 2)
})

test_that("count_canreg uses CI5 cancer and total codes", {
  result <- count_canreg(canregs[[1]], cancer_type = "ci5")
  cancer_codes <- unique(result$fbswicd$cancer)

  expected_codes <- c("60", "61", "401", "461")
  expect_setequal(intersect(cancer_codes, expected_codes), expected_codes)
})

test_that("count_canreg uses named canreg components regardless of list order", {
  registry <- canregs[[1]][c("FBcases", "SWcases", "POP", "regicode")]
  class(registry) <- c("canreg", "list")

  result <- count_canreg(registry)

  expect_s3_class(result, "fbswicd")
  expect_identical(result$regicode, canregs[[1]]$regicode)
  expect_equal(
    sum(result$fbswicd$fbs),
    sum(count_canreg(canregs[[1]])$fbswicd$fbs)
  )
})

test_that("count_canreg retains mortality-only strata after aggregation", {
  registry <- canregs[[1]]
  registry$FBcases <- registry$FBcases[1:20, ]
  registry$SWcases <- registry$SWcases[1:20, ]
  registry$SWcases$sex <- "2"
  registry$SWcases$icd10 <- "C50"
  registry$SWcases$birthda <- as.Date("1950-01-01")
  registry$SWcases$deathda <- as.Date("2022-01-01")
  cancer_code <- as.character(classify_icd10("C50"))

  result <- count_canreg(registry)$fbswicd |>
    dplyr::filter(
      year == "2022",
      sex == "2",
      cancer == cancer_code
    )

  expect_equal(sum(result$sws), 20L)
  expect_equal(sum(result$fbs), 0L)
})

test_that("batch counting matches the per-registry strategy", {
  registries <- structure(
    unclass(canregs)[1:3],
    class = class(canregs)
  )

  batch <- count_canreg(registries)
  registry <- count_canreg(
    registries,
    strategy = "registry"
  )

  expect_equal(batch, registry)
})
