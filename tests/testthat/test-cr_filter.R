test_that("cr_filter", {
  res <- cr_filter(canregs, substr(icd10, 1, 3) == "C73")
  res2 <- cr_filter(canregs[[1]], year == "2021")

  expect_s3_class(res, "canregs")
  expect_s3_class(res2, "canreg")
  expect_equal(
    unname(purrr::map_lgl(res, ~ all(substr(.x$FBcases$icd10, 1, 3) == "C73"))),
    rep(TRUE, length(res))
  )
})

test_that("cr_filter handles additional canreg data-frame components", {
  registry <- canregs[[1]]
  registry$SFcases <- registry$FBcases

  result <- cr_filter(registry, year == "2021")

  expect_s3_class(result, "canreg")
  expect_s3_class(result$SFcases, "FBcases")
})

test_that("cr_filter supports fbswicd objects", {
  year_value <- fbsws[[1]]$fbswicd$year[[1]]
  res <- cr_filter(fbsws[[1]], year == year_value)

  expect_s3_class(res, "fbswicd")
  expect_equal(unique(res$fbswicd$year), year_value)
  expect_equal(unique(res$sitemorp$year), year_value)
  expect_equal(unique(res$pop$year), year_value)
})

test_that("cr_filter skips fbswicd components without requested variables", {
  res <- cr_filter(fbsws[[1]], cancer == "60")

  expect_s3_class(res, "fbswicd")
  expect_equal(unique(res$fbswicd$cancer), "60")
  expect_setequal(unique(res$sitemorp$cancer), character())
  expect_identical(res$pop, fbsws[[1]]$pop)
})

test_that("cr_filter supports fbswicds objects", {
  year_value <- fbsws[[1]]$fbswicd$year[[1]]
  res <- cr_filter(fbsws, year == year_value)

  expect_s3_class(res, "fbswicds")
  expect_gt(length(res), 0)
  expect_equal(
    unname(purrr::map_lgl(res, inherits, "fbswicd")),
    rep(TRUE, length(res))
  )
})

test_that("cr_filter part controls fbswicd components", {
  year_value <- fbsws[[1]]$fbswicd$year[[1]]
  res <- cr_filter(fbsws[[1]], year == year_value, part = "fbswicd")

  expect_equal(unique(res$fbswicd$year), year_value)
  expect_identical(res$sitemorp, fbsws[[1]]$sitemorp)
  expect_identical(res$pop, fbsws[[1]]$pop)
})

test_that("cr_filter validates fbswicd part names", {
  expect_snapshot(error = TRUE, {
    cr_filter(fbsws[[1]], year == "2021", part = "FBcases")
  })
})

test_that("cr_filter drop works for fbswicd cancer components", {
  res <- cr_filter(fbsws[[1]], drop = "total")

  expect_equal(intersect(res$fbswicd$cancer, c("60", "61")), character())
  expect_equal(intersect(res$sitemorp$cancer, c("60", "61")), character())
})

test_that("numeric year conditions preserve stored labels and caller variables", {
  data <- tibble::tibble(year = c("2011", "2012", "2013", " 2019"), value = 1:4)
  cutoff <- 2012
  years <- 2012:2019

  expect_identical(cr_filter(data, year > cutoff), data[3:4, ])
  expect_identical(cr_filter(data, year %in% years), data[2:4, ])
  expect_identical(cr_filter(data, 2012 < year), data[3:4, ])
  expect_identical(cr_filter(data, .data$year >= 2013), data[3:4, ])
  expect_identical(cr_filter(data, .data[["year"]] != 2012), data[c(1, 3, 4), ])
  expect_identical(cr_filter(data, year <= 2012), data[1:2, ])
  expect_identical(cr_filter(data, year == 2012), data[2, ])
})

test_that("character and explicit year expressions keep their semantics", {
  data <- tibble::tibble(year = c("2011", "2012-2016", "2013", NA_character_))
  labels <- c("2012-2016", "2013")

  expect_identical(cr_filter(data, year > "2012"), data[2:3, , drop = FALSE])
  expect_identical(cr_filter(data, year %in% labels), data[2:3, , drop = FALSE])
  expect_identical(
    cr_filter(data, substr(year, 1, 4) == "2012"),
    data[2, , drop = FALSE]
  )
  expect_snapshot({
    mixed <- cr_filter(data, year > 2012 | year == "2012-2016")
    numeric <- cr_filter(data, year %in% 2012:2019)
  })
  expect_identical(mixed, data[2:3, , drop = FALSE])
  expect_identical(numeric, data[3, , drop = FALSE])
})

test_that("year conversion applies across registry and derived methods", {
  cutoff <- 2020
  years <- 2020:2022
  expect_identical(
    cr_filter(canregs, year > cutoff),
    cr_filter(canregs, year > 2020)
  )
  expect_identical(
    cr_filter(fbsws, year %in% years),
    cr_filter(fbsws, year %in% 2020:2022)
  )
  expect_identical(
    cr_filter(canregs, .data$year > cutoff),
    cr_filter(canregs, year > 2020)
  )
  registry <- canregs[[1]]
  registry$POP$year <- " 2021"
  result <- cr_filter(registry, year %in% 2020:2022, part = "POP")
  expect_identical(result$POP, registry$POP)
  expect_identical(result$FBcases, registry$FBcases)
  expect_identical(
    cr_filter(canregs, year > 2020),
    cr_filter(canregs, year > "2020")
  )
  expect_identical(
    cr_filter(fbsws, year %in% 2020:2022),
    cr_filter(fbsws, year %in% as.character(2020:2022))
  )
  rates <- structure(
    list(data.frame(year = c("2011", " 2013"))),
    class = "asrs"
  )
  expect_identical(cr_filter(rates, year > 2012)[[1]]$year, " 2013")
})

test_that("injected function calls remain valid filter conditions", {
  data <- data.frame(year = c("2011", "2019"))
  predicate <- function(x) x == "2019"
  condition <- rlang::call2(predicate, quote(year))
  expect_identical(
    cr_filter(data, !!condition),
    dplyr::filter(data, !!condition)
  )
  expect_identical(
    cr_filter(data, (!!condition) & year > 2012),
    dplyr::filter(data, !!condition)
  )
})
