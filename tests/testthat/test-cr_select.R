test_that("cr_select for canregs selects by logical condition", {
  selected <- cr_select(canregs, mi > 0.5)

  expect_s3_class(selected, "canregs")
  expect_named(selected, "411721")

  selected_summary <- summary(selected, collapse = FALSE)
  expect_equal(unname(purrr::map_dbl(selected_summary, "mi")), 0.67)
})

test_that("registry selection metrics match the standard summary", {
  expected <- summary(canregs[[1]], collapse = FALSE)
  fast <- canregtools:::summary_for_selection(canregs[[1]])

  expect_equal(fast, expected)
})

test_that("cr_select for fbswicds selects by condition", {
  selected <- cr_select(fbsws, area_type == "920000")

  expect_s3_class(selected, "fbswicds")
  expect_named(selected, c("410307", "411721"))
  expect_equal(
    unique(purrr::map_chr(selected, "regicode")),
    c("410307", "411721")
  )
})

test_that("cr_select supports explicit indexes", {
  selected <- cr_select(canregs, index = names(canregs)[1:2])

  expect_s3_class(selected, "canregs")
  expect_named(selected, names(canregs)[1:2])
  expect_length(selected, 2)
})

test_that("cr_select for asrs selects by condition", {
  asr_obj <- create_asr(fbsws, collapse = FALSE)
  selected <- cr_select(asr_obj, cr > 300)

  expect_s3_class(selected, "asrs")
  expect_named(selected, "410103")
  expect_equal(
    unname(purrr::map_lgl(selected, ~ any(.x$cr > 300))),
    rep(TRUE, length(selected))
  )
})

test_that("cr_select for fbswicds can use quality conditions", {
  selected <- cr_select(fbsws, inci > 300)

  expect_s3_class(selected, "fbswicds")
  expect_named(selected, "410103")
})

test_that("cr_select filters canregs by population year count and range", {
  year_data <- canregs
  for (i in seq_len(2L)) {
    years <- if (i == 1L) 2018:2020 else 2018:2022
    population <- year_data[[i]]$POP
    year_data[[i]]$POP <- do.call(
      rbind,
      lapply(years, function(year) {
        result <- population
        result$year <- as.character(year)
        result
      })
    )
  }

  selected_count <- cr_select(year_data, pop_n_years == 3)
  expect_named(selected_count, "410103")

  selected_range <- cr_select(
    year_data,
    all(pop_years %in% as.character(2018:2022))
  )
  expect_named(selected_range, c("410103", "410307"))

  selected_direct <- cr_select(
    year_data,
    length(unique(POP$year)) == 3
  )
  expect_named(selected_direct, "410103")
})

test_that("cr_select filters fbswicds by population year count and range", {
  year_data <- canregs
  for (i in seq_len(2L)) {
    years <- if (i == 1L) 2018:2020 else 2018:2022
    population <- year_data[[i]]$POP
    year_data[[i]]$POP <- do.call(
      rbind,
      lapply(years, function(year) {
        result <- population
        result$year <- as.character(year)
        result
      })
    )
  }
  year_fbsw <- count_canreg(year_data)

  selected_count <- cr_select(
    year_fbsw,
    length(unique(pop$year)) == 3
  )
  expect_named(selected_count, "410103")

  selected_range <- cr_select(
    year_fbsw,
    pop_year_min >= 2018,
    pop_year_max <= 2022
  )
  expect_named(selected_range, c("410103", "410307"))
})

test_that("cr_select derives calendar bounds from population period labels", {
  year_data <- canregs
  year_data[[1]]$POP$year <- "2003-2007"

  selected <- cr_select(
    year_data,
    pop_year_min == 2003,
    pop_year_max == 2007
  )

  expect_named(selected, "410103")
})

test_that("selection ignores missing matches and documents separate row semantics", {
  x <- structure(
    list(
      a = data.frame(year = c("2011", "2019"), cr = c(100, 1)),
      b = data.frame(year = c("2011", "2019"), cr = c(NA_real_, 1)),
      c = data.frame(year = NA_character_, cr = NA_real_)
    ),
    class = c("asrs", "list")
  )
  expect_named(cr_select(x, cr > 50), "a")
  expect_named(cr_select(x, year == "2019", cr > 50), "a")
  expect_length(cr_select(x, year == "2019" & cr > 50), 0L)
  expect_named(cr_select(x, is.na(cr)), c("b", "c"))
})
