test_that("cr_combine combines matching registries", {
  first <- structure(canregs[1:2], class = class(canregs))
  second <- structure(canregs[2:3], class = class(canregs))

  result <- cr_combine(list(first, second))

  expect_s3_class(result, "canregs")
  expect_named(result, sort(names(canregs)))
  expect_s3_class(result[["410307"]], "canreg")
  expect_identical(result[["410307"]]$regicode, "410307")
  expect_s3_class(result[["410307"]]$FBcases, "FBcases")
  expect_s3_class(result[["410307"]]$SWcases, "SWcases")
  expect_s3_class(result[["410307"]]$POP, "POP")
  expect_equal(
    nrow(result[["410307"]]$FBcases),
    2 * nrow(canregs[["410307"]]$FBcases)
  )
  expect_identical(result[["410103"]], canregs[["410103"]])
  expect_identical(result[["411721"]], canregs[["411721"]])
})

test_that("cr_combine combines available SFcases components", {
  first <- structure(canregs[1], class = class(canregs))
  second <- first
  third <- first
  sf <- first[[1]]$SWcases[1:2, , drop = FALSE]
  first[[1]]$SFcases <- structure(sf, class = c("SFcases", class(sf)))
  second[[1]]$SFcases <- first[[1]]$SFcases

  combined_all <- cr_combine(list(first, second))
  combined_some <- cr_combine(list(first, third))
  combined_none <- cr_combine(list(third, third))

  expect_s3_class(combined_all[[1]]$SFcases, "SFcases")
  expect_equal(nrow(combined_all[[1]]$SFcases), 2 * nrow(sf))
  expect_identical(combined_some[[1]]$SFcases, first[[1]]$SFcases)
  expect_named(
    combined_some[[1]],
    c("regicode", "FBcases", "SWcases", "SFcases", "POP")
  )
  expect_named(
    combined_none[[1]],
    c("regicode", "FBcases", "SWcases", "POP")
  )
})

test_that("cr_combine combines matching fbswicds", {
  fbsws <- count_canreg(canregs)
  first <- structure(fbsws[1:2], class = class(fbsws))
  second <- structure(fbsws[2:3], class = class(fbsws))
  result <- cr_combine(list(first, second))

  expect_s3_class(result, "fbswicds")
  expect_named(result, sort(names(fbsws)))
  overlap <- result[["410307"]]
  expect_s3_class(overlap, "fbswicd")
  expect_identical(overlap$regicode, "410307")
  expect_s3_class(overlap$pop, "POP")
  for (component in c("fbswicd", "sitemorp", "pop", "exclude")) {
    expect_equal(
      nrow(overlap[[component]]),
      2 * nrow(fbsws[["410307"]][[component]])
    )
  }
  expect_identical(result[["410103"]], fbsws[["410103"]])
  expect_identical(result[["411721"]], fbsws[["411721"]])
})

test_that("cr_combine supports minimal and optional fbswicd components", {
  counted <- count_canreg(canregs[[1]])
  minimal <- as_fbswicd(
    counted$fbswicd[c("year", "sex", "cancer", "agegrp", "fbs")],
    counted$pop,
    sitemorp = counted$sitemorp[FALSE, ],
    exclude = counted$exclude[FALSE, ],
    regicode = "A"
  )
  without_optional <- minimal[c("regicode", "fbswicd", "pop")]
  class(without_optional) <- class(minimal)
  first <- structure(list(A = minimal), class = c("fbswicds", "list"))
  second <- structure(
    list(A = without_optional),
    class = c("fbswicds", "list")
  )
  result <- cr_combine(list(first, second))
  expect_named(result$A, c("regicode", "fbswicd", "sitemorp", "pop", "exclude"))
  expect_equal(nrow(result$A$sitemorp), 0L)
  expect_equal(nrow(result$A$exclude), 0L)
  expect_equal(sum(result$A$fbswicd$fbs), 2 * sum(minimal$fbswicd$fbs))

  neither <- cr_combine(list(second, second))
  expect_named(neither$A, c("regicode", "fbswicd", "pop"))
})

test_that("cr_combine validates its input", {
  expect_snapshot(error = TRUE, {
    cr_combine(list())
  })
  expect_snapshot(error = TRUE, {
    cr_combine(list(canregs, unclass(canregs)))
  })

  fbsws <- count_canreg(canregs)
  expect_snapshot(error = TRUE, {
    cr_combine(list(canregs, fbsws))
  })

  empty <- structure(list(), class = c("canregs", "list"))
  expect_snapshot(error = TRUE, {
    cr_combine(list(empty))
  })

  unnamed <- canregs
  names(unnamed) <- NULL
  expect_snapshot(error = TRUE, {
    cr_combine(list(unnamed))
  })

  mismatched <- canregs
  mismatched[[1]]$regicode <- "OTHER"
  expect_snapshot(error = TRUE, {
    cr_combine(list(mismatched))
  })

  invalid_sf <- canregs
  invalid_sf[[1]]$SFcases <- "not a data frame"
  expect_snapshot(error = TRUE, {
    cr_combine(list(invalid_sf))
  })

  mismatched_fbsw <- fbsws
  mismatched_fbsw[[1]]$regicode <- "OTHER"
  expect_snapshot(error = TRUE, {
    cr_combine(list(mismatched_fbsw))
  })

  invalid_component <- fbsws
  invalid_component[[1]]$pop <- "not a data frame"
  expect_snapshot(error = TRUE, {
    cr_combine(list(invalid_component))
  })

  base_fbsws <- structure(fbsws[1], class = class(fbsws))
  sparse <- base_fbsws
  sparse[[1]]$fbswicd$sws <- NULL
  expect_snapshot(error = TRUE, {
    cr_combine(list(base_fbsws, sparse))
  })
})
