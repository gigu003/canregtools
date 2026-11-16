test_that("cr_rename renames selected registries and their regicodes", {
  original <- canregs
  result <- cr_rename(
    original,
    c("410103" = "REG_A", "410307" = "REG_B")
  )

  expect_s3_class(result, "canregs")
  expect_named(result, c("REG_A", "REG_B", "411721"))
  expect_identical(result[["REG_A"]]$regicode, "REG_A")
  expect_identical(result[["REG_B"]]$regicode, "REG_B")
  expect_identical(result[["411721"]]$regicode, "411721")
  expect_s3_class(result[["REG_A"]], "canreg")
  expect_identical(result[["REG_A"]]$FBcases, original[["410103"]]$FBcases)
  expect_identical(original[["410103"]]$regicode, "410103")
})

test_that("cr_rename renames fbswicds and their regicodes", {
  original <- count_canreg(canregs)
  result <- cr_rename(
    original,
    c("410103" = "REG_A", "410307" = "REG_B")
  )

  expect_s3_class(result, "fbswicds")
  expect_named(result, c("REG_A", "REG_B", "411721"))
  expect_identical(result[["REG_A"]]$regicode, "REG_A")
  expect_identical(result[["REG_B"]]$regicode, "REG_B")
  expect_identical(result[["411721"]]$regicode, "411721")
  expect_s3_class(result[["REG_A"]], "fbswicd")
  expect_identical(
    result[["REG_A"]]$fbswicd,
    original[["410103"]]$fbswicd
  )
  expect_identical(original[["410103"]]$regicode, "410103")
})

test_that("cr_rename accepts an empty named mapping", {
  result <- cr_rename(canregs, setNames(character(), character()))

  expect_identical(result, canregs)

  summarized <- count_canreg(canregs)
  summarized_result <- cr_rename(
    summarized,
    setNames(character(), character())
  )
  expect_identical(summarized_result, summarized)
})

test_that("cr_rename validates mapping", {
  expect_snapshot(error = TRUE, {
    cr_rename(canregs, c("REG_A", "REG_B"))
  })
  expect_snapshot(error = TRUE, {
    cr_rename(canregs, c("UNKNOWN" = "REG_A"))
  })
  expect_snapshot(error = TRUE, {
    cr_rename(canregs, c("410103" = ""))
  })
  expect_snapshot(error = TRUE, {
    cr_rename(canregs, c("410103" = "REG_A", "410103" = "REG_B"))
  })
  expect_snapshot(error = TRUE, {
    cr_rename(canregs, c("410103" = "410307"))
  })
})
