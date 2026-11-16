test_that("population cleaning is order invariant and conserves counts", {
  p <- canregs[[1]]$POP
  p$rks <- p$rks + 0.25
  p$death <- p$rks / 10
  result <- cr_clean(p)
  expect_equal(result, cr_clean(p[nrow(p):1, ]))
  expect_equal(sum(result$rks), sum(p$rks))
  expect_equal(sum(result$death), sum(p$death))
  expect_equal(result$rks, p$rks)
  coarse <- cr_clean(p, age_breaks = seq(0, 85, 5))
  expect_equal(sum(coarse$rks), sum(p$rks))
  expect_equal(coarse$rks[1], sum(p$rks[1:2]))
})

test_that("standard 18-group population can be split without losing totals", {
  p <- data.frame(
    year = "2021",
    sex = "1",
    agegrp = seq(0, 85, 5),
    rks = seq_len(18) + 0.5
  )
  class(p) <- c("POP", class(p))
  result <- cr_clean(p)
  expect_equal(nrow(result), 19L)
  expect_equal(sum(result$rks), sum(p$rks))
  expect_equal(sum(result$rks[1:2]), p$rks[1])
  expect_equal(tail(result$rks, 1), tail(p$rks, 1))
  expect_equal(unique(result$sex), "1")
})

test_that("empty population stays empty", {
  p <- canregs[[1]]$POP[FALSE, ]
  result <- cr_clean(p)
  expect_equal(nrow(result), 0L)
  expect_named(result, names(p))
  expect_s3_class(result, "POP")
})

test_that("population cleaning converts numeric text and preserves missing death", {
  p <- canregs[[1]]$POP
  p$rks <- as.character(p$rks)
  p$death <- as.character(seq_len(nrow(p)))
  p$death[c(1, 19)] <- NA_character_
  result <- cr_clean(p)
  expect_type(result$rks, "double")
  expect_equal(result$rks, as.numeric(p$rks))
  expect_equal(result$death, as.numeric(p$death))

  registry <- canregs[[1]]
  registry$POP <- p
  counted <- count_canreg(registry)
  expect_equal(counted$pop$death, as.numeric(p$death))

  coarse <- cr_clean(p, age_breaks = seq(0, 85, 5))
  expect_equal(coarse$death[1], NA_real_)
  expect_equal(coarse$death[18], NA_real_)
})

test_that("population errors identify the stratum, variable, age, and value", {
  p <- canregs[[1]]$POP
  p$rks <- as.character(p$rks)
  p$rks[2] <- "missing"
  expect_snapshot(error = TRUE, cr_clean(p))

  p <- canregs[[1]]$POP
  p$rks[3] <- NA
  expect_snapshot(error = TRUE, cr_clean(p))

  p <- canregs[[1]]$POP
  p$death <- 0
  p$death[4] <- -1
  expect_snapshot(error = TRUE, cr_clean(p))
})

test_that("population cleaning rejects incomplete and duplicate schedules", {
  p <- canregs[[1]]$POP
  expect_snapshot(error = TRUE, cr_clean(p[-1, ]))
  expect_snapshot(error = TRUE, cr_clean(rbind(p, p[1, ])))
  p$agegrp <- as.character(p$agegrp)
  p$agegrp[1] <- "unknown"
  expect_snapshot(error = TRUE, cr_clean(p))
  expect_snapshot(
    error = TRUE,
    cr_clean(canregs[[1]]$POP, age_breaks = c(0, 90))
  )
})
