#' Calculate the age standardized rate using the direct method
#'
#' This function calculates the **age-standardized rate (ASR)** using
#' the **direct method** of standardization. It allows for adjustment of crude
#' disease or mortality rates to a standard population structure, enabling
#' valid comparisons across populations with different age distributions. The
#' function also supports computation of confidence intervals
#' using gamma, normal, or log-normal methods, and returns both crude
#' rate (CR) and ASR with associated variances and interval estimates.
#'
#' @param count The number of cases of a specific disease or condition.
#' @param pop The total population of the same group or region
#'        where the disease cases (count) were observed.
#' @param rate Disease rate, which is the number of cases (count) per unit
#'        of population (pop).
#' @param stdpop Standardized population for age standardization.
#' @param method Method used for calculating the age-standardized rate, options
#'        are 'gamma', 'normal', or 'lognormal', default is 'gamma'.
#' @param conf_level Confidence level for calculating confidence
#'        intervals, value between 0 and 1, default is 0.95.
#' @param mp A multiplier used to scale the calculated rates. Default is 100000.
#'
#' @return Age standardized rate and its confidence interval.
#' @details Inputs must have equal, non-zero lengths. Counts and standard
#'   weights must be finite and non-negative; population must be strictly
#'   positive. Standard weights must have a positive total. Zero-event rates
#'   use Gamma bounds (zero lower bound) for all interval methods because the
#'   normal and log-normal approximations are unsuitable at zero. Variances
#'   remain on the unmultiplied rate scale.
#' @export
#'
#' @examples
#' cases <- c(50, 60, 45, 70)
#' pop <- c(1000, 1200, 1100, 900)
#' spop <- c(800, 1000, 1100, 900)
#' ageadjust(cases, pop, stdpop = spop, mp = 100000)
ageadjust <- function(
  count,
  pop,
  rate = NULL,
  stdpop = NULL,
  method = "gamma",
  conf_level = 0.95,
  mp = 100000
) {
  check_vector <- function(x, name, positive = FALSE) {
    if (
      !is.numeric(x) ||
        length(x) == 0L ||
        any(!is.finite(x)) ||
        any(if (positive) x <= 0 else x < 0)
    ) {
      stop(
        "`",
        name,
        "` must be a non-empty finite numeric vector with ",
        if (positive) "positive" else "non-negative",
        " values."
      )
    }
  }
  method <- match.arg(method, c("gamma", "normal", "lognormal"))
  if (
    !is.numeric(conf_level) ||
      length(conf_level) != 1L ||
      !is.finite(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    stop("`conf_level` must be a finite number between 0 and 1.")
  }
  check_vector(mp, "mp", positive = TRUE)
  if (length(mp) != 1L) {
    stop("`mp` must have length one.")
  }
  if (missing(count) && missing(pop)) {
    stop("Supply `count` or `pop` with `rate`.")
  }
  if (missing(count)) {
    check_vector(pop, "pop", positive = TRUE)
    check_vector(rate, "rate")
    if (length(rate) != length(pop)) {
      stop("`rate` and `pop` must have equal lengths.")
    }
    count <- rate * pop
  }
  if (missing(pop)) {
    check_vector(count, "count")
    check_vector(rate, "rate", positive = TRUE)
    if (length(rate) != length(count)) {
      stop("`rate` and `count` must have equal lengths.")
    }
    pop <- count / rate
  }
  check_vector(count, "count")
  check_vector(pop, "pop", positive = TRUE)
  if (length(count) != length(pop)) {
    stop("`count` and `pop` must have equal lengths.")
  }
  if (!is.null(rate)) {
    check_vector(rate, "rate")
    if (
      length(rate) != length(count) ||
        !isTRUE(all.equal(as.numeric(rate), as.numeric(count / pop)))
    ) {
      stop("`rate` must equal `count / pop` for every age group.")
    }
  }
  rate <- count / pop
  if (!is.null(stdpop)) {
    check_vector(stdpop, "stdpop")
    if (length(stdpop) != length(count) || sum(stdpop) <= 0) {
      stop("`stdpop` must match `count` in length and have a positive total.")
    }
  }
  alpha <- 1 - conf_level
  cases <- sum(count)
  rks <- sum(pop)
  cr <- cases / rks
  crvar <- cases / rks^2

  if (!is.null(stdpop)) {
    stdwt <- stdpop / sum(stdpop)
    asr <- sum(stdwt * rate)
    asrvar <- sum((stdwt^2) * (count / pop^2))
    wm <- max(stdwt / pop)
  }

  if (method == "gamma") {
    cr_lci <- if (cases == 0) 0 else mp * qgamma(alpha / 2, shape = cases) / rks
    cr_uci <- mp * qgamma(1 - alpha / 2, shape = cases + 1) / rks
    if (!is.null(stdpop)) {
      asr_lci <- if (asr == 0) {
        0
      } else {
        mp *
          (qgamma(alpha / 2, shape = asr^2 / asrvar) / (asr / asrvar))
      }
      asr_uci <- mp *
        (qgamma(
          1 - alpha / 2,
          shape = ((asr + wm)^2) /
            (asrvar + wm^2),
          scale = (asrvar + wm^2) / (asr + wm)
        ))
    }
  } else if (method == "normal") {
    cr_lci <- mp * (cr + qnorm(alpha / 2) * sqrt(crvar))
    cr_uci <- mp * (cr - qnorm(alpha / 2) * sqrt(crvar))
    if (!is.null(stdpop)) {
      asr_lci <- mp * (asr + qnorm(alpha / 2) * sqrt(asrvar))
      asr_uci <- mp * (asr - qnorm(alpha / 2) * sqrt(asrvar))
    }
  } else if (method == "lognormal") {
    cr_lci <- if (cr == 0) {
      0
    } else {
      mp * exp(log(cr) + qnorm(alpha / 2) * sqrt(crvar) / cr)
    }
    cr_uci <- if (cr == 0) {
      0
    } else {
      mp * exp(log(cr) - qnorm(alpha / 2) * sqrt(crvar) / cr)
    }
    if (!is.null(stdpop)) {
      asr_lci <- if (asr == 0) {
        0
      } else {
        mp * exp(log(asr) + qnorm(alpha / 2) * sqrt(asrvar) / asr)
      }
      asr_uci <- if (asr == 0) {
        0
      } else {
        mp * exp(log(asr) - qnorm(alpha / 2) * sqrt(asrvar) / asr)
      }
    }
  } else {
    stop(
      "Method specified was not supported.
       The supported methods: gamma, normal and lognormal"
    )
  }

  if (cases == 0) {
    cr_lci <- 0
    cr_uci <- mp * qgamma(1 - alpha / 2, shape = 1) / rks
  }
  if (!is.null(stdpop) && asr == 0) {
    asr_lci <- 0
    asr_uci <- mp * qgamma(1 - alpha / 2, shape = 1) * wm
  }
  cr <- mp * cr

  if (!is.null(stdpop)) {
    asr <- mp * asr
    res <- list(
      cases = cases,
      cr = cr,
      cr_var = crvar,
      cr_lci = cr_lci,
      cr_uci = cr_uci,
      asr = asr,
      asr_var = asrvar,
      asr_lci = asr_lci,
      asr_uci = asr_uci
    )
  } else {
    res <- list(
      cases = cases,
      cr = cr,
      cr_var = crvar,
      cr_lci = cr_lci,
      cr_uci = cr_uci
    )
  }
  return(res)
}

#' Calculate truncated age standardized rate
#'
#' Calculates the **truncated age-standardized rate** (ASR) over a specified
#' age range (e.g., 35–64 years). It uses the **direct method of
#' standardization** by applying age-specific rates to a standard population
#' within the truncated age group. This is particularly useful when comparing
#' disease rates in middle-aged or other focused subgroups.
#'
#' @param cases Number of cases.
#' @param pop  Number of population at risk.
#' @param stdpop The standard population.
#' @param trunc_age Inclusive truncated age range. Both ends must align with
#'   complete age groups; the group starting after the upper end is excluded.
#' @param agewidth Age groups width, default is 5.
#' @param sep_zero Logical value, if the 0 age group was a separate group.
#' @param mp A multiplier used to scale the calculated rates. Default is 100.
#' @param decimal Decimals of the calculated rates, default is 2.
#'
#' @return Truncated age standardized rate.
#' @export
#'
#' @examples
#' px <- c(
#'   20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
#'   163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909
#' )
#' dx <- c(
#'   156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
#'   891, 831, 926, 731, 269
#' )
#' stdpop <- c(2.4, 9.6, 10, 9, 9, 8, 8, 6, 6, 6, 6, 5, 4, 4, 3, 2, 1, 0.5, 0.5)
#' truncrate(dx, px, stdpop, trunc_age = c(35, 64))
truncrate <- function(
  cases,
  pop,
  stdpop = NULL,
  trunc_age = c(35, 64),
  agewidth = 5,
  sep_zero = TRUE,
  mp = 100,
  decimal = 2
) {
  if (length(cases) != length(pop)) {
    stop("Length of 'cases' and 'pop' must be the same.")
  }

  nn <- length(cases)
  if (
    length(sep_zero) != 1L ||
      is.na(sep_zero) ||
      !is.logical(sep_zero) ||
      length(agewidth) != 1L ||
      !is.numeric(agewidth) ||
      !is.finite(agewidth) ||
      agewidth < 1 ||
      agewidth != floor(agewidth) ||
      (sep_zero && agewidth <= 1) ||
      nn < if (sep_zero) 3L else 2L
  ) {
    stop(
      "Supply valid agewidth, sep_zero and at least two complete age groups."
    )
  }
  if (
    !is.numeric(trunc_age) ||
      length(trunc_age) != 2L ||
      any(!is.finite(trunc_age)) ||
      trunc_age[1] > trunc_age[2]
  ) {
    stop("`trunc_age` must be an ordered pair of finite ages.")
  }
  if (!is.null(stdpop) && length(stdpop) != nn) {
    stop("Length of 'stdpop' must match 'cases' and 'pop'.")
  }

  if (sep_zero) {
    age <- c(0, 1, seq(agewidth, (nn - 2) * agewidth, agewidth))
    agewidth <- c(1, agewidth - 1, rep(agewidth, nn - 2))
  } else {
    age <- c(0, seq(agewidth, (nn - 1) * agewidth, agewidth))
    agewidth <- rep(agewidth, nn)
  }

  pos_start <- which(age == trunc_age[1])
  pos_end <- which(age + agewidth == (trunc_age[2] + 1))
  if (length(pos_start) != 1L || length(pos_end) != 1L || pos_start > pos_end) {
    stop("`trunc_age` must align with complete age groups within the input.")
  }

  cases <- cases[pos_start:pos_end]
  pop <- pop[pos_start:pos_end]

  # If stdpop is NULL, assume equal weights
  if (is.null(stdpop)) {
    stdpop <- rep(1, length(cases))
  } else {
    stdpop <- stdpop[pos_start:pos_end]
  }

  rate <- cases / pop
  if (!is.null(stdpop)) {
    rate <- sum(rate * stdpop) / sum(stdpop)
  }
  rate <- round(rate * mp, decimal)
  return(rate)
}
