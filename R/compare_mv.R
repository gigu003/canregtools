#' Compare morphologically verified percentages across registries
#'
#' `compare_mv()` applies the statistical test described by Bray and Parkin
#' (2009) to identify registry percentages of morphologically verified cases
#' (MV%) that differ from comparison registries. Each registry is tested in
#' turn, using all other registries in the same stratum as its comparison
#' population.
#'
#' The function accepts either the merged `quality` data frame returned by
#' `create_quality(..., collapse = TRUE)` or the `qualities` list returned by
#' `create_quality(..., collapse = FALSE)`. Strata are detected automatically
#' from identifier columns such as `year`, `sex`, and `cancer`.
#'
#' @details
#'
#' By default, each registry is compared with the other registries in its
#' stratum. Supply reference to use an external reference table instead.
#' External rows are matched to each target stratum using the stratum columns
#' shared by x and reference.
#' For comparison registries indexed by \eqn{i = 1, \ldots, n}, let \eqn{y_i}
#' be the number of morphologically verified cases and \eqn{d_i} the total
#' number of incident cases. The common MV proportion and overdispersion are
#' estimated as
#' \deqn{\hat{p} = \frac{\sum_i y_i}{\sum_i d_i}}
#' and
#' \deqn{\hat{\phi} = \frac{1}{n - 1} \sum_i
#' \frac{(y_i - \hat{p}d_i)^2}{d_i\hat{p}(1 - \hat{p})}.}
#' For the registry under test, indexed by \eqn{j}, the statistic is
#' \deqn{Z^2 = \frac{(y_j - \hat{p}d_j)^2}
#' {\hat{\phi}\hat{p}(1 - \hat{p})d_j}},
#' which is compared with a chi-squared distribution with one degree of
#' freedom.
#'
#' When `x` contains an `mv_cases` column, it is used as the exact numerator
#' \eqn{y}. Otherwise, MV case counts are reconstructed as
#' `round(fbs * mv / 100)` from the percentage returned by `create_quality()`.
#' The `mvs` column is a different quality indicator and is not used in this
#' test.
#'
#' A status other than `"ok"` indicates that the row could not be evaluated.
#' Possible values are `"zero_target_cases"`,
#' `"insufficient_reference_registries"`, `"zero_reference_cases"`,
#' `"boundary_reference_proportion"`, and `"zero_reference_dispersion"`.
#' An `unusual` result indicates a statistical difference from the comparison
#' registries; it does not by itself identify the reason for that difference.
#'
#' @param x An object with class `quality` or `qualities` containing the
#'   registry identifier `regicode`, incident case counts `fbs`, and MV
#'   percentages `mv`. An optional `mv_cases` column supplies exact MV case
#'   counts.
#' @param alpha A single numeric significance level strictly between 0 and 1.
#'   Defaults to `0.05`, corresponding to a chi-squared critical value of
#'   approximately 3.84 with one degree of freedom.
#'
#' @return A tibble containing the stratum identifiers, `regicode`, `fbs`, the
#'   observed `mv`, the MV count used in the calculation `mv_cases`, the number
#'   of comparison registries `reference_n`, the estimated common proportion
#'   `p`, the overdispersion estimate `phi`, the test statistic `z2`,
#'   `p_value`, `critical_value`, the logical flag `unusual`, and an evaluation
#'   `status`.
#'
#' @references
#' Bray, F., & Parkin, D. M. (2009). Evaluation of data quality in the cancer
#' registry: Principles and methods. Part I: Comparability, validity and
#' timeliness. *European Journal of Cancer*, 45(5), 747-755.
#' \doi{10.1016/j.ejca.2008.11.032}
#'
#' @param reference An optional external reference table. It must contain
#'   regicode, fbs, and mv, or mv_cases; registry and cases are accepted as
#'   aliases for regicode and fbs. At least two reference rows are needed in
#'   each matched stratum to estimate dispersion.
#'
#' @export
#'
#' @examples
#' quality_data <- structure(
#'   data.frame(
#'     regicode = c("A", "B", "C", "D"),
#'     year = "2024",
#'     sex = "1",
#'     cancer = "101",
#'     fbs = c(100, 100, 150, 200),
#'     mv = c(95, 70, 80, 75)
#'   ),
#'   class = c("quality", "data.frame")
#' )
#' compare_mv(quality_data)
compare_mv <- function(x, alpha = 0.05, reference = NULL) {
  if (!inherits(x, "quality") && !inherits(x, "qualities")) {
    stop("`x` must inherit from class 'quality' or 'qualities'.", call. = FALSE)
  }

  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    stop(
      "`alpha` must be a single number strictly between 0 and 1.",
      call. = FALSE
    )
  }

  if (inherits(x, "qualities")) {
    x <- cr_merge(x)
  }

  if (!is.data.frame(x)) {
    stop("`x` must resolve to a data frame.", call. = FALSE)
  }
  x <- standardize_year_sex(x)
  if (nrow(x) == 0L) {
    stop("`x` must contain at least one row.", call. = FALSE)
  }

  required <- c("regicode", "fbs", "mv")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(
      "`x` is missing required columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (anyNA(x$regicode) || any(trimws(as.character(x$regicode)) == "")) {
    stop("`regicode` must not contain missing or empty values.", call. = FALSE)
  }

  validate_mv_counts(x$fbs, "fbs")
  validate_mv_percent(x$mv, x$fbs)

  if ("mv_cases" %in% names(x)) {
    validate_mv_counts(x$mv_cases, "mv_cases")
    if (any(x$mv_cases > x$fbs)) {
      stop("`mv_cases` must not exceed `fbs`.", call. = FALSE)
    }
  } else {
    x$mv_cases <- ifelse(x$fbs > 0, round(x$fbs * x$mv / 100), 0)
  }

  measure_columns <- c(
    "rks",
    "fbs",
    "inci",
    "sws",
    "mort",
    "mi",
    "mv",
    "mv_cases",
    "dco",
    "ub",
    "sub",
    "mvs",
    "ids",
    "mult",
    "death",
    "ex"
  )
  strata <- setdiff(names(x), c("regicode", measure_columns))
  reference <- prepare_comparison_reference(reference, "mv")
  validate_reference_keys(reference, strata)
  key_columns <- c(strata, "regicode")

  if (anyDuplicated(x[key_columns])) {
    stop(
      "Each `regicode` must occur only once within each detected stratum.",
      call. = FALSE
    )
  }

  grouped <- if (length(strata) == 0L) {
    list(x)
  } else {
    x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(strata)), .drop = FALSE) |>
      dplyr::group_split(.keep = TRUE)
  }

  critical_value <- stats::qchisq(1 - alpha, df = 1)
  purrr::map_dfr(
    grouped,
    compare_mv_group,
    critical_value = critical_value,
    reference = reference,
    strata = strata
  )
}

validate_mv_counts <- function(x, name) {
  if (!is.numeric(x)) {
    stop("`", name, "` must be numeric.", call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x)) || any(x < 0)) {
    stop(
      "`",
      name,
      "` must contain finite, non-negative values without missing values.",
      call. = FALSE
    )
  }
  if (any(abs(x - round(x)) > sqrt(.Machine$double.eps))) {
    stop("`", name, "` must contain whole-number counts.", call. = FALSE)
  }
}

validate_mv_percent <- function(mv, cases) {
  if (!is.numeric(mv)) {
    stop("`mv` must be numeric.", call. = FALSE)
  }
  positive_cases <- cases > 0
  if (
    any(is.na(mv[positive_cases])) ||
      any(!is.na(mv) & (!is.finite(mv) | mv < 0 | mv > 100))
  ) {
    stop(
      "`mv` must be finite and between 0 and 100 when `fbs` is positive.",
      call. = FALSE
    )
  }
}

compare_mv_group <- function(
  data,
  critical_value,
  reference = NULL,
  strata = character()
) {
  n <- nrow(data)
  target_fbs <- data$fbs
  target_mv_cases <- data$mv_cases
  reference_data <- if (is.null(reference)) {
    NULL
  } else {
    select_comparison_reference(reference, data[1, , drop = FALSE], strata)
  }
  reference_n <- if (is.null(reference)) n - 1L else nrow(reference_data)
  status <- rep("ok", n)
  p <- rep(NA_real_, n)
  phi <- rep(NA_real_, n)
  z2 <- rep(NA_real_, n)
  p_value <- rep(NA_real_, n)
  unusual <- rep(NA, n)

  if (is.null(reference)) {
    reference_fbs <- sum(data$fbs) - target_fbs
    reference_mv_cases <- sum(data$mv_cases) - target_mv_cases
    zero_reference <- sum(data$fbs == 0) - as.integer(data$fbs == 0) > 0
  } else {
    reference_fbs <- sum(reference_data$fbs)
    reference_mv_cases <- sum(reference_data$mv_cases)
    zero_reference <- rep(any(reference_data$fbs == 0), n)
  }

  zero_target <- target_fbs == 0
  status[zero_target] <- "zero_target_cases"
  insufficient <- !zero_target & reference_n < 2L
  status[insufficient] <- "insufficient_reference_registries"
  no_zero_reference <- !zero_target & !insufficient & !zero_reference
  if (any(no_zero_reference)) {
    if (is.null(reference)) {
      p[no_zero_reference] <-
        reference_mv_cases[no_zero_reference] /
        reference_fbs[no_zero_reference]
    } else {
      p[no_zero_reference] <- reference_mv_cases / reference_fbs
    }
    boundary <- no_zero_reference & (p <= 0 | p >= 1)
    status[boundary] <- "boundary_reference_proportion"
    evaluable <- no_zero_reference & !boundary

    if (any(evaluable)) {
      if (is.null(reference)) {
        sum_mv2_over_fbs <- sum(data$mv_cases^2 / data$fbs)
        sum_mv <- sum(data$mv_cases)
        sum_fbs <- sum(data$fbs)
        phi[evaluable] <- (sum_mv2_over_fbs -
          target_mv_cases[evaluable]^2 /
            target_fbs[evaluable] -
          2 * p[evaluable] * (sum_mv - target_mv_cases[evaluable]) +
          p[evaluable]^2 * (sum_fbs - target_fbs[evaluable])) /
          ((reference_n - 1L) * p[evaluable] * (1 - p[evaluable]))
      } else {
        phi_value <- sum(
          (reference_data$mv_cases -
            (reference_mv_cases / reference_fbs) * reference_data$fbs)^2 /
            (reference_data$fbs *
              (reference_mv_cases / reference_fbs) *
              (1 - reference_mv_cases / reference_fbs))
        ) /
          (reference_n - 1L)
        phi[evaluable] <- phi_value
      }
      zero_dispersion <- evaluable & (!is.finite(phi) | phi <= 0)
      status[zero_dispersion] <- "zero_reference_dispersion"
      valid <- evaluable & !zero_dispersion
      if (any(valid)) {
        z2[valid] <- (target_mv_cases[valid] - p[valid] * target_fbs[valid])^2 /
          (phi[valid] * p[valid] * (1 - p[valid]) * target_fbs[valid])
        p_value[valid] <- stats::pchisq(z2[valid], df = 1, lower.tail = FALSE)
        unusual[valid] <- z2[valid] >= critical_value
      }
    }
  }
  status[!zero_target & !insufficient & zero_reference] <-
    "zero_reference_cases"

  result <- dplyr::tibble(
    reference_n = rep(reference_n, n),
    p = p,
    phi = phi,
    z2 = z2,
    p_value = p_value,
    critical_value = rep(critical_value, n),
    unusual = unusual,
    status = status
  )
  identifier_columns <- c(
    "regicode",
    setdiff(
      names(data),
      c(
        "regicode",
        "rks",
        "fbs",
        "inci",
        "sws",
        "mort",
        "mi",
        "mv",
        "mv_cases",
        "dco",
        "ub",
        "sub",
        "mvs",
        "ids",
        "mult",
        "death",
        "ex"
      )
    )
  )
  data |>
    dplyr::select(dplyr::all_of(identifier_columns)) |>
    dplyr::mutate(
      fbs = target_fbs,
      mv = data$mv,
      mv_cases = target_mv_cases
    ) |>
    dplyr::bind_cols(result)
}
