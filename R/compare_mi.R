#' Compare mortality-to-incidence ratios across registries
#'
#' `compare_mi()` applies the statistical test described by Parkin and Bray
#' (2009) to identify registry mortality-to-incidence (M:I) ratios that differ
#' from comparison registries. Each registry is tested in turn, using all other
#' registries in the same stratum as its comparison population.
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
#' For comparison registries indexed by \eqn{i = 1, \ldots, n}, let \eqn{d_i}
#' be the number of incident cases, \eqn{m_i} the number of deaths, and
#' \eqn{n_i = m_i + d_i}. The common M:I ratio and dispersion are estimated as
#' \deqn{\hat{\theta} = \frac{\sum_i m_i}{\sum_i d_i}}
#' and
#' \deqn{\hat{\phi} = \frac{1}{n - 1} \sum_i
#' \frac{(m_i - \hat{\theta}d_i)^2}{n_i\hat{\theta}}.}
#' For the registry under test, indexed by \eqn{j}, the statistic is
#' \deqn{Z^2 = \frac{(m_j - \hat{\theta}d_j)^2}
#' {\hat{\phi}n_j\hat{\theta}}},
#' which is compared with a chi-squared distribution with one degree of
#' freedom.
#'
#' A status other than `"ok"` indicates that the row could not be evaluated.
#' Possible values are `"zero_target_incidence"`,
#' `"insufficient_reference_registries"`, `"zero_reference_incidence"`,
#' `"zero_reference_mi"`, and `"zero_reference_dispersion"`. An `unusual`
#' result indicates a statistical difference from the comparison registries;
#' it does not by itself establish incomplete registration.
#'
#' @param x An object with class `quality` or `qualities` containing the
#'   registry identifier `regicode`, incident case counts `fbs`, and death
#'   counts `sws`.
#' @param alpha A single numeric significance level strictly between 0 and 1.
#'   Defaults to `0.05`, corresponding to a chi-squared critical value of
#'   approximately 3.84 with one degree of freedom.
#'
#' @return A tibble containing the stratum identifiers, `regicode`, `fbs`,
#'   `sws`, the observed `mi`, the number of comparison registries
#'   `reference_n`, the estimated common ratio `theta`, the dispersion estimate
#'   `phi`, the test statistic `z2`, `p_value`, `critical_value`, the logical
#'   flag `unusual`, and an evaluation `status`.
#'
#' @references
#' Parkin, D. M., & Bray, F. (2009). Evaluation of data quality in the cancer
#' registry: Principles and methods Part II. Completeness. *European Journal of
#' Cancer*, 45(5), 756-764. \doi{10.1016/j.ejca.2008.11.033}
#'
#' @param reference An optional external reference table. It must contain
#'   regicode, fbs, and sws; registry and cases are accepted as aliases for
#'   regicode and fbs. At least two reference rows are needed in each matched
#'   stratum to estimate dispersion.
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
#'     sws = c(80, 40, 75, 80)
#'   ),
#'   class = c("quality", "data.frame")
#' )
#' compare_mi(quality_data)
compare_mi <- function(x, alpha = 0.05, reference = NULL) {
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

  required <- c("regicode", "fbs", "sws")
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

  validate_mi_counts(x$fbs, "fbs")
  validate_mi_counts(x$sws, "sws")

  measure_columns <- c(
    "rks",
    "fbs",
    "inci",
    "sws",
    "mort",
    "mi",
    "mv",
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
  reference <- prepare_comparison_reference(reference, "mi")
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
    compare_mi_group,
    critical_value = critical_value,
    reference = reference,
    strata = strata
  )
}

validate_mi_counts <- function(x, name) {
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

compare_mi_group <- function(
  data,
  critical_value,
  reference = NULL,
  strata = character()
) {
  n <- nrow(data)
  target_fbs <- data$fbs
  target_sws <- data$sws
  reference_data <- if (is.null(reference)) {
    NULL
  } else {
    select_comparison_reference(reference, data[1, , drop = FALSE], strata)
  }
  reference_n <- if (is.null(reference)) n - 1L else nrow(reference_data)
  status <- rep("ok", n)
  theta <- rep(NA_real_, n)
  phi <- rep(NA_real_, n)
  z2 <- rep(NA_real_, n)
  p_value <- rep(NA_real_, n)
  unusual <- rep(NA, n)

  if (is.null(reference)) {
    reference_fbs <- sum(target_fbs) - target_fbs
    reference_sws <- sum(target_sws) - target_sws
    zero_reference <- sum(target_fbs == 0) - as.integer(target_fbs == 0) > 0
  } else {
    reference_fbs <- sum(reference_data$fbs)
    reference_sws <- sum(reference_data$sws)
    zero_reference <- rep(any(reference_data$fbs == 0), n)
  }

  zero_target <- target_fbs == 0
  status[zero_target] <- "zero_target_incidence"
  insufficient <- !zero_target & reference_n < 2L
  status[insufficient] <- "insufficient_reference_registries"
  no_zero_reference <- !zero_target & !insufficient & !zero_reference
  if (any(no_zero_reference)) {
    if (is.null(reference)) {
      theta[no_zero_reference] <-
        reference_sws[no_zero_reference] /
        reference_fbs[no_zero_reference]
    } else {
      theta[no_zero_reference] <- reference_sws / reference_fbs
    }
    zero_mi <- no_zero_reference & theta == 0
    status[zero_mi] <- "zero_reference_mi"
    evaluable <- no_zero_reference & !zero_mi

    if (any(evaluable)) {
      if (is.null(reference)) {
        reference_total <- target_fbs + target_sws
        sum_sws2_over_total <- sum(target_sws^2 / reference_total)
        sum_sws_fbs_over_total <- sum(target_sws * target_fbs / reference_total)
        sum_fbs2_over_total <- sum(target_fbs^2 / reference_total)
        phi[evaluable] <- (sum_sws2_over_total -
          target_sws[evaluable]^2 /
            reference_total[evaluable] -
          2 *
            theta[evaluable] *
            (sum_sws_fbs_over_total -
              target_sws[evaluable] *
                target_fbs[evaluable] /
                reference_total[evaluable]) +
          theta[evaluable]^2 *
            (sum_fbs2_over_total -
              target_fbs[evaluable]^2 / reference_total[evaluable])) /
          ((reference_n - 1L) * theta[evaluable])
      } else {
        reference_total <- reference_data$fbs + reference_data$sws
        theta_value <- reference_sws / reference_fbs
        phi_value <- sum(
          (reference_data$sws - theta_value * reference_data$fbs)^2 /
            (reference_total * theta_value)
        ) /
          (reference_n - 1L)
        phi[evaluable] <- phi_value
      }
      zero_dispersion <- evaluable & (!is.finite(phi) | phi <= 0)
      status[zero_dispersion] <- "zero_reference_dispersion"
      valid <- evaluable & !zero_dispersion
      if (any(valid)) {
        target_total <- target_fbs + target_sws
        z2[valid] <- (target_sws[valid] - theta[valid] * target_fbs[valid])^2 /
          (phi[valid] * target_total[valid] * theta[valid])
        p_value[valid] <- stats::pchisq(z2[valid], df = 1, lower.tail = FALSE)
        unusual[valid] <- z2[valid] >= critical_value
      }
    }
  }
  status[!zero_target & !insufficient & zero_reference] <-
    "zero_reference_incidence"

  result <- dplyr::tibble(
    reference_n = rep(reference_n, n),
    theta = theta,
    phi = phi,
    z2 = z2,
    p_value = p_value,
    critical_value = rep(critical_value, n),
    unusual = unusual,
    status = status
  )
  identifier_columns <- setdiff(
    names(data),
    c(
      "rks",
      "inci",
      "mort",
      "mi",
      "mv",
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
  data |>
    dplyr::select(dplyr::all_of(identifier_columns)) |>
    dplyr::mutate(
      mi = dplyr::if_else(target_fbs > 0, target_sws / target_fbs, NA_real_),
      .after = "sws"
    ) |>
    dplyr::bind_cols(result)
}
