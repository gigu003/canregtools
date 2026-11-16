#' Compare incidence or mortality rates
#'
#' Compare a registry rate with an external CI5-style reference rate using
#' the standard errors of the two rates. The test is performed on the
#' logarithm of the rate ratio, so the null hypothesis is that the rate ratio
#' equals one.
#'
#' The input is normally an object returned by `create_asr(..., show_var =
#' TRUE)` or `create_asr(..., show_se = TRUE)`. Existing variance columns are
#' preferred when both forms are present. The reference table must contain one
#' matching row for each target stratum.
#'
#' @param x An asr data frame, an asrs list, or a data frame containing
#'   regicode, the selected rate column, and its standard-error or variance
#'   column.
#' @param event The event label to report: inci for incidence or mort for
#'   mortality.
#' @param standard Standard-population suffix used to find the rate columns.
#'   Defaults to wld85. Use cr to compare crude rates.
#' @param reference An external reference table with the same rate and
#'   uncertainty columns as x. It may omit regicode when it contains one row.
#'   Reference rows are matched using stratum columns shared with x.
#' @param rate_col Optional explicit rate column name. Defaults to
#'   asr_standard, or cr when standard is cr.
#' @param se_col Optional explicit standard-error column name. When neither
#'   uncertainty-column argument is supplied, existing variance columns are
#'   preferred, followed by `asr_se_standard` or `cr_se`.
#' @param variance_col Optional explicit variance column name. Used only when
#'   `se_col` is not supplied.
#' @param alpha Two-sided significance level. Defaults to 0.05.
#' @param multiplier Rate multiplier used only when
#'   `variance_scale = "unmultiplied"`. Defaults to `100000`.
#' @param variance_scale Scale of supplied variance columns. The default,
#'   `"display"`, matches current [create_asr()] output. Use `"unmultiplied"`
#'   for legacy variance columns that have not been multiplied by
#'   `multiplier^2`. Standard-error columns are always interpreted on the
#'   displayed rate scale.
#'
#' @return A tibble with the target stratum, rate, reference rate, rate
#'   difference, rate ratio, log-rate-ratio standard error, z statistic,
#'   p_value, confidence limits for the rate ratio, unusual, and status.
#'
#' @export
compare_rate <- function(
  x,
  event = c("inci", "mort"),
  standard = "wld85",
  reference = NULL,
  rate_col = NULL,
  se_col = NULL,
  variance_col = NULL,
  alpha = 0.05,
  multiplier = 100000,
  variance_scale = c("display", "unmultiplied")
) {
  event <- match.arg(event)
  variance_scale <- match.arg(variance_scale)
  validate_rate_alpha(alpha)
  validate_rate_multiplier(multiplier)

  x <- resolve_rate_data(x, "x")
  x <- standardize_year_sex(x)
  if (!is.data.frame(x) || nrow(x) == 0L) {
    stop("x must contain at least one rate row.", call. = FALSE)
  }
  if (!"regicode" %in% names(x)) {
    x$regicode <- "target"
  }

  columns <- resolve_rate_columns(
    x,
    standard,
    rate_col,
    se_col,
    variance_col
  )
  validate_rate_columns(x, columns, "x")
  reference <- resolve_rate_data(reference, "reference")
  if (is.null(reference)) {
    stop(
      "reference is required for incidence or mortality rate comparison.",
      call. = FALSE
    )
  }
  reference <- standardize_year_sex(reference)
  if (nrow(reference) == 0L) {
    stop("reference must contain at least one rate row.", call. = FALSE)
  }
  if (!"regicode" %in% names(reference)) {
    reference$regicode <- "reference"
  }
  reference_columns <- resolve_rate_columns(
    reference,
    standard,
    rate_col,
    se_col,
    variance_col
  )
  validate_rate_columns(reference, reference_columns, "reference")

  rate_measure_columns <- grep(
    "^(no_cases|cr($|_)|asr_|truncr_|cumur$|prop$|rank$)",
    names(x),
    value = TRUE
  )
  strata <- setdiff(
    names(x),
    c(
      "regicode",
      rate_measure_columns,
      unname(unlist(columns)),
      unname(unlist(reference_columns))
    )
  )
  validate_rate_keys(x, strata, "x")
  validate_rate_keys(
    reference,
    intersect(strata, names(reference)),
    "reference"
  )

  grouped <- if (length(strata) == 0L) {
    list(x)
  } else {
    x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(strata)), .drop = FALSE) |>
      dplyr::group_split(.keep = TRUE)
  }

  critical_value <- stats::qnorm(1 - alpha / 2)
  purrr::map_dfr(
    grouped,
    compare_rate_group,
    reference = reference,
    strata = strata,
    columns = columns,
    reference_columns = reference_columns,
    event = event,
    standard = standard,
    critical_value = critical_value,
    multiplier = multiplier,
    variance_scale = variance_scale
  )
}

resolve_rate_data <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "asrs")) {
    x <- cr_merge(x)
  }
  if (!is.data.frame(x)) {
    stop(
      name,
      " must be a data frame or an object with class asrs.",
      call. = FALSE
    )
  }
  x
}

resolve_rate_columns <- function(
  data,
  standard,
  rate_col,
  se_col,
  variance_col
) {
  if (
    !is.character(standard) ||
      length(standard) != 1L ||
      is.na(standard) ||
      !nzchar(standard)
  ) {
    stop("standard must be a single non-empty character value.", call. = FALSE)
  }

  default_rate <- if (identical(standard, "cr")) {
    "cr"
  } else {
    paste0("asr_", standard)
  }
  default_se <- if (identical(standard, "cr")) {
    "cr_se"
  } else {
    paste0("asr_se_", standard)
  }
  default_variance <- if (identical(standard, "cr")) {
    "cr_var"
  } else {
    paste0("asr_var_", standard)
  }

  if (!is.null(se_col) && !is.null(variance_col)) {
    stop("Supply only one of `se_col` and `variance_col`.", call. = FALSE)
  }

  if (!is.null(se_col)) {
    uncertainty <- se_col
    uncertainty_type <- "se"
  } else if (!is.null(variance_col)) {
    uncertainty <- variance_col
    uncertainty_type <- "variance"
  } else if (default_variance %in% names(data)) {
    uncertainty <- default_variance
    uncertainty_type <- "variance"
  } else if (default_se %in% names(data)) {
    uncertainty <- default_se
    uncertainty_type <- "se"
  } else {
    uncertainty <- default_variance
    uncertainty_type <- "variance"
  }

  list(
    rate = rate_col %||% default_rate,
    uncertainty = uncertainty,
    uncertainty_type = uncertainty_type
  )
}

validate_rate_columns <- function(data, columns, name) {
  required <- unname(unlist(columns[c("rate", "uncertainty")]))
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      name,
      " is missing required rate columns: ",
      paste(missing, collapse = ", "),
      ". Use create_asr(..., show_var = TRUE), create_asr(..., show_se = ",
      "TRUE), or supply an uncertainty column explicitly.",
      call. = FALSE
    )
  }

  rate <- data[[columns$rate]]
  uncertainty <- data[[columns$uncertainty]]
  if (!is.numeric(rate) || !is.numeric(uncertainty)) {
    stop(name, " rate and uncertainty columns must be numeric.", call. = FALSE)
  }
  if (
    any(!is.na(rate) & (!is.finite(rate) | rate < 0)) ||
      any(!is.na(uncertainty) & (!is.finite(uncertainty) | uncertainty < 0))
  ) {
    stop(
      name,
      " rate and uncertainty values must be finite and non-negative when present.",
      call. = FALSE
    )
  }
}

validate_rate_alpha <- function(alpha) {
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    stop(
      "alpha must be a single number strictly between 0 and 1.",
      call. = FALSE
    )
  }
}

validate_rate_multiplier <- function(multiplier) {
  if (
    !is.numeric(multiplier) ||
      length(multiplier) != 1L ||
      is.na(multiplier) ||
      !is.finite(multiplier) ||
      multiplier <= 0
  ) {
    stop(
      "multiplier must be a single finite number greater than zero.",
      call. = FALSE
    )
  }
}

validate_rate_keys <- function(data, strata, name) {
  if (anyNA(data$regicode) || any(trimws(as.character(data$regicode)) == "")) {
    stop(
      name,
      " regicode must not contain missing or empty values.",
      call. = FALSE
    )
  }
  key_columns <- c(strata, "regicode")
  if (anyDuplicated(data[key_columns])) {
    stop(
      name,
      " must contain only one regicode within each detected stratum.",
      call. = FALSE
    )
  }
}

select_rate_reference <- function(reference, target, strata) {
  common_strata <- intersect(strata, names(reference))
  if (length(common_strata) == 0L) {
    return(reference)
  }

  keep <- rep(TRUE, nrow(reference))
  for (column in common_strata) {
    value <- target[[column]][[1]]
    if (is.na(value)) {
      keep <- keep & is.na(reference[[column]])
    } else {
      keep <- keep &
        !is.na(reference[[column]]) &
        as.character(reference[[column]]) == as.character(value)
    }
  }
  reference[keep, , drop = FALSE]
}

compare_rate_group <- function(
  data,
  reference,
  strata,
  columns,
  reference_columns,
  event,
  standard,
  critical_value,
  multiplier,
  variance_scale
) {
  n <- nrow(data)
  reference_data <- select_rate_reference(
    reference,
    data[1, , drop = FALSE],
    strata
  )
  reference_n <- nrow(reference_data)
  target_rate <- data[[columns$rate]]
  target_uncertainty <- data[[columns$uncertainty]]
  status <- rep("ok", n)
  reference_rate <- rep(NA_real_, n)
  reference_uncertainty <- rep(NA_real_, n)

  if (reference_n != 1L) {
    status <- rep(
      if (reference_n == 0L) {
        "missing_reference_rate"
      } else {
        "multiple_reference_rates"
      },
      n
    )
  } else {
    reference_rate <- rep(reference_data[[reference_columns$rate]][[1]], n)
    reference_uncertainty <- rep(
      reference_data[[reference_columns$uncertainty]][[1]],
      n
    )
    missing_rate <- is.na(target_rate) | is.na(reference_rate)
    missing_variance <- !missing_rate &
      (is.na(target_uncertainty) | is.na(reference_uncertainty))
    nonpositive <- !missing_rate &
      !missing_variance &
      (target_rate <= 0 | reference_rate <= 0)
    status[missing_rate] <- "missing_rate"
    status[missing_variance] <- "missing_rate_variance"
    status[nonpositive] <- "nonpositive_rate"
  }

  result <- dplyr::tibble(
    event = rep(event, n),
    standard = rep(standard, n),
    multiplier = rep(multiplier, n),
    variance_scale = rep(variance_scale, n),
    reference_n = rep(reference_n, n),
    rate = target_rate,
    reference_rate = reference_rate,
    rate_difference = rep(NA_real_, n),
    rate_ratio = rep(NA_real_, n),
    se_log_rate_ratio = rep(NA_real_, n),
    z = rep(NA_real_, n),
    z2 = rep(NA_real_, n),
    p_value = rep(NA_real_, n),
    rate_ratio_lower = rep(NA_real_, n),
    rate_ratio_upper = rep(NA_real_, n),
    critical_value = rep(critical_value, n),
    unusual = rep(NA, n),
    status = status
  )

  ok <- status == "ok"
  if (any(ok)) {
    result$rate_difference[ok] <- target_rate[ok] - reference_rate[ok]
    result$rate_ratio[ok] <- target_rate[ok] / reference_rate[ok]
    target_se <- rate_standard_error(
      target_uncertainty[ok],
      columns$uncertainty_type,
      multiplier,
      variance_scale
    )
    reference_se <- rate_standard_error(
      reference_uncertainty[ok],
      reference_columns$uncertainty_type,
      multiplier,
      variance_scale
    )
    se_log_rate_ratio <- sqrt(
      (target_se / target_rate[ok])^2 +
        (reference_se / reference_rate[ok])^2
    )
    result$se_log_rate_ratio[ok] <- se_log_rate_ratio
    zero_variance <- !is.finite(se_log_rate_ratio) | se_log_rate_ratio <= 0
    ok_index <- which(ok)
    result$status[ok_index[zero_variance]] <- "zero_rate_variance"
    valid <- ok_index[!zero_variance]
    if (length(valid) > 0L) {
      se <- result$se_log_rate_ratio[valid]
      ratio <- result$rate_ratio[valid]
      z <- log(ratio) / se
      result$z[valid] <- z
      result$z2[valid] <- z^2
      result$p_value[valid] <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
      result$rate_ratio_lower[valid] <- ratio * exp(-critical_value * se)
      result$rate_ratio_upper[valid] <- ratio * exp(critical_value * se)
      result$unusual[valid] <- abs(z) >= critical_value
    }
  }

  data |>
    dplyr::select(dplyr::all_of(c("regicode", strata))) |>
    dplyr::bind_cols(result)
}

rate_standard_error <- function(
  uncertainty,
  type,
  multiplier,
  variance_scale
) {
  if (identical(type, "se")) {
    return(uncertainty)
  }
  scale <- if (identical(variance_scale, "unmultiplied")) multiplier else 1
  sqrt(uncertainty) * scale
}
