#' Detect unusual annual rates or quality indicators
#'
#' `detect_anomaly()` identifies annual observations that deserve review in
#' outputs from [create_asr()] or [create_quality()]. The default
#' `"uncertainty"` method compares consecutive years on the log-rate scale and
#' uses the standard errors implied by the underlying counts or reported
#' variances. The `"robust"` method compares each observation with the
#' historical median using the median absolute deviation (MAD).
#'
#' This function is intended for quality-control screening. An `anomaly` flag
#' is not evidence that a registry value is wrong, and the unadjusted
#' `p_value` returned by the uncertainty method is not a substitute for a
#' prespecified trend model or multiplicity-adjusted inference.
#'
#' @param x An object returned by [create_asr()] or [create_quality()]. Objects
#'   returned with `collapse = FALSE` are also supported.
#' @param metrics Character vector of metric columns to check. For `asr`
#'   objects the default is `c("cr", "asr_wld85")`; for `quality` objects the
#'   default is `c("inci", "mort", "mv", "mi")`.
#' @param method Detection method: `"uncertainty"` compares consecutive
#'   years, while `"robust"` uses the historical median and MAD.
#' @param by Character vector of columns defining independent time series.
#'   The default uses the available columns among `regicode`, `sex`, and
#'   `cancer`.
#' @param year Name of the year column. Defaults to `"year"`.
#' @param threshold Absolute score at which an observation is flagged.
#'   Defaults to `3`.
#' @param min_history Minimum number of usable historical values required by
#'   the robust method. Defaults to `4`.
#' @param multiplier Rate multiplier used by `create_asr()` and
#'   `create_quality()`. For ASR variance columns, it is used only when
#'   `variance_scale = "unmultiplied"`. Defaults to `100000`.
#' @param zero_policy Whether zero-valued observations are excluded from
#'   comparisons or included where the selected method permits it. The
#'   default is `"exclude"`.
#' @param variance_scale Scale of ASR variance columns. The default,
#'   `"display"`, matches current [create_asr()] output. Use `"unmultiplied"`
#'   for legacy variance columns. Standard-error columns are always interpreted
#'   on the displayed rate scale.
#'
#' @return A tibble with one row per input observation and metric. It includes
#'   grouping columns, `year`, `metric`, `value`, `expected`, `se`,
#'   `reference_se`, `scale`, `score`, `p_value`, `direction`, `anomaly`, and
#'   `status`.
#'
#' @seealso [create_asr()], [create_quality()]
#'
#' @references
#' International Agency for Research on Cancer. *Cancer Incidence in Five
#' Continents, Volume XII: Statistical methods for registries*.
#' <https://publications.iarc.who.int/_publications/media/download/3523/>
#'
#' @export
#'
#' @examples
#' data("canregs")
#' fbsws <- count_canreg(canregs)
#' asr <- create_asr(
#'   fbsws,
#'   year,
#'   show_var = TRUE,
#'   collapse = TRUE
#' )
#' detect_anomaly(asr, metrics = "asr_wld85")
detect_anomaly <- function(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 100000,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
) {
  method <- match.arg(method)
  zero_policy <- match.arg(zero_policy)
  variance_scale <- match.arg(variance_scale)
  validate_anomaly_arguments(
    threshold = threshold,
    min_history = min_history,
    multiplier = multiplier,
    year = year
  )
  UseMethod("detect_anomaly", x)
}

#' @export
#' @rdname detect_anomaly
detect_anomaly.asr <- function(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 100000,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
) {
  detect_anomaly_frame(
    x,
    metrics = if (is.null(metrics)) c("cr", "asr_wld85") else metrics,
    kind = "asr",
    method = match.arg(method),
    by = by,
    year = year,
    threshold = threshold,
    min_history = min_history,
    multiplier = multiplier,
    zero_policy = match.arg(zero_policy),
    variance_scale = match.arg(variance_scale)
  )
}

#' @export
#' @rdname detect_anomaly
detect_anomaly.asrs <- function(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 100000,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
) {
  detect_anomaly(
    cr_merge(x),
    metrics = metrics,
    method = method,
    by = by,
    year = year,
    threshold = threshold,
    min_history = min_history,
    multiplier = multiplier,
    zero_policy = zero_policy,
    variance_scale = variance_scale
  )
}

#' @export
#' @rdname detect_anomaly
detect_anomaly.quality <- function(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 100000,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
) {
  detect_anomaly_frame(
    x,
    metrics = if (is.null(metrics)) c("inci", "mort", "mv", "mi") else metrics,
    kind = "quality",
    method = match.arg(method),
    by = by,
    year = year,
    threshold = threshold,
    min_history = min_history,
    multiplier = multiplier,
    zero_policy = match.arg(zero_policy),
    variance_scale = match.arg(variance_scale)
  )
}

#' @export
#' @rdname detect_anomaly
detect_anomaly.qualities <- function(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 100000,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
) {
  detect_anomaly(
    cr_merge(x),
    metrics = metrics,
    method = method,
    by = by,
    year = year,
    threshold = threshold,
    min_history = min_history,
    multiplier = multiplier,
    zero_policy = zero_policy,
    variance_scale = variance_scale
  )
}

#' @export
#' @rdname detect_anomaly
detect_anomaly.default <- function(
  x,
  metrics = NULL,
  method = c("uncertainty", "robust"),
  by = NULL,
  year = "year",
  threshold = 3,
  min_history = 4,
  multiplier = 100000,
  zero_policy = c("exclude", "include"),
  variance_scale = c("display", "unmultiplied")
) {
  stop(
    "`x` must inherit from class 'asr', 'asrs', 'quality', or 'qualities'.",
    call. = FALSE
  )
}

detect_anomaly_frame <- function(
  data,
  metrics,
  kind,
  method,
  by,
  year,
  threshold,
  min_history,
  multiplier,
  zero_policy,
  variance_scale
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`x` must contain at least one row.", call. = FALSE)
  }

  required <- c(year, metrics)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      "`x` is missing required columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!is.character(metrics) || length(metrics) == 0L) {
    stop("`metrics` must contain at least one column name.", call. = FALSE)
  }
  if (anyDuplicated(metrics)) {
    stop("`metrics` must not contain duplicates.", call. = FALSE)
  }
  if (any(!vapply(data[metrics], is.numeric, logical(1)))) {
    stop("All selected metric columns must be numeric.", call. = FALSE)
  }

  if (is.null(by)) {
    by <- intersect(c("regicode", "sex", "cancer"), names(data))
  }
  if (!is.character(by) || any(!by %in% names(data))) {
    stop("`by` must contain existing column names.", call. = FALSE)
  }
  if (year %in% by) {
    stop("`year` must not be included in `by`.", call. = FALSE)
  }

  parse_anomaly_years(data[[year]])

  key <- c(by, year)
  duplicates <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  duplicates <- duplicates[duplicates$n > 1L, , drop = FALSE]
  if (nrow(duplicates) > 0L) {
    stop(
      "Each time series must contain at most one row per year.",
      call. = FALSE
    )
  }

  groups <- if (length(by) == 0L) {
    list(data)
  } else {
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::group_split(.keep = TRUE)
  }

  result <- purrr::map_dfr(
    groups,
    function(group) {
      purrr::map_dfr(
        metrics,
        function(metric) {
          detect_anomaly_metric(
            group,
            metric = metric,
            kind = kind,
            method = method,
            year = year,
            threshold = threshold,
            min_history = min_history,
            multiplier = multiplier,
            zero_policy = zero_policy,
            variance_scale = variance_scale,
            by = by
          )
        }
      )
    }
  )

  class(result) <- c("anomaly", class(result))
  result
}

detect_anomaly_metric <- function(
  data,
  metric,
  kind,
  method,
  year,
  threshold,
  min_history,
  multiplier,
  zero_policy,
  variance_scale,
  by
) {
  year_label <- as.character(data[[year]])
  year_value <- parse_anomaly_years(year_label)
  value <- as.numeric(data[[metric]])
  order_index <- order(year_value)
  year_value <- year_value[order_index]
  year_label <- year_label[order_index]
  value <- value[order_index]
  data <- data[order_index, , drop = FALSE]

  se <- if (method == "uncertainty") {
    anomaly_standard_error(
      data,
      metric = metric,
      kind = kind,
      multiplier = multiplier,
      variance_scale = variance_scale
    )
  } else {
    rep(NA_real_, length(value))
  }

  output <- if (method == "uncertainty") {
    detect_uncertainty_metric(
      value,
      se,
      year_value,
      threshold = threshold,
      zero_policy = zero_policy
    )
  } else {
    detect_robust_metric(
      value,
      threshold = threshold,
      min_history = min_history,
      zero_policy = zero_policy
    )
  }

  output <- dplyr::bind_cols(
    data[by],
    dplyr::tibble(year = year_label, metric = metric, value = value),
    output
  )
  output |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, "year", "metric"))))
}

parse_anomaly_years <- function(x) {
  label <- trimws(as.character(x))
  valid <- grepl("^[0-9]{4}$", label)
  value <- suppressWarnings(as.integer(label))
  if (any(!valid | is.na(value))) {
    stop(
      paste(
        "`detect_anomaly()` requires single four-digit calendar-year labels;",
        "period labels such as `2003-2007` are not supported."
      ),
      call. = FALSE
    )
  }
  value
}

anomaly_standard_error <- function(
  data,
  metric,
  kind,
  multiplier,
  variance_scale
) {
  if (kind == "asr") {
    variance_name <- if (metric == "cr") {
      "cr_var"
    } else {
      paste0("asr_var_", sub("^asr_", "", metric))
    }
    if (variance_name %in% names(data)) {
      variance <- data[[variance_name]]
      scale <- if (identical(variance_scale, "unmultiplied")) multiplier else 1
      return(sqrt(pmax(as.numeric(variance), 0)) * scale)
    }

    se_name <- if (metric == "cr") {
      "cr_se"
    } else {
      paste0("asr_se_", sub("^asr_", "", metric))
    }
    if (se_name %in% names(data)) {
      return(pmax(as.numeric(data[[se_name]]), 0))
    }
    return(rep(NA_real_, nrow(data)))
  }

  if (metric %in% c("inci", "mort")) {
    count_name <- if (metric == "inci") "fbs" else "sws"
    if (!all(c(count_name, "rks") %in% names(data))) {
      return(rep(NA_real_, nrow(data)))
    }
    count <- as.numeric(data[[count_name]])
    population <- as.numeric(data$rks)
    return(ifelse(
      is.finite(count) & is.finite(population) & population > 0 & count >= 0,
      multiplier * sqrt(count) / population,
      NA_real_
    ))
  }

  if (metric == "mv") {
    if (!all(c("fbs", "mv") %in% names(data))) {
      return(rep(NA_real_, nrow(data)))
    }
    cases <- as.numeric(data$fbs)
    proportion <- as.numeric(data$mv) / 100
    return(ifelse(
      is.finite(cases) &
        cases > 0 &
        is.finite(proportion) &
        proportion >= 0 &
        proportion <= 1,
      100 * sqrt(proportion * (1 - proportion) / cases),
      NA_real_
    ))
  }

  if (metric == "mi") {
    if (!all(c("fbs", "sws") %in% names(data))) {
      return(rep(NA_real_, nrow(data)))
    }
    value <- as.numeric(data$mi)
    fbs <- as.numeric(data$fbs)
    sws <- as.numeric(data$sws)
    return(ifelse(
      is.finite(value) & value >= 0 & fbs > 0 & sws > 0,
      value * sqrt(1 / fbs + 1 / sws),
      NA_real_
    ))
  }

  rep(NA_real_, nrow(data))
}

detect_uncertainty_metric <- function(value, se, year, threshold, zero_policy) {
  previous_value <- c(NA_real_, utils::head(value, -1L))
  previous_se <- c(NA_real_, utils::head(se, -1L))
  year_gap <- c(NA_real_, diff(year))
  scale <- sqrt((se / value)^2 + (previous_se / previous_value)^2)
  valid <- is.finite(value) &
    is.finite(previous_value) &
    is.finite(se) &
    is.finite(previous_se) &
    value > 0 &
    previous_value > 0 &
    year_gap == 1 &
    is.finite(scale) &
    scale > 0
  score <- ifelse(valid, log(value / previous_value) / scale, NA_real_)
  status <- rep("ok", length(value))
  status[!is.finite(value)] <- "missing_value"
  status[is.finite(value) & value == 0 & zero_policy == "exclude"] <-
    "zero_value"
  status[is.finite(value) & value > 0 & !is.finite(se)] <- "missing_se"
  status[
    is.finite(value) & value > 0 & is.finite(se) & is.na(previous_value)
  ] <-
    "no_previous_year"
  status[
    is.finite(value) &
      is.finite(previous_value) &
      (previous_value == 0 | value == 0)
  ] <- "zero_value"
  status[is.finite(value) & is.finite(previous_value) & year_gap != 1] <-
    "nonconsecutive_year"
  status[
    is.finite(value) &
      is.finite(previous_value) &
      is.finite(se) &
      is.finite(previous_se) &
      scale <= 0
  ] <- "zero_scale"

  dplyr::tibble(
    expected = previous_value,
    se = se,
    reference_se = previous_se,
    scale = scale,
    score = score,
    p_value = ifelse(
      is.finite(score),
      2 * stats::pnorm(-abs(score)),
      NA_real_
    ),
    direction = score_direction(score),
    anomaly = is.finite(score) & abs(score) >= threshold,
    status = status
  )
}

detect_robust_metric <- function(value, threshold, min_history, zero_policy) {
  usable <- is.finite(value) & (value != 0 | zero_policy == "include")
  usable_n <- sum(usable)
  expected <- if (usable_n > 0L) stats::median(value[usable]) else NA_real_
  scale <- if (usable_n > 1L) {
    stats::mad(value[usable], constant = 1)
  } else {
    NA_real_
  }
  score <- if (usable_n >= min_history && is.finite(scale) && scale > 0) {
    0.6745 * (value - expected) / scale
  } else {
    rep(NA_real_, length(value))
  }
  score[!usable] <- NA_real_
  status <- rep("ok", length(value))
  status[!is.finite(value)] <- "missing_value"
  status[is.finite(value) & value == 0 & zero_policy == "exclude"] <-
    "zero_value"
  status[usable & usable_n < min_history] <- "insufficient_history"
  status[
    usable & usable_n >= min_history & (!is.finite(scale) | scale <= 0)
  ] <- "zero_scale"

  dplyr::tibble(
    expected = rep(expected, length(value)),
    se = rep(NA_real_, length(value)),
    reference_se = rep(NA_real_, length(value)),
    scale = rep(scale, length(value)),
    score = score,
    p_value = rep(NA_real_, length(value)),
    direction = score_direction(score),
    anomaly = is.finite(score) & abs(score) >= threshold,
    status = status
  )
}

score_direction <- function(score) {
  ifelse(
    !is.finite(score),
    NA_character_,
    ifelse(score > 0, "high", ifelse(score < 0, "low", "normal"))
  )
}

validate_anomaly_arguments <- function(
  threshold,
  min_history,
  multiplier,
  year
) {
  if (!is.character(year) || length(year) != 1L || is.na(year) || year == "") {
    stop("`year` must be one non-empty column name.", call. = FALSE)
  }
  if (
    !is.numeric(threshold) ||
      length(threshold) != 1L ||
      !is.finite(threshold) ||
      threshold <= 0
  ) {
    stop("`threshold` must be one positive finite number.", call. = FALSE)
  }
  if (
    !is.numeric(min_history) ||
      length(min_history) != 1L ||
      !is.finite(min_history) ||
      min_history < 2 ||
      min_history != as.integer(min_history)
  ) {
    stop(
      "`min_history` must be one integer greater than or equal to 2.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(multiplier) ||
      length(multiplier) != 1L ||
      !is.finite(multiplier) ||
      multiplier <= 0
  ) {
    stop("`multiplier` must be one positive finite number.", call. = FALSE)
  }
}
