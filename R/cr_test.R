#' Run cancer-registry quality tests
#'
#' `cr_test()` provides one entry point for the registry-level statistical
#' tests implemented by [compare_mi()], [compare_mv()], and [compare_rate()].
#' For quality data, the default runs the M:I and MV% tests. For an `asr`
#' object, the default runs incidence and mortality rate tests.
#'
#' @param x An object with class `quality` or `qualities`, typically returned
#'   by [create_quality()], or an object with class `asr`/`asrs` returned by
#'   [create_asr()].
#' @param test The test to run. `"all"` runs M:I and MV% for quality data, or
#'   incidence and mortality rate tests for ASR data. `"mi"` and `"mv"` select
#'   quality tests; `"inci"` and `"mort"` select CI5-style rate tests.
#' @param reference An optional external reference table passed to
#'   the selected comparison. For `mi` and `mv`, it must contain the required
#'   counts; for `inci` and `mort`, it must contain the selected rate and
#'   variance columns. CI5-XII incidence data alone do not contain death counts
#'   or MV counts and cannot be used directly for the first two tests.
#' @param alpha A single numeric significance level strictly between 0 and 1.
#'   Defaults to `0.05`.
#' @param standard Standard-population suffix used by the rate test. Defaults
#'   to `"wld85"`. Use `"cr"` for crude rates.
#' @param multiplier Rate multiplier used only for legacy unmultiplied rate
#'   variances. Defaults to `100000`.
#' @param variance_scale Scale of rate variance columns passed to
#'   [compare_rate()]. The default, `"display"`, matches current [create_asr()]
#'   output; use `"unmultiplied"` for legacy variance columns.
#'
#' @return A tibble containing the selected result for a single test. With
#'   `test = "all"`, a named list with class `cr_tests` is returned.
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
#'     sws = c(80, 40, 75, 80),
#'     mv = c(95, 70, 80, 75)
#'   ),
#'   class = c("quality", "data.frame")
#' )
#' tests <- cr_test(quality_data)
#' tests$mi
cr_test <- function(
  x,
  test = c("all", "mi", "mv", "inci", "mort"),
  reference = NULL,
  standard = "wld85",
  alpha = 0.05,
  multiplier = 100000,
  variance_scale = c("display", "unmultiplied")
) {
  test <- match.arg(test)
  variance_scale <- match.arg(variance_scale)

  if (test == "mi") {
    return(compare_mi(x, reference = reference, alpha = alpha))
  }

  if (test == "mv") {
    return(compare_mv(x, reference = reference, alpha = alpha))
  }

  if (test == "inci") {
    return(compare_rate(
      x,
      event = "inci",
      standard = standard,
      reference = reference,
      alpha = alpha,
      multiplier = multiplier,
      variance_scale = variance_scale
    ))
  }

  if (test == "mort") {
    return(compare_rate(
      x,
      event = "mort",
      standard = standard,
      reference = reference,
      alpha = alpha,
      multiplier = multiplier,
      variance_scale = variance_scale
    ))
  }

  if (inherits(x, c("asr", "asrs"))) {
    return(structure(
      list(
        inci = compare_rate(
          x,
          event = "inci",
          standard = standard,
          reference = reference,
          alpha = alpha,
          multiplier = multiplier,
          variance_scale = variance_scale
        ),
        mort = compare_rate(
          x,
          event = "mort",
          standard = standard,
          reference = reference,
          alpha = alpha,
          multiplier = multiplier,
          variance_scale = variance_scale
        )
      ),
      class = c("cr_tests", "list")
    ))
  }

  structure(
    list(
      mi = compare_mi(x, reference = reference, alpha = alpha),
      mv = compare_mv(x, reference = reference, alpha = alpha)
    ),
    class = c("cr_tests", "list")
  )
}

prepare_comparison_reference <- function(reference, kind) {
  if (is.null(reference)) {
    return(NULL)
  }

  if (inherits(reference, "qualities")) {
    reference <- cr_merge(reference)
  }

  if (!is.data.frame(reference)) {
    stop(
      "reference must be a data frame or an object with class 'qualities'.",
      call. = FALSE
    )
  }
  if (nrow(reference) == 0L) {
    stop("reference must contain at least one row.", call. = FALSE)
  }

  if (!"regicode" %in% names(reference) && "registry" %in% names(reference)) {
    reference$regicode <- reference$registry
  }
  if (!"fbs" %in% names(reference) && "cases" %in% names(reference)) {
    reference$fbs <- reference$cases
  }
  reference <- standardize_year_sex(reference)

  required <- if (kind == "mi") {
    c("regicode", "fbs", "sws")
  } else {
    c("regicode", "fbs")
  }
  missing <- setdiff(required, names(reference))
  if (length(missing) > 0L) {
    stop(
      "reference is missing required columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (
    anyNA(reference$regicode) ||
      any(trimws(as.character(reference$regicode)) == "")
  ) {
    stop(
      "reference$regicode must not contain missing or empty values.",
      call. = FALSE
    )
  }

  if (kind == "mi") {
    validate_mi_counts(reference$fbs, "reference$fbs")
    validate_mi_counts(reference$sws, "reference$sws")
  } else {
    if (!"mv" %in% names(reference) && !"mv_cases" %in% names(reference)) {
      stop(
        "reference must contain either mv or mv_cases.",
        call. = FALSE
      )
    }
    validate_mv_counts(reference$fbs, "reference$fbs")
    if ("mv" %in% names(reference)) {
      validate_mv_percent(reference$mv, reference$fbs)
    }
    if ("mv_cases" %in% names(reference)) {
      validate_mv_counts(reference$mv_cases, "reference$mv_cases")
      if (any(reference$mv_cases > reference$fbs)) {
        stop(
          "reference$mv_cases must not exceed reference$fbs.",
          call. = FALSE
        )
      }
    } else {
      reference$mv_cases <- ifelse(
        reference$fbs > 0,
        round(reference$fbs * reference$mv / 100),
        0
      )
    }
  }

  reference
}

validate_reference_keys <- function(reference, strata) {
  if (is.null(reference)) {
    return(invisible(NULL))
  }

  common_strata <- intersect(strata, names(reference))
  key_columns <- c(common_strata, "regicode")
  if (anyDuplicated(reference[key_columns])) {
    stop(
      "Each external reference regicode must occur only once within each ",
      "matched stratum.",
      call. = FALSE
    )
  }
}

select_comparison_reference <- function(reference, target, strata) {
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
