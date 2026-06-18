#' Create a standardized cancer registry report data object
#'
#' `create_canreg_report_data()` builds a compact report-ready object from
#' `canregs` or `fbswicds` data. The object stores summary-level indicators
#' for the province/all areas, urban areas, and rural areas, as well as
#' registry-level indicators for individual registries.
#'
#' @param data An object of class `canregs` or `fbswicds`.
#' @param year Data year stored as metadata. The statistical calculations still
#'   use the `year` column in the input data as a grouping variable.
#' @param province,province_en Province names stored as metadata.
#' @param population_total Optional total provincial population stored as
#'   metadata.
#' @param summary_codes Named character vector identifying the summary groups.
#'   Defaults to `c(all = "410000", urban = "910000", rural = "920000")`.
#' @param cancer_codes Cancer codes to store as metadata for report generation.
#' @param cancer_total_code Code for all malignant cancers. Defaults to `60`.
#' @param cancer_type Cancer classification type passed to [count_canreg()] when
#'   `data` is a `canregs` object.
#' @param keep_source Logical. If `TRUE`, keep intermediate `fbswicd` objects in
#'   `source`.
#' @param include_age_registry Logical. If `TRUE`, calculate registry-level
#'   age-specific incidence and mortality rates.
#'
#' @return An object of class `canreg_report`.
#' @export
#'
#' @examples
#' \dontrun{
#' data("canregs")
#' report_data <- create_canreg_report_data(canregs, year = 2022)
#' summary(report_data)
#' }
create_canreg_report_data <- function(data,
                                      year,
                                      province = NULL,
                                      province_en = NULL,
                                      population_total = NULL,
                                      summary_codes = c(
                                        all = "410000",
                                        urban = "910000",
                                        rural = "920000"
                                      ),
                                      cancer_codes = c(101:110, 112, 114:118, 120:125),
                                      cancer_total_code = 60,
                                      cancer_type = "big",
                                      keep_source = TRUE,
                                      include_age_registry = TRUE) {
  if (!inherits(data, c("canregs", "fbswicds"))) {
    stop("`data` must be an object of class 'canregs' or 'fbswicds'.", call. = FALSE)
  }

  summary_codes <- validate_summary_codes(summary_codes)

  if (inherits(data, "canregs")) {
    data_registry <- cr_reframe(data, "registry")
    data_summary <- cr_reframe(data_registry, c("area_type", "province"))
    fbsw_registry <- count_canreg(data_registry, cancer_type = cancer_type)
    fbsw_summary <- count_canreg(data_summary, label_tail = "", cancer_type = cancer_type)
  } else {
    fbsw_registry <- cr_reframe(data, "registry")
    fbsw_summary <- cr_reframe(fbsw_registry, c("area_type", "province"))
  }

  year_sym <- rlang::sym("year")
  sex_sym <- rlang::sym("sex")
  cancer_sym <- rlang::sym("cancer")

  add_report_labels <- function(x) {
    res <- add_labels(
      x,
      label_type = c("full", "abbr", "abbr"),
      lang = "cn",
      suffix = TRUE
    )
    add_labels(
      res,
      label_type = c("full", "abbr", "abbr"),
      lang = "en",
      suffix = TRUE
    )
  }

  inci_summary <- create_asr(
    fbsw_summary, !!year_sym, !!sex_sym, !!cancer_sym,
    event = "fbs", collapse = TRUE
  ) |>
    add_report_labels()

  mort_summary <- create_asr(
    fbsw_summary, !!year_sym, !!sex_sym, !!cancer_sym,
    event = "sws", collapse = TRUE
  ) |>
    add_report_labels()

  inci_registry <- create_asr(
    fbsw_registry, !!year_sym, !!sex_sym, !!cancer_sym,
    event = "fbs", collapse = TRUE
  ) |>
    add_report_labels()

  mort_registry <- create_asr(
    fbsw_registry, !!year_sym, !!sex_sym, !!cancer_sym,
    event = "sws", collapse = TRUE
  ) |>
    add_report_labels()

  inci_age_summary <- create_age_rate(
    fbsw_summary, !!year_sym, !!sex_sym, !!cancer_sym,
    event = "fbs", collapse = TRUE
  ) |>
    add_report_labels()

  mort_age_summary <- create_age_rate(
    fbsw_summary, !!year_sym, !!sex_sym, !!cancer_sym,
    event = "sws", collapse = TRUE
  ) |>
    add_report_labels()

  if (isTRUE(include_age_registry)) {
    inci_age_registry <- create_age_rate(
      fbsw_registry, !!year_sym, !!sex_sym, !!cancer_sym,
      event = "fbs", collapse = TRUE
    ) |>
      add_report_labels()

    mort_age_registry <- create_age_rate(
      fbsw_registry, !!year_sym, !!sex_sym, !!cancer_sym,
      event = "sws", collapse = TRUE
    ) |>
      add_report_labels()
  } else {
    inci_age_registry <- NULL
    mort_age_registry <- NULL
  }

  quality_summary <- create_quality(fbsw_summary, !!year_sym, collapse = TRUE) |>
    add_report_labels()
  quality_registry <- create_quality(fbsw_registry, !!year_sym, collapse = TRUE) |>
    add_report_labels()
  quality_cancer_summary <- create_quality(fbsw_summary, !!year_sym, !!cancer_sym, collapse = TRUE) |>
    add_report_labels()
  quality_cancer_registry <- create_quality(fbsw_registry, !!year_sym, !!cancer_sym, collapse = TRUE) |>
    add_report_labels()

  pop_summary <- get_pop(fbsw_summary, "sex", collapse = TRUE) |>
    add_report_labels()
  pop_registry <- get_pop(fbsw_registry, "sex", collapse = TRUE) |>
    add_report_labels()
  pop_age_summary <- get_pop(fbsw_summary, c("sex", "agegrp"), collapse = TRUE) |>
    add_report_labels()
  pop_age_registry <- get_pop(fbsw_registry, c("sex", "agegrp"), collapse = TRUE) |>
    add_report_labels()

  meta <- list(
    object_version = "0.1.0",
    year_data = year,
    province = province,
    province_en = province_en,
    population_total = population_total,
    summary_codes = summary_codes,
    cancer_total_code = cancer_total_code,
    cancer_codes = cancer_codes,
    created_at = Sys.time()
  )

  source <- if (isTRUE(keep_source)) {
    list(
      fbsw_summary = fbsw_summary,
      fbsw_registry = fbsw_registry
    )
  } else {
    NULL
  }

  report <- new_canreg_report(
    meta = meta,
    source = source,
    pop = list(
      summary = pop_summary,
      registry = pop_registry,
      age_summary = pop_age_summary,
      age_registry = pop_age_registry
    ),
    quality = list(
      summary = quality_summary,
      registry = quality_registry,
      cancer_summary = quality_cancer_summary,
      cancer_registry = quality_cancer_registry
    ),
    inci = list(
      summary = inci_summary,
      registry = inci_registry,
      age_summary = inci_age_summary,
      age_registry = inci_age_registry
    ),
    mort = list(
      summary = mort_summary,
      registry = mort_registry,
      age_summary = mort_age_summary,
      age_registry = mort_age_registry
    )
  )

  validate_canreg_report(report)
  report
}

#' Construct a `canreg_report` object
#'
#' @param meta Metadata list.
#' @param source Optional intermediate source objects.
#' @param pop,quality,inci,mort Report component lists.
#'
#' @return An object of class `canreg_report`.
#' @export
new_canreg_report <- function(meta,
                              source = NULL,
                              pop,
                              quality,
                              inci,
                              mort) {
  structure(
    list(
      meta = meta,
      source = source,
      pop = pop,
      quality = quality,
      inci = inci,
      mort = mort
    ),
    class = c("canreg_report", "list")
  )
}

#' Validate a `canreg_report` object
#'
#' @param x An object to validate.
#'
#' @return `x`, invisibly, if valid.
#' @export
validate_canreg_report <- function(x) {
  if (!inherits(x, "canreg_report")) {
    stop("`x` must inherit from class 'canreg_report'.", call. = FALSE)
  }

  required_top <- c("meta", "pop", "quality", "inci", "mort")
  missing_top <- setdiff(required_top, names(x))
  if (length(missing_top) > 0) {
    stop("Missing top-level components: ", paste(missing_top, collapse = ", "), call. = FALSE)
  }

  required_event <- c("summary", "registry", "age_summary")
  for (event in c("inci", "mort")) {
    missing_event <- setdiff(required_event, names(x[[event]]))
    if (length(missing_event) > 0) {
      stop("Missing components in `", event, "`: ", paste(missing_event, collapse = ", "), call. = FALSE)
    }
  }

  required_cols <- c("areacode_cn", "areacode", "year", "sex", "cancer")
  for (event in c("inci", "mort")) {
    missing_cols <- setdiff(required_cols, names(x[[event]][["summary"]]))
    if (length(missing_cols) > 0) {
      stop("Missing columns in `", event, "$summary`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  }

  invisible(x)
}

#' @export
print.canreg_report <- function(x, ...) {
  meta <- x[["meta"]]
  cat("A canreg_report object\n")
  cat("- Object version:", meta[["object_version"]], "\n")
  cat("- Data year:", meta[["year_data"]], "\n")
  if (!is.null(meta[["province"]])) cat("- Province:", meta[["province"]], "\n")
  cat("- Components: pop, quality, inci, mort\n")
  invisible(x)
}

#' @export
summary.canreg_report <- function(object, ...) {
  meta <- object[["meta"]]
  out <- list(
    object_version = meta[["object_version"]],
    year_data = meta[["year_data"]],
    province = meta[["province"]],
    summary_codes = meta[["summary_codes"]],
    n_summary_rows_inci = nrow(object[["inci"]][["summary"]]),
    n_registry_rows_inci = nrow(object[["inci"]][["registry"]]),
    n_summary_rows_mort = nrow(object[["mort"]][["summary"]]),
    n_registry_rows_mort = nrow(object[["mort"]][["registry"]])
  )
  class(out) <- c("summary.canreg_report", "list")
  out
}

#' @export
print.summary.canreg_report <- function(x, ...) {
  cat("Summary of canreg_report\n")
  cat("- Object version:", x[["object_version"]], "\n")
  cat("- Data year:", x[["year_data"]], "\n")
  if (!is.null(x[["province"]])) cat("- Province:", x[["province"]], "\n")
  cat("- Summary groups:", paste(names(x[["summary_codes"]]), x[["summary_codes"]], sep = "=", collapse = ", "), "\n")
  cat("- Incidence summary rows:", x[["n_summary_rows_inci"]], "\n")
  cat("- Incidence registry rows:", x[["n_registry_rows_inci"]], "\n")
  cat("- Mortality summary rows:", x[["n_summary_rows_mort"]], "\n")
  cat("- Mortality registry rows:", x[["n_registry_rows_mort"]], "\n")
  invisible(x)
}

standardize_report_event <- function(event) {
  event <- match.arg(event, c("inci", "incidence", "mort", "mortality"))
  switch(event,
    incidence = "inci",
    inci = "inci",
    mortality = "mort",
    mort = "mort"
  )
}

validate_summary_codes <- function(summary_codes) {
  if (is.null(names(summary_codes)) || !all(c("all", "urban", "rural") %in% names(summary_codes))) {
    stop("`summary_codes` must be a named vector containing all, urban, and rural.", call. = FALSE)
  }
  summary_codes <- summary_codes[c("all", "urban", "rural")]
  stats::setNames(as.character(summary_codes), names(summary_codes))
}