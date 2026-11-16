#' Validate cancer registry data
#'
#' These functions inspect cancer case, population, and complete registry
#' objects without stopping at the first problem. They return a structured table
#' in which each row describes one issue and, where possible, identifies the
#' source row and variable.
#'
#' `validate_cases()` validates an incidence or mortality data frame.
#' `validate_population()` validates a population data frame.
#' `validate_canreg()` validates a `canreg` object or every registry in a
#' `canregs` object. `validate_fbswicd()` validates a summary-level `fbswicd`
#' object or every registry in a `fbswicds` object, including checks between
#' its case-summary and population components.
#'
#' @param x A case data frame, population data frame, or object of class
#'   `canreg`, `canregs`, `fbswicd`, or `fbswicds`.
#' @param type Type of case data passed to `validate_cases()`: `"incidence"` or
#'   `"mortality"`.
#'
#' @return A tibble of class `canreg_validation` with columns `registry`,
#'   `component`, `row`, `variable`, `severity`, `rule`, and `message`. An empty
#'   result means that no issues were detected by the implemented rules.
#'
#' @seealso [as_canreg()], [as_fbswicd()], [read_canreg()]
#' @export
#'
#' @examples
#' data("canregs")
#' validation <- validate_canreg(canregs[[1]])
#' validation
#' is_valid_canreg(canregs[[1]])
validate_cases <- function(x, type = c("incidence", "mortality")) {
  type <- match.arg(type)
  component <- if (type == "incidence") "FBcases" else "SWcases"
  required <- if (type == "incidence") {
    c("sex", "birthda", "inciden", "basi", "icd10", "morp", "topo")
  } else {
    c("sex", "birthda", "deathda", "icd10")
  }

  if (!is.data.frame(x)) {
    return(validation_issue(
      component = component,
      variable = NA_character_,
      severity = "error",
      rule = "data_frame",
      message = paste0(component, " must be a data frame.")
    ))
  }

  issues <- validation_required_columns(x, required, component)
  available <- intersect(required, names(x))
  issues <- validation_bind(
    issues,
    validation_missing_values(x, available, component)
  )

  if ("sex" %in% names(x)) {
    sex <- suppressWarnings(tidy_sex(x[["sex"]]))
    invalid <- which(!validation_missing(x[["sex"]]) & !sex %in% c("1", "2"))
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        "sex",
        "error",
        "valid_sex",
        "Case sex must resolve to 1 (male) or 2 (female)."
      )
    )
  }

  date_vars <- intersect(c("birthda", "inciden", "deathda"), names(x))
  parsed_dates <- lapply(x[date_vars], validation_parse_date)
  for (var in date_vars) {
    invalid <- which(
      !validation_missing(x[[var]]) & is.na(parsed_dates[[var]])
    )
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        var,
        "error",
        "valid_date",
        paste0("`", var, "` must contain valid dates.")
      )
    )
  }

  event_var <- if (type == "incidence") "inciden" else "deathda"
  if (all(c("birthda", event_var) %in% names(parsed_dates))) {
    invalid <- which(parsed_dates[["birthda"]] > parsed_dates[[event_var]])
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        paste0("birthda/", event_var),
        "error",
        "date_order",
        paste0("`birthda` must not be later than `", event_var, "`.")
      )
    )
  }

  if (
    type == "incidence" && all(c("inciden", "deathda") %in% names(parsed_dates))
  ) {
    invalid <- which(parsed_dates[["deathda"]] < parsed_dates[["inciden"]])
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        "inciden/deathda",
        "error",
        "date_order",
        "`deathda` must not be earlier than `inciden`."
      )
    )
  }

  if ("icd10" %in% names(x)) {
    issues <- validation_bind(
      issues,
      validation_code_format(
        x[["icd10"]],
        "^[CD][0-9]{2}(\\.[0-9A-Z]{1,2})?$",
        component,
        "icd10",
        "ICD-10 values should use a C00-D48 style code."
      )
    )
  }
  if (type == "incidence" && "morp" %in% names(x)) {
    issues <- validation_bind(
      issues,
      validation_code_format(
        x[["morp"]],
        "^[0-9]{4}$",
        component,
        "morp",
        "Morphology values should contain four digits."
      )
    )
  }
  if (type == "incidence" && "topo" %in% names(x)) {
    issues <- validation_bind(
      issues,
      validation_code_format(
        x[["topo"]],
        "^C[0-9]{2}(\\.[0-9])?$",
        component,
        "topo",
        "Topography values should use a C00.0-C80.9 style code."
      )
    )
  }

  issues
}

#' @rdname validate_cases
#' @aliases validate_population
#' @export
validate_population <- function(x) {
  component <- "POP"
  required <- c("year", "sex", "agegrp", "rks")
  if (!is.data.frame(x)) {
    return(validation_issue(
      component = component,
      variable = NA_character_,
      severity = "error",
      rule = "data_frame",
      message = "POP must be a data frame."
    ))
  }

  issues <- validation_required_columns(x, required, component)
  available <- intersect(required, names(x))
  issues <- validation_bind(
    issues,
    validation_missing_values(x, available, component)
  )

  if ("year" %in% names(x)) {
    invalid <- which(
      !validation_missing(x[["year"]]) & !is_valid_year_label(x[["year"]])
    )
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        "year",
        "error",
        "valid_year",
        paste(
          "Population year must be a four-digit calendar year",
          "or an inclusive YYYY-YYYY period."
        )
      )
    )
  }

  if ("sex" %in% names(x)) {
    sex <- suppressWarnings(tidy_sex(x[["sex"]]))
    invalid <- which(!validation_missing(x[["sex"]]) & !sex %in% c("1", "2"))
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        "sex",
        "error",
        "valid_sex",
        "Population sex must resolve to 1 (male) or 2 (female)."
      )
    )
  }

  if ("agegrp" %in% names(x)) {
    agegrp <- suppressWarnings(as.numeric(as.character(x[["agegrp"]])))
    invalid <- which(
      !validation_missing(x[["agegrp"]]) &
        (!is.finite(agegrp) | agegrp < 0)
    )
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        "agegrp",
        "error",
        "valid_agegrp",
        "Population age groups must be non-negative numeric values."
      )
    )
  }

  for (var in intersect(c("rks", "death"), names(x))) {
    value <- suppressWarnings(as.numeric(as.character(x[[var]])))
    invalid <- which(
      !validation_missing(x[[var]]) &
        (!is.finite(value) | value < 0)
    )
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        var,
        "error",
        "nonnegative",
        paste0("`", var, "` must contain non-negative numeric values.")
      )
    )
  }

  keys <- c("year", "sex", "agegrp")
  if (all(keys %in% names(x))) {
    duplicate <- duplicated(x[keys]) | duplicated(x[keys], fromLast = TRUE)
    complete <- stats::complete.cases(x[keys])
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        which(duplicate & complete),
        component,
        paste(keys, collapse = "/"),
        "error",
        "duplicate_stratum",
        "Population year, sex, and age group combinations must be unique."
      )
    )
  }

  issues
}

#' @rdname validate_cases
#' @aliases validate_canreg
#' @export
validate_canreg <- function(x) {
  if (inherits(x, "canregs")) {
    if (length(x) == 0) {
      return(validation_issue(
        component = "canregs",
        variable = NA_character_,
        severity = "error",
        rule = "nonempty",
        message = "A canregs object must contain at least one registry."
      ))
    }
    registry_names <- names(x)
    if (is.null(registry_names)) {
      registry_names <- as.character(seq_along(x))
    } else {
      missing_name <- is.na(registry_names) | registry_names == ""
      registry_names[missing_name] <- as.character(which(missing_name))
    }
    results <- lapply(seq_along(x), function(i) {
      result <- validate_canreg_single(x[[i]])
      result[["registry"]] <- rep(registry_names[[i]], nrow(result))
      new_canreg_validation(result)
    })
    return(validation_bind(results))
  }

  validate_canreg_single(x)
}

#' @rdname validate_cases
#' @aliases validate_fbswicd
#' @export
validate_fbswicd <- function(x) {
  if (inherits(x, "fbswicds")) {
    if (length(x) == 0L) {
      return(validation_issue(
        component = "fbswicds",
        variable = NA_character_,
        severity = "error",
        rule = "nonempty",
        message = "A fbswicds object must contain at least one registry."
      ))
    }
    registry_names <- names(x)
    if (is.null(registry_names)) {
      registry_names <- as.character(seq_along(x))
    } else {
      missing_name <- is.na(registry_names) | registry_names == ""
      registry_names[missing_name] <- as.character(which(missing_name))
    }
    results <- lapply(seq_along(x), function(i) {
      result <- validate_fbswicd_single(x[[i]])
      result[["registry"]] <- rep(registry_names[[i]], nrow(result))
      new_canreg_validation(result)
    })
    return(validation_bind(results))
  }

  validate_fbswicd_single(x)
}

#' Test whether a validation result contains no errors
#'
#' @param x A `canreg` or `canregs` object, or a `canreg_validation` result.
#'
#' @return A single logical value. Warnings do not make a result invalid.
#' @export
#'
#' @examples
#' data("canregs")
#' is_valid_canreg(canregs[[1]])
is_valid_canreg <- function(x) {
  if (!inherits(x, "canreg_validation")) {
    x <- validate_canreg(x)
  }
  !any(x[["severity"]] == "error")
}

#' @export
print.canreg_validation <- function(x, ..., n = 10) {
  errors <- sum(x[["severity"]] == "error")
  warnings <- sum(x[["severity"]] == "warning")
  cat("Cancer registry validation\n")
  cat(paste0("- Errors: ", errors, "\n"))
  cat(paste0("- Warnings: ", warnings, "\n"))
  if (nrow(x) == 0) {
    cat("No issues detected.\n")
  } else {
    shown <- utils::head(as.data.frame(x), n)
    print(shown, row.names = FALSE)
    if (nrow(x) > n) {
      cat("... and", nrow(x) - n, "more issue(s).\n")
    }
  }
  invisible(x)
}

#' @export
summary.canreg_validation <- function(object, ...) {
  levels <- c("error", "warning")
  counts <- table(factor(object[["severity"]], levels = levels))
  dplyr::tibble(
    severity = levels,
    n = as.integer(counts)
  )
}

validate_canreg_single <- function(x) {
  if (!inherits(x, "canreg") || !is.list(x)) {
    return(validation_issue(
      component = "canreg",
      variable = NA_character_,
      severity = "error",
      rule = "class",
      message = "`x` must be a canreg or canregs object."
    ))
  }

  issues <- validation_empty()
  required <- c("regicode", "FBcases", "SWcases", "POP")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    issues <- validation_bind(
      issues,
      validation_issue(
        component = "canreg",
        variable = missing,
        severity = "error",
        rule = "required_component",
        message = paste0("Missing required component `", missing, "`.")
      )
    )
  }

  if ("regicode" %in% names(x)) {
    regicode <- x[["regicode"]]
    if (length(regicode) != 1 || validation_missing(regicode)) {
      issues <- validation_bind(
        issues,
        validation_issue(
          component = "canreg",
          variable = "regicode",
          severity = "error",
          rule = "registry_identifier",
          message = "`regicode` must be one non-missing registry identifier."
        )
      )
    }
  }
  if ("FBcases" %in% names(x)) {
    issues <- validation_bind(
      issues,
      validate_cases(x[["FBcases"]], type = "incidence")
    )
  }
  if ("SWcases" %in% names(x)) {
    issues <- validation_bind(
      issues,
      validate_cases(x[["SWcases"]], type = "mortality")
    )
  }
  if ("POP" %in% names(x)) {
    issues <- validation_bind(issues, validate_population(x[["POP"]]))
  }

  if ("regicode" %in% names(x) && length(x[["regicode"]]) == 1) {
    issues[["registry"]] <- rep(as.character(x[["regicode"]]), nrow(issues))
    issues <- new_canreg_validation(issues)
  }
  issues
}

validate_fbswicd_single <- function(x) {
  if (!inherits(x, "fbswicd") || !is.list(x)) {
    return(validation_issue(
      component = "fbswicd",
      variable = NA_character_,
      severity = "error",
      rule = "class",
      message = "`x` must be a fbswicd or fbswicds object."
    ))
  }

  issues <- validation_empty()
  required <- c("regicode", "fbswicd", "pop")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    issues <- validation_bind(
      issues,
      validation_issue(
        component = "fbswicd",
        variable = missing,
        severity = "error",
        rule = "required_component",
        message = paste0("Missing required component `", missing, "`.")
      )
    )
  }

  regicode <- NULL
  if ("regicode" %in% names(x)) {
    regicode <- x[["regicode"]]
    if (
      length(regicode) != 1L ||
        validation_missing(regicode) ||
        !nzchar(trimws(as.character(regicode)))
    ) {
      issues <- validation_bind(
        issues,
        validation_issue(
          component = "fbswicd",
          variable = "regicode",
          severity = "error",
          rule = "registry_identifier",
          message = "`regicode` must be one non-missing registry identifier."
        )
      )
    } else {
      regicode <- as.character(regicode)
    }
  }

  if ("fbswicd" %in% names(x)) {
    issues <- validation_bind(
      issues,
      validate_fbswicd_summary(x[["fbswicd"]])
    )
  }
  if ("pop" %in% names(x)) {
    issues <- validation_bind(
      issues,
      validate_fbswicd_population(x[["pop"]])
    )
  }
  if (
    all(c("fbswicd", "pop") %in% names(x)) &&
      is.data.frame(x[["fbswicd"]]) &&
      is.data.frame(x[["pop"]])
  ) {
    issues <- validation_bind(
      issues,
      validate_fbswicd_population_coverage(x[["fbswicd"]], x[["pop"]])
    )
  }

  if ("sitemorp" %in% names(x) && !is.null(x[["sitemorp"]])) {
    issues <- validation_bind(
      issues,
      validate_fbswicd_sitemorp(x[["sitemorp"]], x[["fbswicd"]])
    )
  }

  if ("exclude" %in% names(x) && !is.null(x[["exclude"]])) {
    if (!is.data.frame(x[["exclude"]])) {
      issues <- validation_bind(
        issues,
        validation_component_data_frame(x[["exclude"]], "exclude")
      )
    }
  }

  if (!is.null(regicode)) {
    for (component in intersect(
      names(x),
      c("fbswicd", "pop", "sitemorp", "exclude")
    )) {
      issues <- validation_bind(
        issues,
        validation_component_regicode(x[[component]], component, regicode)
      )
    }
    issues[["registry"]] <- rep(regicode, nrow(issues))
    issues <- new_canreg_validation(issues)
  }
  issues
}

validate_fbswicd_summary <- function(x) {
  component <- "fbswicd"
  if (!is.data.frame(x)) {
    return(validation_component_data_frame(x, component))
  }

  required <- c("year", "sex", "cancer", "agegrp", "fbs")
  issues <- validation_required_columns(x, required, component)
  key_vars <- intersect(c("year", "sex", "cancer", "agegrp"), names(x))
  issues <- validation_bind(
    issues,
    validation_missing_values(x, key_vars, component)
  )

  if ("year" %in% names(x)) {
    invalid <- which(
      !validation_missing(x[["year"]]) & !is_valid_year_label(x[["year"]])
    )
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        "year",
        "error",
        "valid_year",
        paste(
          "Summary year must be a four-digit calendar year",
          "or an inclusive YYYY-YYYY period."
        )
      )
    )
  }

  if ("sex" %in% names(x)) {
    sex <- suppressWarnings(tidy_sex(x[["sex"]]))
    invalid <- which(
      !validation_missing(x[["sex"]]) & !sex %in% c("0", "1", "2")
    )
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        "sex",
        "error",
        "valid_sex",
        "Summary sex must resolve to 0 (overall), 1 (male), or 2 (female)."
      )
    )
  }

  issues <- validation_bind(
    issues,
    validation_fbswicd_counts(
      x,
      intersect(
        c("fbs", "sws", "mv", "dco", "mvs", "ub", "sub", "ids", "mult"),
        names(x)
      ),
      component
    )
  )

  keys <- c("year", "sex", "cancer", "agegrp")
  if (all(keys %in% names(x))) {
    duplicate <- duplicated(x[keys]) | duplicated(x[keys], fromLast = TRUE)
    complete <- stats::complete.cases(x[keys])
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        which(duplicate & complete),
        component,
        paste(keys, collapse = "/"),
        "error",
        "duplicate_stratum",
        "Summary year, sex, cancer, and age group combinations must be unique."
      )
    )
  }
  issues
}

validate_fbswicd_population <- function(x) {
  component <- "pop"
  if (!is.data.frame(x)) {
    return(validation_component_data_frame(x, component))
  }

  required <- c("year", "sex", "agegrp", "rks")
  issues <- validation_required_columns(x, required, component)
  issues <- validation_bind(
    issues,
    validation_missing_values(x, intersect(required, names(x)), component)
  )

  if ("year" %in% names(x)) {
    invalid <- which(
      !validation_missing(x[["year"]]) & !is_valid_year_label(x[["year"]])
    )
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        "year",
        "error",
        "valid_year",
        paste(
          "Population year must be a four-digit calendar year",
          "or an inclusive YYYY-YYYY period."
        )
      )
    )
  }

  if ("sex" %in% names(x)) {
    sex <- suppressWarnings(tidy_sex(x[["sex"]]))
    invalid <- which(
      !validation_missing(x[["sex"]]) & !sex %in% c("0", "1", "2")
    )
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        invalid,
        component,
        "sex",
        "error",
        "valid_sex",
        "Population sex must resolve to 0 (overall), 1 (male), or 2 (female)."
      )
    )
  }

  issues <- validation_bind(
    issues,
    validation_fbswicd_counts(
      x,
      intersect(c("rks", "death"), names(x)),
      component
    )
  )

  keys <- c("year", "sex", "agegrp")
  if (all(keys %in% names(x))) {
    duplicate <- duplicated(x[keys]) | duplicated(x[keys], fromLast = TRUE)
    complete <- stats::complete.cases(x[keys])
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        which(duplicate & complete),
        component,
        paste(keys, collapse = "/"),
        "error",
        "duplicate_stratum",
        "Population year, sex, and age group combinations must be unique."
      )
    )
  }
  issues
}

validate_fbswicd_population_coverage <- function(summary, population) {
  keys <- c("year", "sex", "agegrp")
  if (!all(keys %in% names(summary)) || !all(keys %in% names(population))) {
    return(validation_empty())
  }

  summary_complete <- stats::complete.cases(summary[keys])
  population_keys <- unique(validation_key_values(population, keys))
  summary_keys <- validation_key_values(summary, keys)
  missing <- which(
    summary_complete & !(summary_keys %in% population_keys)
  )
  validation_issue_rows(
    missing,
    "fbswicd",
    paste(keys, collapse = "/"),
    "error",
    "population_coverage",
    "Every summary year, sex, and age group stratum must have a matching population row."
  )
}

validate_fbswicd_sitemorp <- function(x, summary) {
  component <- "sitemorp"
  if (!is.data.frame(x)) {
    return(validation_component_data_frame(x, component))
  }

  required <- c("year", "sex", "cancer", "icd10", "morp")
  issues <- validation_required_columns(x, required, component)
  issues <- validation_bind(
    issues,
    validation_missing_values(
      x,
      intersect(c("year", "sex", "cancer"), names(x)),
      component
    )
  )

  for (variable in intersect(c("icd10", "morp"), names(x))) {
    valid <- if (is.list(x[[variable]])) {
      vapply(
        x[[variable]],
        function(value) {
          is.numeric(value) &&
            !is.null(names(value)) &&
            all(is.finite(value)) &&
            all(value >= 0)
        },
        logical(1)
      )
    } else {
      rep(FALSE, nrow(x))
    }
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        which(!valid),
        component,
        variable,
        "error",
        "frequency_vector",
        paste0(
          "`",
          variable,
          "` must be a list-column of named, finite, non-negative numeric vectors."
        )
      )
    )
  }

  keys <- c("year", "sex", "cancer")
  if (
    is.data.frame(summary) &&
      all(keys %in% names(x)) &&
      all(keys %in% names(summary))
  ) {
    source_keys <- unique(validation_key_values(summary, keys))
    item_keys <- validation_key_values(x, keys)
    missing <- which(
      stats::complete.cases(x[keys]) & !(item_keys %in% source_keys)
    )
    issues <- validation_bind(
      issues,
      validation_issue_rows(
        missing,
        component,
        paste(keys, collapse = "/"),
        "warning",
        "summary_coverage",
        "Site/morphology strata should have a matching summary cancer stratum."
      )
    )
  }
  issues
}

validation_component_data_frame <- function(x, component) {
  validation_issue(
    component = component,
    variable = NA_character_,
    severity = "error",
    rule = "data_frame",
    message = paste0("`", component, "` must be a data frame.")
  )
}

validation_fbswicd_counts <- function(x, variables, component) {
  results <- lapply(variables, function(variable) {
    value <- tryCatch(
      suppressWarnings(as.numeric(as.character(x[[variable]]))),
      error = function(e) rep(NA_real_, nrow(x))
    )
    invalid <- validation_missing(x[[variable]]) |
      is.na(value) |
      !is.finite(value) |
      value < 0
    validation_issue_rows(
      which(invalid),
      component,
      variable,
      "error",
      "nonnegative_count",
      paste0(
        "`",
        variable,
        "` must contain finite, non-negative numeric values."
      )
    )
  })
  validation_bind(results)
}

validation_key_values <- function(x, keys) {
  do.call(
    paste,
    c(lapply(x[keys], as.character), sep = "\r")
  )
}

validation_component_regicode <- function(x, component, regicode) {
  if (!is.data.frame(x) || !"regicode" %in% names(x)) {
    return(validation_empty())
  }
  value <- as.character(x[["regicode"]])
  invalid <- which(!validation_missing(value) & value != regicode)
  validation_issue_rows(
    invalid,
    component,
    "regicode",
    "error",
    "registry_identifier",
    paste0("`", component, "$regicode` must match the top-level `regicode`.")
  )
}

validation_empty <- function() {
  new_canreg_validation(dplyr::tibble(
    registry = character(),
    component = character(),
    row = integer(),
    variable = character(),
    severity = character(),
    rule = character(),
    message = character()
  ))
}

new_canreg_validation <- function(x) {
  structure(
    dplyr::as_tibble(x),
    class = c("canreg_validation", "tbl_df", "tbl", "data.frame")
  )
}

validation_issue <- function(
  component,
  variable,
  severity,
  rule,
  message,
  row = NA_integer_,
  registry = NA_character_
) {
  size <- max(
    length(component),
    length(variable),
    length(severity),
    length(rule),
    length(message),
    length(row),
    length(registry)
  )
  new_canreg_validation(dplyr::tibble(
    registry = rep(as.character(registry), length.out = size),
    component = rep(as.character(component), length.out = size),
    row = rep(as.integer(row), length.out = size),
    variable = rep(as.character(variable), length.out = size),
    severity = rep(as.character(severity), length.out = size),
    rule = rep(as.character(rule), length.out = size),
    message = rep(as.character(message), length.out = size)
  ))
}

validation_issue_rows <- function(
  rows,
  component,
  variable,
  severity,
  rule,
  message
) {
  if (length(rows) == 0) {
    return(validation_empty())
  }
  validation_issue(
    component = component,
    variable = variable,
    severity = severity,
    rule = rule,
    message = message,
    row = rows
  )
}

validation_bind <- function(...) {
  inputs <- list(...)
  if (
    length(inputs) == 1 &&
      is.list(inputs[[1]]) &&
      !inherits(inputs[[1]], "data.frame")
  ) {
    inputs <- inputs[[1]]
  }
  inputs <- inputs[vapply(inputs, nrow, integer(1)) > 0]
  if (length(inputs) == 0) {
    return(validation_empty())
  }
  new_canreg_validation(dplyr::bind_rows(inputs))
}

validation_required_columns <- function(x, required, component) {
  missing <- setdiff(required, names(x))
  if (length(missing) == 0) {
    return(validation_empty())
  }
  validation_issue(
    component = component,
    variable = missing,
    severity = "error",
    rule = "required_column",
    message = paste0("Missing required column `", missing, "`.")
  )
}

validation_missing_values <- function(x, variables, component) {
  results <- lapply(variables, function(var) {
    validation_issue_rows(
      which(validation_missing(x[[var]])),
      component,
      var,
      "error",
      "missing_value",
      paste0("Required variable `", var, "` must not be missing.")
    )
  })
  validation_bind(results)
}

validation_missing <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

is_valid_year_label <- function(x) {
  label <- trimws(as.character(x))
  single_year <- grepl("^[0-9]{4}$", label)
  period <- grepl("^[0-9]{4}-[0-9]{4}$", label)
  start <- suppressWarnings(as.integer(substr(label, 1L, 4L)))
  end <- suppressWarnings(as.integer(substr(label, 6L, 9L)))
  single_year | (period & start <= end)
}

validation_parse_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(as.Date(x))
  }
  suppressWarnings(as.Date(as.character(x)))
}

validation_code_format <- function(x, pattern, component, variable, message) {
  value <- toupper(trimws(as.character(x)))
  invalid <- which(!validation_missing(x) & !grepl(pattern, value))
  validation_issue_rows(
    invalid,
    component,
    variable,
    "warning",
    "code_format",
    message
  )
}
