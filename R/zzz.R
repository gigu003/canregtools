#' Check the legality of group vars passed by dot dot dot
#'
#' @param x x
#' @param ... Dot dot dot parameters
#' @param quiet Run the function quietly or not.
#'
#' @returns Logical value
#' @noRd
check_group_vars <- function(x, ..., quiet = TRUE) {
  accept_vars <- regicode_vars()
  gvars <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  error_vars <- dplyr::setdiff(gvars, c(accept_vars, "year", "sex", "cancer"))
  res <- length(error_vars) > 0
  if (res && !quiet) {
    message(paste0(
      paste0(error_vars, collapse = ", "),
      " was not supported. \nAvailable options are ",
      paste0(c(accept_vars, "year", "sex", "cancer"), collapse = ", ")
    ))
  }
  return(!res)
}


#' Reframe the population data according to vars passed by dot dot dot.
#'
#' @param x Population data.
#' @param ... Dot dot dot.
#'
#' @returns Reframed population data.
#' @noRd
pre_deal_pop <- function(x, ...) {
  # Extract the population data from the input list
  pop <- purrr::pluck(x, "pop")

  # Retrieve the accepted area code variables from the regicode object
  accept_vars <- regicode_vars()

  # Capture the grouping variables passed in '...'
  # Convert them to names and remove the "cancer" variable from grouping
  gvars <- rlang::enquos(...) |>
    purrr::map_chr(rlang::as_name) |>
    dplyr::setdiff("cancer")
  if ("sex" %nin% gvars) {
    gvars <- c(gvars, "sex")
  }
  gvars1 <- gvars |> rlang::syms()

  # Determine variables that need to be summed by excluding
  # the accepted area codes and demographic variables like year, sex, agegrp
  sum_vars <- dplyr::setdiff(
    names(pop),
    c(accept_vars, "year", "sex", "agegrp")
  ) |>
    rlang::syms()

  pop <- complete_overall_sex(
    pop,
    purrr::map_chr(sum_vars, rlang::as_string),
    "population"
  )
  pop_modi <- pop |>
    group_by(!!!gvars1, !!rlang::sym("agegrp")) |>
    reframe(across(c(!!!sum_vars), \(x) sum(x, na.rm = TRUE))) |>
    ungroup()

  pop_modi
}

#' Prepare the fbswicd data before other calculation
#'
#' @param x Object with class of fbswicd
#' @param ... Dot dot dot
#' @param .arrange Whether to sort the intermediate result. Internal callers
#'   that sort their final output can disable this redundant step.
#'
#' @returns fbswicd data removed combined stats.
#' @noRd
pre_deal_fbswicd <- function(x, ..., .arrange = TRUE) {
  accept_vars <- regicode_vars()
  gvars <- rlang::enquos(...) |>
    purrr::map_chr(rlang::as_name)
  gvars <- c(gvars, "agegrp")
  if ("sex" %nin% gvars) {
    gvars_modi <- c(gvars, "sex")
  } else {
    gvars_modi <- gvars
  }
  if ("cancer" %nin% gvars) {
    gvars_modi <- c(gvars_modi, "cancer")
  } else {
    gvars_modi <- gvars_modi
  }
  fbswicd <- purrr::pluck(x, "fbswicd")
  sum_vars <- dplyr::setdiff(
    names(fbswicd),
    c(accept_vars, "year", "sex", "cancer", "agegrp")
  )
  fbswicd <- complete_summary_age_groups(x, sum_vars)
  res <- complete_overall_sex(fbswicd, sum_vars, "summary") |>
    group_by(!!!rlang::syms(gvars_modi)) |>
    reframe(across(all_of(sum_vars), \(x) sum(x, na.rm = TRUE))) |>
    select(!!!rlang::syms(gvars_modi), !!!rlang::syms(sum_vars))

  if (.arrange) {
    res <- arrange(res, !!!rlang::syms(gvars_modi))
  }

  # Deal with combine sites.
  if ("cancer" %nin% gvars) {
    res |>
      filter(!!rlang::sym("cancer") %nin% c(60, 61))
  } else {
    res
  }
}


# Resolve sex totals at the original grain before collapsing any dimensions.
complete_overall_sex <- function(data, values, component) {
  keys <- intersect(
    c(setdiff(regicode_vars(), "sex"), "year", "cancer", "agegrp"),
    names(data)
  )
  data <- standardize_year_sex(data)
  data$sex <- tidy_sex(data$sex)
  if (anyNA(data$sex)) {
    stop(
      "Sex values in ",
      component,
      " must resolve to 0, 1, or 2.",
      call. = FALSE
    )
  }
  data <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "sex")))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(values), \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
  totals <- data[data$sex == "0" & !is.na(data$sex), ]
  details <- data[data$sex %in% c("1", "2"), ]
  computed <- details |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(values), sum),
      .sex_count = dplyr::n_distinct(!!rlang::sym("sex")),
      .groups = "drop"
    )
  paired <- dplyr::inner_join(
    totals,
    computed[computed$.sex_count == 2L, ],
    by = keys,
    suffix = c(".supplied", ".computed")
  )
  for (value in values) {
    supplied <- paired[[paste0(value, ".supplied")]]
    expected <- paired[[paste0(value, ".computed")]]
    mismatch <- is.finite(supplied) &
      is.finite(expected) &
      abs(supplied - expected) > 1e-8 * pmax(1, abs(supplied), abs(expected))
    if (any(mismatch)) {
      stop(
        "Inconsistent sex = '0' totals in ",
        component,
        " column '",
        value,
        "': supplied totals must equal male plus female values when both are present.",
        call. = FALSE
      )
    }
  }
  missing_totals <- dplyr::anti_join(computed, totals, by = keys) |>
    dplyr::select(-dplyr::all_of(".sex_count")) |>
    dplyr::mutate(sex = "0")
  dplyr::bind_rows(data, missing_totals)
}


# Complete only ages within observed summary strata, using the population schema.
complete_summary_age_groups <- function(x, values) {
  data <- standardize_year_sex(x$fbswicd)
  data$sex <- tidy_sex(data$sex)
  for (value in values) {
    counts <- data[[value]]
    if (!is.numeric(counts) || any(!is.finite(counts) | counts < 0)) {
      stop(
        "Summary column '",
        value,
        "' must contain finite non-negative counts. Missing rows can be completed as zero; supplied NA values cannot.",
        call. = FALSE
      )
    }
  }
  if (!nrow(data)) {
    return(data)
  }
  population <- complete_overall_sex(
    x$pop,
    intersect(c("rks", "death"), names(x$pop)),
    "population"
  )
  keys <- intersect(
    c(regicode_vars(), "year", "sex", "cancer", "agegrp"),
    names(data)
  )
  strata <- setdiff(keys, "agegrp")
  join_keys <- setdiff(intersect(strata, names(population)), "cancer")
  population_keys <- unique(population[c(join_keys, "agegrp")])
  groups <- unique(data[strata])
  grid <- dplyr::inner_join(
    groups,
    population_keys,
    by = join_keys,
    relationship = "many-to-many"
  )
  # Include schema levels even when no cases occur in those age groups.
  age_levels <- levels(x$pop$agegrp)
  if (is.null(age_levels)) {
    age_levels <- unique(as.character(population$agegrp))
    lower <- suppressWarnings(as.numeric(sub(
      "^[^0-9]*([0-9]+).*$",
      "\\1",
      age_levels
    )))
    if (all(is.finite(lower)) && !anyDuplicated(lower)) {
      age_levels <- age_levels[order(lower)]
    }
  }
  age_levels <- unique(c(
    age_levels,
    levels(data$agegrp),
    as.character(data$agegrp)
  ))
  missing <- dplyr::anti_join(grid, data, by = keys)
  for (value in values) {
    missing[[value]] <- rep(0, nrow(missing))
  }
  result <- dplyr::bind_rows(data, missing[names(data)])
  result$agegrp <- factor(as.character(result$agegrp), levels = age_levels)
  result
}
