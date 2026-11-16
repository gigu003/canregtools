#' Select elements from objects with class `"canregs"`, or `"fbswicds"`
#'
#' @description
#' This function allows you to select specific elements from objects of class
#' 'canregs', 'fbswicds', or 'asrs' based on provided indices, logical
#' conditions, or expressions. The selected elements are returned while
#' preserving the class of the input object.
#'
#' @rdname cr_select
#' @param data An object of class 'canregs', 'fbswicds', or 'asrs' from which
#'        elements will be selected.
#' @param ... Optional conditions or expressions used to filter elements within
#'        the list or data frame. Conditions are evaluated for each element of
#'        the input object. For `canregs` and `fbswicds`, population-year
#'        expressions can refer to `POP` or `pop`, respectively. The aliases
#'        `pop_years`, `pop_n_years`, `pop_year_min`, `pop_year_max`, and
#'        `pop_year_range` are also available. They mean:
#'        `pop_years` is the sorted unique non-missing population year labels;
#'        `pop_n_years` is the number of those years; `pop_year_min` and
#'        `pop_year_max` are the minimum and maximum calendar-year bounds; and
#'        `pop_year_range` is a length-two vector containing those bounds.
#'        Period labels such as `"2003-2007"` contribute 2003 and 2007 to the
#'        respective bounds. For example, use
#'        `all(pop_years %in% as.character(2018:2022))` to require all population years to
#'        fall within 2018--2022.
#' @details Each comma-separated condition must be satisfied by at least one
#'   non-missing value within an element. The conditions may be satisfied by
#'   different rows. To require conditions on the same row, combine them with
#'   `&` in a single expression. Missing condition values do not count as matches.
#' @param index A vector of indices specifying the elements to
#'      select. This can be a character vector (matching element names), a
#'      numeric vector (specifying positions), or a logical vector
#'      (indicating inclusion).
#' @return An object of the same class as the input object, containing only the
#'         selected elements that meet the specified indices or conditions.
#'
#' @export
#'
cr_select <- function(data, ..., index = names(data)) {
  UseMethod("cr_select", data)
}

#' @rdname cr_select
#' @method cr_select canregs
#' @export
#' @examples
#' data("canregs")
#' # Select elements which mi greather than 0.5 from `canregs`
#' canregs_mi <- cr_select(canregs, mi > 0.5)
#' # Select registries with population data for exactly three years
#' canregs_three_years <- cr_select(canregs, pop_n_years == 3)
#' # Select registries whose population years are all within 2018--2022
#' canregs_period <- cr_select(
#'   canregs,
#'   all(pop_years %in% as.character(2018:2022))
#' )
#' # The original population data can also be used directly
#' canregs_three_years <- cr_select(
#'   canregs,
#'   length(unique(POP$year)) == 3
#' )
#'
cr_select.canregs <- function(data, ..., index = names(data)) {
  data_index <- structure(data[index], class = class(data))

  conds <- rlang::enquos(...)
  if (length(conds) == 0) {
    return(data_index)
  }
  vars <- get_expr_vars(!!!conds)
  if (all(vars %in% tidy_var_maps[["reframe"]][["code"]])) {
    isnot <- check_reframe_expr(names(data_index), !!!conds)
    res <- data_index[isnot]
  } else if (length(conds) > 0) {
    summ <- purrr::map(data_index, summary_for_selection)
    keep <- purrr::map2_lgl(
      data_index,
      summ,
      select_population_conditions,
      pop_name = "POP",
      conds = conds
    )
    res <- data_index[keep]
  }
  structure(compact(res), class = class(data))
}

#' @rdname cr_select
#' @method cr_select asrs
#' @export
cr_select.asrs <- function(data, ..., index = names(data)) {
  data_index <- structure(data[index], class = class(data))
  conds <- rlang::enquos(...)
  if (length(conds) == 0) {
    return(data_index)
  }

  vars <- get_expr_vars(!!!conds)
  if (all(vars %in% tidy_var_maps[["reframe"]][["code"]])) {
    isnot <- check_reframe_expr(names(data_index), !!!conds)
    res <- data_index[isnot]
  } else if (length(conds) > 0) {
    res <- purrr::keep(data_index, check_conds, !!!conds)
  }
  structure(compact(res), class = class(data))
}

#' @rdname cr_select
#' @method cr_select age_rates
#' @export
cr_select.age_rates <- cr_select.asrs

#' @rdname cr_select
#' @method cr_select fbswicds
#' @export
#' @examples
#' # Select elements from obejct with class of `fbswicds`
#' fbsws <- count_canreg(canregs)
#' # Select elements which `inci` greater than 250 per 100000 population
#' fbsws_inci <- cr_select(fbsws, inci > 250)
#'
cr_select.fbswicds <- function(data, ..., index = names(data)) {
  data_index <- structure(
    data[index],
    class = class(data)
  )

  conds <- rlang::enquos(...)
  if (length(conds) == 0) {
    return(data_index)
  }
  vars <- get_expr_vars(!!!conds)
  if (all(vars %in% tidy_var_maps[["reframe"]][["code"]])) {
    isnot <- check_reframe_expr(names(data_index), !!!conds)
    res <- data_index[isnot]
  } else if (length(conds) > 0) {
    qua <- create_quality(data_index, collapse = FALSE)
    keep <- purrr::map2_lgl(
      data_index,
      qua,
      select_population_conditions,
      pop_name = "pop",
      conds = conds
    )
    res <- data_index[keep]
  }
  structure(compact(res), class = class(data))
}

#' @rdname cr_select
#' @method cr_select summaries
#' @export
#' @examples
#' # Select elements from object with class of `summaries`
#' summ <- summary(canregs, collapse = FALSE)
#' # Select elements for whcih `mi` greater than 0.5
#' summ_mi <- cr_select(summ, mi > 0.5)
#' names(summ_mi)
#'
cr_select.summaries <- function(data, ..., index = names(data)) {
  data_index <- structure(data[index], class = class(data))
  conds <- rlang::enquos(...)
  if (length(conds) == 0) {
    return(data_index)
  }
  res <- purrr::keep(data_index, check_conds, !!!conds)
  structure(
    res,
    class = class(data)
  )
}

#' Check if all conditions are met by any row in the data
#'
#' @param x A data frame-like object.
#' @param ... One or more conditions (expressions) to evaluate in `x`.
#'
#' @returns `TRUE` if every condition is satisfied by at least one row in `x`,
#'    `FALSE` otherwise.
#'
#' @noRd
check_conds <- function(x, ...) {
  conds <- rlang::enquos(...)
  all_conds <- purrr::map_lgl(conds, function(cond) {
    any(rlang::eval_tidy(cond, data = x), na.rm = TRUE)
  })
  all(all_conds)
}

#' Check if the data frame has rows after filtering
#'
#' Applies given conditions to filter a data frame and checks whether any
#' rows remain.
#'
#' @param x A data frame to check.
#' @param ... Conditions passed to dplyr::filter for subsetting the data.
#'
#' @return Logical. Returns TRUE if any rows remain after filtering;
#'    FALSE otherwise.
#' @noRd
has_rows <- function(x, ...) {
  sel_df <- dplyr::filter(x, !!!rlang::enquos(...))
  nrow(sel_df) > 0
}

#' Build data used by population-year selection expressions
#'
#' @param x A canreg or fbswicd object.
#' @param metrics A summary list or a list of quality-indicator columns.
#' @param pop_name Name of the population component in `x`.
#' @param conds A list of quosured selection conditions.
#' @return Logical scalar indicating whether `x` meets all conditions.
#' @noRd
select_population_conditions <- function(x, metrics, pop_name, conds) {
  pop <- x[[pop_name]]
  years <- if (is.data.frame(pop) && "year" %in% names(pop)) {
    sort(unique(as.character(pop[["year"]][!is.na(pop[["year"]])])))
  } else {
    character()
  }
  year_bounds <- population_year_bounds(years)
  year_range <- if (length(year_bounds) > 0L) {
    range(year_bounds)
  } else {
    c(NA_integer_, NA_integer_)
  }

  metrics <- if (is.data.frame(metrics)) as.list(metrics) else unclass(metrics)
  eval_data <- c(
    metrics,
    list(
      POP = pop,
      pop = pop,
      pop_years = years,
      pop_n_years = length(years),
      pop_year_min = year_range[[1]],
      pop_year_max = year_range[[2]],
      pop_year_range = year_range
    )
  )
  check_conds(eval_data, !!!conds)
}

population_year_bounds <- function(labels) {
  labels <- trimws(as.character(labels))
  valid_format <- grepl("^[0-9]{4}(?:-[0-9]{4})?$", labels)
  starts <- suppressWarnings(as.integer(substr(labels, 1L, 4L)))
  ends <- starts
  has_end <- nchar(labels) == 9L
  ends[has_end] <- suppressWarnings(as.integer(substr(labels[has_end], 6L, 9L)))
  valid <- valid_format & !is.na(starts) & !is.na(ends) & starts <= ends
  c(starts[valid], ends[valid])
}
