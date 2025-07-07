#' Check the class of 'canreg'
#'
#' Provides data structure validation tools for cancer registry objects.
#' Includes the generic function `check_canreg()` and its methods for objects
#' of class `canreg` and `canregs`, which check for essential components in
#' cancer incidence and population data. Supporting functions `check_FBcases()`,
#' `check_SWcases()`, and `check_pop()` assess the presence of
#' required variables, structural completeness, and validity of data values
#' such as age groups, sex codes, and population counts.
#'
#' @rdname check_canreg
#' @param data Object with class of 'canreg' or 'canregs'.
#' @param pop_type Type of population data, either "single" or "long".
#'   Defaults to "long".
#'
#' @return A list of class "check" (or "checks" for canregs objects) containing
#'    results of data structure validation.
#' @export
#' @examples
#' data("canregs")
check_canreg <- function(data,
                         pop_type = "long"
                         ) {
  UseMethod("check_canreg", data)
}

#' @rdname check_canreg
#' @method check_canreg canregs
#' @export
#' @examples
#' check <- check_canreg(canregs)
check_canreg.canregs <- function(data,
                                 pop_type = "long"
                                 ) {
  structure(
    purrr::map(data, check_canreg.canreg, pop_type = pop_type),
    class = c("checks", "list")
  )
}

#' @rdname check_canreg
#' @method check_canreg canreg
#' @export
#' @examples
#' check <- check_canreg(canregs[[1]])
check_canreg.canreg <- function(data,
                                pop_type = "long"
                              ) {
  structure(
    list(
      FBcases = check_FBcases(pluck(data, "FBcases")),
      SWcases = check_SWcases(pluck(data, "SWcases")),
      POP = check_pop(pluck(data, "POP"), pop_type = pop_type)
    ),
    class = c("check", "list")
  )
}

#' Check SWcases data
#'
#' @param x A data frame assumed to be of class SWcases.
#' @param r_vars Required variables. Defaults to c("sex", "birthda",
#'    "deathda", "icd10", "morp", "basi").
#'
#' @return A list containing:
#' \describe{
#'   \item{null_data}{TRUE if the data has 0 rows.}
#'   \item{required_missing}{TRUE if any required variables are missing.}
#'   \item{missing_vars}{Character vector of missing required variables.}
#' }
#' @noRd
#' @examples
#' sw <- purrr::pluck(canregs[[1]], "SWcases")
#' check <- check_SWcases(sw)
check_SWcases <- function(x,
                          r_vars = c("sex", "birthda", "deathda", "icd10",
                                     "morp", "basi")
                          ) {
  if (is.null(x) || !is.data.frame(x)) {
    stop("Input must be a non-null data frame.")
  }
  
  z_rows <- nrow(x) == 0
  missing_vars <- setdiff(r_vars, colnames(x))
  m_vars <- length(missing_vars) > 0
  
  list(
    null_data = z_rows,
    required_missing = m_vars,
    missing_vars = missing_vars
  )
}

#' Check FBcases data
#'
#' @param x A data frame assumed to be of class FBcases.
#' @param r_vars Required variables. Defaults to c("sex", "birthda",
#'    "inciden", "icd10", "morp", "basi").
#'
#' @return A list containing:
#' \describe{
#'   \item{null_data}{TRUE if the data has 0 rows.}
#'   \item{required_missing}{TRUE if any required variables are missing.}
#'   \item{missing_vars}{A character vector of missing variable names.}
#' }
#' @noRd
#' @examples
#' fb <- purrr::pluck(canregs[[1]], "FBcases")
#' check <- check_FBcases(fb)
check_FBcases <- function(x,
                          r_vars = c("sex", "birthda", "inciden",
                                     "icd10", "morp", "basi")
                          ) {
  if (is.null(x) || !is.data.frame(x)) {
    stop("Input must be a non-null data frame.")
  }
  
  z_rows <- nrow(x) == 0
  missing_vars <- setdiff(r_vars, colnames(x))
  m_vars <- length(missing_vars) > 0
  
  list(
    null_data = z_rows,
    required_missing = m_vars,
    missing_vars = missing_vars
  )
}

#' Check the population data
#'
#' @param x A data frame containing population data.
#' @param pop_type Type of population data, either "single" or "long".
#'   Defaults to "single".
#' @param agegrps Number of expected age groups. Defaults to 19.
#' @param r_vars Required variable names for validation.
#'    Defaults to c("year", "sex", "agegrp", "rks", "death").
#'
#' @return A list with results of various validation checks:
#' \describe{
#'   \item{ok}{TRUE if all checks pass, FALSE otherwise.}
#'   \item{null_data}{TRUE if `x` has 0 rows.}
#'   \item{row_mismatch}{TRUE if the number of rows is not a multiple of
#'   age groups * 2.}
#'   \item{required_missing}{TRUE if any required variables are missing.}
#'   \item{missing_vars}{Names of required variables not found in the data.}
#'   \item{sex_invalid}{TRUE if `sex` column has unexpected values.}
#'   \item{agegrp_invalid}{TRUE if `agegrp` column has values not in
#'   standard groupings.}
#'   \item{rks_negative}{TRUE if `rks` column has negative values.}
#'   \item{death_negative}{TRUE if `death` column has negative values
#'   (only checked if `pop_type != "single"`).}
#' }
#' 
#' @noRd
#'
#' @examples
#' pop <- purrr::pluck(canregs[[1]], "POP")
#' check <- check_pop(pop, pop_type = "long")
check_pop <- function(x,
                      pop_type = "long",
                      agegrps = 19,
                      r_vars = c("year", "sex", "agegrp", "rks", "death")) {
  if (is.null(x) || !is.data.frame(x)) {
    stop("Input must be a non-null data frame.")
  }
  
  # Adjust required variables if population type is "single"
  if (pop_type == "single") {
    r_vars <- setdiff(r_vars, "death")
  }
  
  # Null data check
  z_rows <- nrow(x) == 0
  m_rows <- nrow(x) %% (agegrps * 2) != 0
  
  # Missing variable check
  missing_vars <- setdiff(r_vars, colnames(x))
  m_vars <- length(missing_vars) > 0
  
  # Value range checks
  sex_values <- if ("sex" %in% colnames(x)) !all(x[["sex"]] %in% c(1L, 2L)) else NA
  r_age_groups <- c(0L, 1L, seq(5L, 85L, 5L))
  age_values <- if ("agegrp" %in% colnames(x)) !all(unique(x[["agegrp"]]) %in% r_age_groups) else NA
  rks_values <- if ("rks" %in% colnames(x)) any(x[["rks"]] < 0, na.rm = TRUE) else NA
  death_values <- if (pop_type != "single" && "death" %in% colnames(x)) {
    any(x[["death"]] < 0, na.rm = TRUE)
  } else {
    FALSE
  }
  
  ok <- !(
    z_rows || m_rows || m_vars ||
      isTRUE(sex_values) || isTRUE(age_values) ||
      isTRUE(rks_values) || isTRUE(death_values)
  )
  
  list(
    ok = ok,
    null_data = z_rows,
    row_mismatch = m_rows,
    required_missing = m_vars,
    missing_vars = missing_vars,
    sex_invalid = isTRUE(sex_values),
    agegrp_invalid = isTRUE(age_values),
    rks_negative = isTRUE(rks_values),
    death_negative = isTRUE(death_values)
  )
}
