#' Get the dictionary file address
#'
#' @noRd
#' @keywords internal
dict_addr <- function(dict = "registry") {
  stopifnot(is.character(dict), length(dict) == 1)
  cache_dir <- tools::R_user_dir("canregtools", "config")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  file.path(cache_dir, paste0(dict, "_dict.rds"))
}

#' Not in operation
#'
#' @noRd
#' @keywords internal
"%nin%" <- function(chr_vel_ements, chr_vset) {
  !(chr_vel_ements %in% chr_vset)
}

#' Replace `NA`
#'
#' Replace `NA` with `0`
#'
#' @noRd
#' @keywords internal
replace_na <- function(x) {
  ifelse(is.na(x), 0, x)
}

#' Calculate proportion
#'
#' @noRd
#' @keywords internal
prop <- function(x, fmu, mp = 100, decimal = 2) {
  round(sum(x) / sum(fmu) * mp, decimal)
}

#' @method c canregs
#' @export
c.canregs <- function(...) {
  args <- list(...)
  if (!all(vapply(args, inherits, logical(1), "canregs"))) {
    stop("All arguments must be 'canregs' objects.")
  }
  res <- unlist(lapply(args, unclass), recursive = FALSE)
  structure(res, class = c("canregs", "list"))
}

#' Count variables
#'
#' Count variable value frequency
#'
#' @noRd
#' @keywords internal
ctp <- function(x) {
  res <- table(x)
  res_vec <- as.vector(res)
  names(res_vec) <- names(res)
  res_vec
}


#' Standardize language input
#'
#' Converts various language input formats to standardized codes
#' "en", "cn", "code", or "icd10".
#'
#' @param lang A character string indicating language preference. Accepts
#'        variants like "en", "english", "zh-cn", "icd10", etc.
#'
#' @return A standardized language code: "en", "cn", "code", or "icd10"
#' @noRd
#' @keywords internal
std_lang <- function(lang) {
  lang <- tolower(lang)
  if (lang %in% c("en", "eng", "english")) {
    "en"
  } else if (lang %in% c("cn", "zh-cn", "zh", "chinese", "zh_cn")) {
    "cn"
  } else if (lang == "code") {
    "code"
  } else if (lang == "icd10") {
    "icd10"
  } else {
    stop("Invalid language specification")
  }
}

#' Extract unique year values from a date column
#'
#' This internal helper function extracts the unique year(s) from a date column
#' in a data frame, typically used to determine the year of cancer registry
#' data.
#'
#' @param x A data frame containing a date column (e.g., case data with an
#'   `"inciden"` column).
#' @param date_var A character string specifying the name of the date column
#'   (default is `"inciden"`).
#'
#' @return An integer vector of unique years found in the specified date column,
#'   with any `NA` values removed.
#'
#' @keywords internal
#' @noRd
get_year <- function(x, date_var = "inciden") {
  date_var <- rlang::sym(date_var)
  x |>
    dplyr::pull(!!date_var) |>
    format("%Y") |>
    unique() |>
    as.integer() |>
    na.omit()
}

#' Extract summary variable names from population data
#'
#' @param x An object of class 'canreg' or similar, containing a 'POP' or
#'    'pop' data frame.
#'
#' @return A character vector of variable names excluding 'year', 'sex', and
#'    'agegrp'.
#'
#' @noRd
#' @keywords internal
get_sumvar <- function(x) {
  pop_name <- if (inherits(x, "canreg")) "POP" else "pop"
  
  if (!pop_name %in% names(x)) {
    stop(sprintf("Component '%s' not found in object.", pop_name))
  }
  
  data <- purrr::pluck(x, pop_name)
  setdiff(names(data), c("year", "sex", "agegrp"))
}

#' Extract variable names from expressions
#'
#' This internal utility extracts all variable names referenced
#' in quosured expressions, typically used in tidy evaluation.
#'
#' @param ... Quosured expressions (e.g., passed via `...`).
#' @return A character vector of variable names.
#' @noRd
#' @keywords internal
get_expr_vars <- function(...) {
  conds <- rlang::enquos(...)
  
  extract_from_expr <- function(expr) {
    if (rlang::is_quosure(expr)) {
      expr <- rlang::get_expr(expr)
    }
    if (rlang::is_symbol(expr)) {
      return(as.character(expr))
    } else if (rlang::is_call(expr)) {
      return(unlist(lapply(expr[-1], extract_from_expr)))
    } else {
      return(NULL)
    }
  }
  
  unique(unlist(lapply(conds, extract_from_expr)))
}
