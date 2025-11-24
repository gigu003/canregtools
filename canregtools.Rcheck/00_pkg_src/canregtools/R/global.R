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
  lang_map <- dict_maps[["lang"]]
  res <- unname(lang_map[tolower(lang)])
  if (any(is.na(res))) {
    stop("Invalid language specification: ", paste(lang[is.na(res)], collapse = ", "))
  }
  res
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
  intersect(c("rks", "death"), names(data))
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
#' @importFrom rlang get_expr
get_expr_vars <- function(...) {
  conds <- enquos(...)
  # extract all variable names in expressions
  conds |> 
    purrr::map(~ all.vars(rlang::get_expr(.))) |> 
    purrr::flatten_chr() |> 
    unique()
}


#' Extract variable names from parameters
#'
#' @param ... Parameters (e.g., passed via `...`).
#' @return A character vector of variable names.
#' @noRd
#' @keywords internal
get_para_vars <- function(...) {
  args <- enquos(...)
  list(
    param = names(args),
    value = map_chr(args, ~ {
      expr <- get_expr(.x)
      if (is.character(expr) && length(expr) == 1) {
        expr                # 字符串直接取值
      } else {
        all.vars(expr)      # 符号提取变量名
      }
    })
  )
}

#' Check reframe variables met specified conditions
#'
#' @param data A character vector (e.g., area codes).
#' @param ... Expressions/conditions based on reframe variables.
#' @return A logical vector indicating which entries meet the conditions.
#' @noRd
#' @keywords internal
check_reframe_expr <- function(data, ...) {
  expr_list <- enquos(...)
  if (length(expr_list) == 0) {
    return(rep(TRUE, length(data)))
  }
  
  # extract all variable names in expressions
  vars <- get_expr_vars(!!!expr_list)
  
  # apply the function 'classify_areacode2' to all variable names
  env_list <- rlang::set_names(
    purrr::map(vars, ~ classify_areacode2(data, attr = .x)[[.x]]),
    vars
  )
  
  # construct evaluation environment
  eval_env <- rlang::new_data_mask(as.environment(env_list))
  # evaluate all expressions
  results <- purrr::map(expr_list, ~ rlang::eval_tidy(rlang::get_expr(.), data = eval_env))
  
  # combine logical vectors
  reduce(results, `&`)
}

#' Get cancer codes
#'
#' @param cancer_type Cancer category: "small", "big", "system", "gco", or "all".
#' @param sex A character string: "all", "male" or "female".
#' @return A character vector of cancer codes.
#' @noRd
#' @keywords internal
get_cancer <- function(cancer_type = "all", sex = "all") {
  # Define cancer types by ID range
  cancer_ids <- switch(
    cancer_type,
    "small" = as.character(1:59),
    "big" = as.character(101:126),
    "system" = as.character(201:211),
    "gco" = as.character(301:338),
    "all" = as.character(c(1:59, 101:126, 201:211, 301:338)),
    stop("Invalid cancer_type. Choose from 'small', 'big', 'system', 'gco', or 'all'.")
  )
  
  if (sex == "all") {
    return(cancer_ids)
  }
  
  # Define sex-specific cancer sites
  sex_ids <- switch(
    sex,
    "male" = as.character(c(29:37, 114:117, 206, 320:325)),
    "female" = as.character(c(38:41, 118:119, 207, 326:328)),
    stop("Invalid sex. Choose from 'all', 'male', or 'female'.")
  )
  
  # Return the intersection of cancer_ids and sex-specific IDs
  intersect(cancer_ids, sex_ids)
}

#' Remove sex-incompatible cancer records and optionally drop missing values
#'
#' @param data A data frame containing columns "sex" and "cancer".
#' @param drop_na Optional character vector of column names.
#'   If specified, rows with NA in any of these columns will be dropped.
#'
#' @return A filtered data frame with sex-incompatible cancer codes removed,
#'   and optionally missing values dropped.
#'
#' @noRd
#' @keywords internal
drop_sex_cancer <- function(data, drop_na = NULL) {
  sex <- rlang::sym("sex")
  cancer <- rlang::sym("cancer")
  res <- data |>
    filter(
      !(!!sex == 1 & !!cancer %in% get_cancer(sex = "female")) |
        !(!!sex == 2 & !!cancer %in% get_cancer(sex = "male"))
    )
  if (!is.null(drop_na)) {
    res |>
      tidyr::drop_na(any_of(drop_na))
  } else {
    return(res)
  }
}
