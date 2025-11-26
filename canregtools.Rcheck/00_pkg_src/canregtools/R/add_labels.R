#' Add variable labels for data set.
#'
#' `add_labels()` adds labels for selected variables such as `sex`, `cancer`,
#' and `areacode` in the dataset based on the specified language and label type.
#'
#' @param x A data frame or tibble to be labeled.
#' @param vars A character vector of variable names to label. Default includes
#'   `"sex"`, `"cancer"`, and `"areacode"`.
#' @param names A character vector contains the labels added.
#' @template label_type 
#' @template lang
#' @param sep String used to separate labels when multiple languages are
#'    specified.
#' @template as_factor
#' 
#' @return A data frame or tibble with labeled variables and reordered columns.
#' @export
#'
#' @examples
#' data("canregs")
#' asr <- create_asr(canregs[[1]], year, sex, cancer)
#' asr <- add_labels(asr, label_type = "full", lang = "zh")
#' asr <- add_labels(asr, label_type = "full", lang = "en")
add_labels <- function(
    x,
    vars = c("sex", "cancer", "areacode"),
    names = NULL,
    label_type = "full",
    lang = "zh",
    sep = " ",
    as_factor = TRUE
    ) {
  vars_exists <- vars %in% colnames(x)
  vars_in_x <- vars[vars_exists]
  if (is.null(names)) {
    names <- paste0(vars_in_x, "_", paste(std_lang(lang), collapse = "_"))
  } else {
    if (length(names) != length(vars_in_x)) {
      warning("Length of `names` does not match `vars`; using default names.")
      names <- paste0(vars_in_x, "_", paste(std_lang(lang), collapse = "_"))
    }
  }
  
  label_type <- rep(label_type, length.out = length(vars_in_x))
  
  new_cols <- purrr::map(seq_along(vars_in_x), function(id) {
    tidy_var(pluck(x, vars_in_x[id]), var_name = vars_in_x[id],
             label_type = label_type[id], lang = lang, sep = sep,
             as_factor = as_factor)
    })
  names(new_cols) <- names
  res <- dplyr::bind_cols(
    x,
    as_tibble(new_cols)
  )
  
  for(name in seq_along(names)) {
    res <- dplyr::relocate(res,
                           !!rlang::sym(names[name]),
                           .after = !!rlang::sym(vars_in_x[name]))
  }
  res
}

#' Add Variable Labels with Units
#'
#' Returns formatted labels (with optional units) for commonly used
#' statistical variables in cancer registry reporting, such as incidence,
#' mortality, crude rate, age-standardized rate, and proportion.
#'
#' This function looks up the corresponding label for each input variable code
#' in the internal dictionary `tidy_var_maps[["stats"]]`, and optionally adds
#' a unit suffix (e.g., `(1/10<sup>5</sup>)` or `(%)`) depending on the variable type.
#' It supports Chinese and English, full names or abbreviations, and can format
#' the unit with or without a line break.
#'
#' @param x A character vector of variable codes to be labeled (e.g., `"cr"`, `"asr"`, `"prop"`).
#' @param label_type Label style: `"abbr"` (default) for abbreviation, `"full"` for full name.
#' @param lang Language for the labels. One of `"cn"` (default) for Chinese, `"en"` for English.
#'   Special options `"code"` or `"icd10"` return the variable code or ICD-10 code instead.
#' @param break_line Logical. If `TRUE` (default), adds a line break (`\\n`) before the unit.
#'   If `FALSE`, appends the unit directly after the label, suitable for HTML rendering
#'   (e.g., in `flextable`).
#'
#' @return A character vector of the same length as `x`, where each element is a formatted label.
#'
#' @examples
#' add_var_labels(c("cr", "asr"))
#' add_var_labels(c("cr", "prop"), label_type = "full", lang = "en")
#' add_var_labels("asr", break_line = FALSE)
#'
#' @export
add_var_labels <- function(
    x,
    label_type = "abbr",
    lang = "cn",
    break_line = TRUE
) {
  x_lower <- tolower(x)
  var_map <- tidy_var_maps[["stats"]]
  
  lang_code <- std_lang(lang)
  label_col <- switch(
    lang_code,
    "en" = if (label_type == "full") "ename" else "abbr_en",
    "cn" = if (label_type == "full") "cname" else "abbr_cn",
    "code" = "code",
    "icd10" = "icd10"
  )
  
  idx <- match(x_lower, var_map[["code"]])
  label <- var_map[idx, label_col]
  label <- unname(unlist(label))
  unit <- var_map[idx, "unit"]
  unit <- unname(unlist(unit))
  
  unit_label <- vapply(unit, function(u) {
    switch(
      as.character(u),
      "1" = "",
      "2" = if (break_line) "\n(1/10<sup>5</sup>)" else "(1/10<sup>5</sup>)",
      "3" = if (break_line) "\n(%)" else "(%)",
      ""
    )
  }, character(1))
  
  paste0(label, unit_label)
}
