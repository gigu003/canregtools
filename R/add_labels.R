#' Add variable labels for data set.
#'
#' `add_labels()` adds labels for selected variables such as `sex`, `cancer`,
#' and `areacode` in the dataset based on the specified language and label type.
#'
#' @param x A data frame or tibble to be labeled.
#' @param vars A character vector of variable names to label. Default includes
#'   `"sex"`, `"cancer"`, and `"areacode"`.
#' @template label_type
#' @template lang
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
    label_type = "full",
    lang = "zh",
    as_factor = TRUE
) {
  res <- x
  vars_in_x <- vars[vars %in% colnames(x)]
  for (var in vars_in_x) {
    var_sym <- rlang::sym(var)
    if (var == "cancer") {
      res <- dplyr::mutate(
        res,
        site = tidy_var(!!var_sym, var_name = var, label_type = label_type,
                        lang = lang, as_factor = as_factor),
        icd10 = tidy_var(!!var_sym, var_name = var, label_type = label_type,
                         lang = "icd10", as_factor = as_factor)
      )
    } else if (var == "areacode") {
      res <- dplyr::mutate(
        res,
        name = tidy_var(!!var_sym, var_name = "areacode",
                        label_type = label_type, lang = lang,
                        as_factor = as_factor)
      )
    } else {
      res <- dplyr::mutate(
        res,
        !!var_sym := tidy_var(
          !!var_sym,
          var_name = var,
          label_type = label_type,
          lang = lang,
          as_factor = as_factor)
      )
    }
  }
  key_vars <- c("areacode", "name", "year", "sex", "cancer", "site", "icd10")
  relocate_vars <- key_vars[key_vars %in% names(res)]
  dplyr::relocate(res, !!!rlang::syms(relocate_vars))
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
