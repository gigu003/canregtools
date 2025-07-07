#' Tidy gender variable
#'
#' Standardizes gender-related values into consistent numeric codes or factors.
#' This function maps various gender-related character strings (e.g., "male",
#' "female", "man", "woman", "1", "2", etc.) to standardized numeric values:
#' `1` for male, `2` for female, and `0` for total. It supports both Chinese
#' and English labels. Optionally, the result can be returned as a factor with
#' appropriate labels.
#' 
#' @param x A character or numeric vector containing gender information.
#' @template lang
#' @template as_factor
#' @return A numeric vector or a factor representing gender:
#' \describe{
#'   \item{0}{Total}
#'   \item{1}{Male}
#'   \item{2}{Female}
#' }
#' If `as_factor = TRUE`, a factor is returned with labels in the specified
#' language (`lang`).
#'
#' @export
#'
#' @examples
#' gender <- c("male", "men", "women", "female", "women", "man", "1", "2")
#' tidy_sex(gender)
tidy_sex <- function(x, lang = "cn", as_factor = FALSE) {
  x <- tolower(x)
  m_des <- c("\u7537", "male", "man", "men")
  f_des <- c("\u5973", "female", "women", "woman")
  t_des <- c("\u5408", "total", "\u5408\u8ba1")
  x[grepl(paste(f_des, collapse = "|"), x)] <- 2
  x[grepl(paste(m_des, collapse = "|"), x)] <- 1
  x[grepl(paste(t_des, collapse = "|"), x)] <- 0
  x[!(x %in% c(0, 1, 2))] <- NA
  if (std_lang(lang) == "cn") {
    labels <- c("\u5408\u8ba1", "\u7537\u6027", "\u5973\u6027")
  } else if (std_lang(lang) == "en") {
    labels <- c("Total", "Male", "Female")
  } else {
    warning("Unsupported language. Using default labels.")
    labels <- c("Total", "Male", "Female")
  }
  valid_values <- sort(unique(na.omit(as.integer(x))))
  if (as_factor) {
    factor(as.integer(x), levels = valid_values,
           labels = labels[valid_values + 1])
  } else {
    return(as.integer(x))
  }
}


#' Tidy age description
#'
#' Parses age descriptions written in Chinese and converts them into numeric
#' values expressed in years, months, or days. It interprets age strings
#' containing Chinese characters such as (years), (months), and (days), and
#' converts them to a numeric vector representing age in the specified unit.
#'
#' @param x A character vector containing age descriptions in Chinese.
#' @param unit A character string specifying the unit of the returned values.
#'   Options are `"year"` (default), `"month"`, or `"day"`.
#'
#' @return A numeric vector representing ages in the specified unit:
#' * year: Truncated age in years.
#' * month: Truncated age in months.
#' * day: Rounded age in days.
#'
#' @export
#'
#' @examples
#' agedes <- c(
#'   "50\u5c8110\u67083\u6708", "19\u5c815\u6708",
#'   "1\u5c8130\u6708", "3\u670820\u6708", "30\u6708"
#' )
#' tidy_age(agedes, unit = "year")
#' tidy_age(agedes, unit = "month")
#' tidy_age(agedes, unit = "day")
tidy_age <- function(x, unit = "year") {
  x <- tolower(x)
  calc_age <- function(age_des) {
    # set initial value
    years <- 0
    months <- 0
    days <- 0
    # extract numeric value for years.
    if (grepl("\u5c81", age_des)) {
      years <- as.numeric(sub(".*?(\\d+)\\s*\u5c81.*", "\\1", age_des))
    }
    # extract numeric value for months.
    if (grepl("\u6708", age_des)) {
      months <- as.numeric(sub(".*?(\\d+)\\s*\u6708.*", "\\1", age_des))
    }
    # extract numeric value for days.
    if (grepl("\u5929", age_des)) {
      days <- as.numeric(sub(".*?(\\d+)\\s*\u5929.*", "\\1", age_des))
    }
    # convert year,month,and days to days.
    tdays <- (years * 365.25) + (months * 30.44) + days
    tdays
  }
  # apply function to vector.
  days <- unlist(lapply(x, calc_age))
  days[is.na(days)] <- 0
  # convert days to another unit.
  if (unit == "year") {
    trunc(days / 365.25)
  } else if (unit == "month") {
    trunc(days / 30.44)
  } else if (unit == "day") {
    round(days, 0)
  } else {
    print("unit not supported")
  }
}


#' Reformat variable values for Cancer Registration in China
#'
#' Standardizes and labels values of a specified variable according to the
#' national cancer registration standard of China: T/CHIA 18-2021.
#'
#' `tidy_var()` converts raw character inputs into standardized labels, codes,
#' or abbreviations based on reference mappings defined for each variable (e.g.,
#' occupation, basis of diagnosis, etc.). It supports both Chinese and English
#' outputs and can return values as factors with labeled levels.
#'
#' @param x A character vector containing raw values of a variable used in
#'   cancer registry data.
#' @param var_name A character string indicating the name of the variable to
#'   reformat (e.g., `"occu"` for occupation). Must be one of the variable
#'   names defined in `tidy_var_maps`.
#' @template label_type
#' @template lang
#' @template as_factor
#'
#' @returns A character or factor vector of reformatted values. The output
#'    depends on the settings for `label_type`, `lang`, and `as_factor`:
#' * If `as_factor = FALSE`, returns a character vector.
#' * If `as_factor = TRUE`, returns a factor with sorted unique levels.
#' * The labels used depend on `lang` (`"cn"`, `"en"`, `"code"`, or `"icd10"`)
#'   and `label_type` (`"full"` or `"abbr"`).
#'
#' @export
#'
#' @examples
#' occu <- c("11", "13", "17", "21", "24", "27", "31", "37", "51", "80", "90")
#' tidy_var(occu, var_name = "occu", lang = "cn")
#' tidy_var(occu, var_name = "occu", lang = "en")
#' tidy_var(occu, var_name = "occu", lang = "cn", label_type = "abbr")
#' tidy_var(occu, var_name = "occu", lang = "en", label_type = "abbr")
#'
tidy_var <- function(
    x,
    var_name = "occu",
    label_type = "full",
    lang = "code",
    as_factor = FALSE
    ) {
  # Get the supported variable names from tidy_var_maps dictionary.
  supported_vars <- names(tidy_var_maps)
  var_name <- tolower(var_name)
  if (!var_name %in% supported_vars) {
    stop("The variable name is not supported.")
  }

  var_map <- tidy_var_maps[[var_name]]
  x_lower <- tolower(x)
  idx <- match(x_lower, var_map[["code"]])

  # Handle language settings
  lang_code <- std_lang(lang)
  label_col <- switch(lang_code,
    "en" = if (label_type == "full") "ename" else "abbr_en",
    "cn" = if (label_type == "full") "cname" else "abbr_cn",
    "code" = "code",
    "icd10" = "icd10"
  )

  res <- var_map[idx, label_col]
  res <- unname(unlist(res))

  if (as_factor) {
    levels <- var_map[[label_col]][sort(na.omit(idx))]
    res <- factor(res, levels = levels, labels = levels)
  }

  return(res)
}
