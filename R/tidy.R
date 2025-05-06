#' Tidy gender variable
#'
#' @param x Vector contains gender information.
#' @inheritParams lang
#' @inheritParams as_factor
#' @return A factor vector contains gender information.
#' @export
#'
#' @examples
#' gender <- c("male", "men", "women", "female", "women", "man", "1", "2")
#' tidy_sex(gender)
tidy_sex <- function(x, lang = "cn", as_factor = FALSE) {
  x <- tolower(x)
  m_des <- c("\u7537", "male", "man", "men")
  f_des <- c("\u5973", "female", "women", "woman")
  t_des <- c( "\u5408", "total", "\u5408\u8ba1")
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
    factor(as.integer(x), levels = valid_values, labels = labels[valid_values + 1])  
  } else {
    return(as.integer(x))
  }
}


#' Tidy age description.
#' 
#'
#' @param x Vector contains age description in Chinese.
#' @param unit Character, unit of values return, options are "year", "month",
#'        or "day", default is "year".
#'
#' @return Numeric vector contains age.
#' @export
#'
#' @examples
#' agedes <- c("50\u5c8110\u67083\u6708", "19\u5c815\u6708",
#'             "1\u5c8130\u6708", "3\u670820\u6708","30\u6708")
#' tidy_age(agedes, unit = "year")
#' tidy_age(agedes, unit = "month")
#' tidy_age(agedes, unit = "day")
tidy_age <- function(x, unit = "year") {
  x <- tolower(x)
  calc_age <- function(age_des) {
    #set initial value
    years <- 0
    months <- 0
    days <- 0
    #extract numeric value for years.
    if (grepl("\u5c81", age_des)) {
      years <- as.numeric(sub(".*?(\\d+)\\s*\u5c81.*", "\\1", age_des))
    }
    #extract numeric value for months.
    if (grepl("\u6708", age_des)) {
      months <- as.numeric(sub(".*?(\\d+)\\s*\u6708.*", "\\1", age_des))
    }
    #extract numeric value for days.
    if (grepl("\u5929", age_des)) {
      days <- as.numeric(sub(".*?(\\d+)\\s*\u5929.*", "\\1", age_des))
    }
    #convert year,month,and days to days.
    total_days <- (years * 365.25) + (months * 30.44) + days
    return(total_days)
  }
  #apply function to vector.
  days <- unlist(lapply(x, calc_age))
  days[is.na(days)] <- 0
  #convert days to another unit.
  if (unit == "year") {
    res <-trunc(days / 365.25)
  } else if (unit == "month") {
    res <- trunc(days / 30.44)
  } else if (unit == "day") {
    res <- round(days, 0)
  } else {
    print("unit not supported")
  }
  return(res)
}


#' Convert variable according to the Standard for dataset of CR in China
#'
#' @param x Character values of variable to be converted.
#' @param var_name Character value indicate name of the variable.
#' @inheritParams label_type
#' @inheritParams lang
#' @inheritParams as_factor
#'
#' @returns A formatted value.
#' @export
#'
tidy_var <- function(x,
                     var_name = "occu",
                     label_type = "full",
                     lang = "code",
                     as_factor = FALSE){
  supported_vars <- c("areacode", "province", "region",
                      "cancer",
                      "sex", "edu", "trib", "occu", "marri",
                      "grad", "beha", "basi", "treat",
                      "status", "caus", "deadplace", "lost",
                      "stats")
  var_name <- tolower(var_name)
  if (!var_name %in% supported_vars) {
    stop("The variable name is not supported.")
  }
  
  var_map <- tidy_var_maps[[var_name]]
  x_lower <- tolower(x)
  idx <- match(x_lower, var_map[["code"]])
  
  # Handle language settings
  lang_code <- std_lang(lang)
  label_col <- switch(
    lang_code,
    "en" = if (label_type == "full") "ename" else "abbr_en",
    "cn" = if (label_type == "full") "cname" else "abbr_cn",
    "code" = "code",
    "icd10" = "icd10"
  )
  
  res <- var_map[idx, label_col]
  res <- unname(unlist(res))
  
  if (as_factor){
    levels <- var_map[[label_col]][sort(na.omit(idx))]
    res <- factor(res, levels = levels, labels = levels)
  }
  
  return(res)
}

#' Standardize language input
#'
#' @inheritParams lang
#' @return Standardized language code: "en", "cn", or "code".
std_lang <- function(lang) {
  lang <- tolower(lang)
  if (lang %in% c("en", "eng", "english")) "en" else
    if (lang %in% c("cn", "zh-cn", "zh", "chinese", "zh_cn")) "cn" else
      if (lang == "code") "code" else
        if (lang == "icd10") "icd10" else
        stop("Invalid language specification")
}

#' Generate variable mapping list
#'
#' @param var_name Character value indicating the name of the variable.
#' @return A list where each code corresponds to a vector of possible values.
var_map_list <- function(var_name) {
  var_map <- tidy_var_maps[[var_name]]
  code_map <- split(var_map, var_map["code"])
  code_map <- lapply(code_map, function(b) {
    p_values <- b[, c("code","cname","ename","abbr_cn","abbr_en")]
    values <- as.character(unname(unlist(p_values)))
    if ("other_des" %in% names(b)) {
        additional_values <- unlist(strsplit(unname(unlist(b[, "other_des"])), split = ","))
        values <- c(values, additional_values)
      }
    return(values)
  })
  names(code_map) <- var_map[["code"]]
  return(code_map)
}

