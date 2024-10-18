tidy_areacode <- function(x, lang = "cn"){
  # Check if the province code existed.
  provs <- c(11:15, 21:23, 31:37, 41:46, 50:54, 61:65, 71, 81, 82)
  prov_logi <- substr(x, 1, 2) %in% provs
  # Check if the district code is valid (6 digits)
  format_logi <- grepl("^\\d{6}$", x)
  check <- prov_logi & format_logi
  
  x[!check] <- NA
  
  # Extract the province codes.
  prov_codes <- substr(x, 1, 2)
  breakpoints <- c(10, 20, 30, 40, 50, 60, 70, 90)
  
  # Judge urban or rural according to the fifth number of the area code.
  county_codes <- ifelse(substr(x, 5, 5) %in% c("0", "1"), 1, 2)
  # Judge urban or rural according to the user defined area type dict.
  if (file.exists(".cache_dict/area_type_dict.rds")){
    area_type_dict <- readRDS(".cache_dict/area_type_dict.rds")
    type_logi <- x %in% names(area_type_dict)
    dict_urban_rural <- unlist(area_type_dict[x[type_logi]])
    urban_rural <- ifelse(tolower(dict_urban_rural) == "urban", 1, 2)
    county_codes[type_logi] <- urban_rural
  }
  
  if (lang == "cn"){
    prov_name <- factor(prov_codes, levels = provs, labels = prov_label[[1]])
    regions <- cut(as.numeric(prov_codes),
                   breaks = breakpoints,
                   labels = region_label[[1]],
                   include.lowest = TRUE)
    area_type <- factor(county_codes, levels = c(1, 2),
                        labels = c("\u57ce\u5e02", "\u519c\u6751"))
  } else {
    prov_name <- factor(prov_codes, levels = provs, labels = prov_label[[2]])
    regions <- cut(as.numeric(prov_codes),
                   breaks = breakpoints,
                   labels = region_label[[2]],
                   include.lowest = TRUE)
    area_type <- factor(county_codes, levels = c(1, 2),
                        labels = c("Urban", "Rural"))
  }
  
  
  #Extract city codes.
  city_codes <- paste0(substr(x, 1, 4), "00")
  registry <- x
  if (file.exists(".cache_dict/registry_dict.rds")){
    registry_dict <- readRDS(".cache_dict/registry_dict.rds")
    registry_logi <- registry %in% names(registry_dict)
    registry[registry_logi] <- unlist(registry_dict[registry[registry_logi]])
  }
  
  # return result list
  res <- list(areacode = x,
              registry = registry,
              province = prov_name,
              city = city_codes,
              area_type = area_type,
              region = regions)
  return(res)
}

#' Tidy cancer code
#'
#' @param x Cancer codes that output by classify_icd10.
#' @param label_type Type of labels used, options are 'full' or 'abbr'.
#' @param lang  Language used for labels, options area 'en' or 'cn'.
#' @param as_factor Logical value, output factor or not.
#'
#' @return Factors of cancer code label.
#' @export
#'
tidy_cancer <- function(x,
                        label_type = "full",
                        lang = "zh",
                        as_factor = TRUE) {
  # validate parameters
  if (label_type %nin% c("full", "abbr", "icd10")) {
    stop("Invalid label_type. Choose from 'full', 'abbr', or 'icd10'.")
  }
  
  if (lang %nin% c("en", "eng", "english", "cn", "zh-cn", "zh")) {
    stop("Invalid lang. Choose from 'en' or 'cn'.")
  }
  
  if (lang %in% c("en", "eng", "english")) {
    if (label_type == "full") {
      labs <- label_cancer[["site_en"]]
    } else if (label_type == "abbr") {
      labs <- label_cancer[["site_abbr_en"]]
    } else if (label_type == "icd10"){
      labs <- label_cancer[["icd10"]]
    }
  } else if (lang %in% c("cn", "zh-cn", "zh")){
    if (label_type == "full") {
      labs <- label_cancer[["site_cn"]]
    } else if (label_type == "abbr") {
      labs <- label_cancer[["site_abbr_cn"]]
    } else if (label_type == "icd10"){
      labs <- label_cancer[["icd10"]]
    }
  }
  cancer <- label_cancer[["cancer"]]
  pos <- na.omit(unique(match(x, cancer)))
  pos <- c(pos[pos %nin% c(60, 61)], 60, 61)
  
  res <- factor(x, levels = cancer[pos], labels = labs[pos])
  
  if (!as_factor) {
    res <- as.character(res)
    } 
  return(res)
}


#' Tidy gender variable
#'
#' @param x Vector contains gender information.
#' @param lang Character, specify the output language, options are 'cn',
#'        or 'en', default is 'cn'.
#' @param as_factor Return factor value.
#'
#' @return A factor vector contains gender information.
#' @export
#'
#' @examples
#' gender <- c("male", "men", "women", "female", "women", "man", "1", "2")
#' tidy_sex(gender)
tidy_sex <- function(x, lang = "zh", as_factor = FALSE) {
  x <- tolower(x)
  m_des <- c("\u7537", "male", "man", "men")
  f_des <- c("\u5973", "female", "women", "woman")
  t_des <- c( "\u5408", "total", "\u5408\u8ba1")
  x[grepl(paste(f_des, collapse = "|"), x)] <- 2
  x[grepl(paste(m_des, collapse = "|"), x)] <- 1
  x[grepl(paste(t_des, collapse = "|"), x)] <- 0
  x[!(x %in% c(0, 1, 2))] <- NA
  if (tolower(lang) %in% c("cn", "zh-cn", "zh")) {
    labels <- c("\u5408\u8ba1", "\u7537\u6027", "\u5973\u6027")
  } else if (tolower(lang) %in% c("en", "eng", "english")) {
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


#' Tidy occupation codes or description.
#'
#' @param x A character vector contains description of occupation
#' @param lang Language, Options are "eng" for English, and "cn" for chinese.
#'        Default is cn.
#'
#' @return A factor vector contains formatted occupation.
#' @export
#'
tidy_occu <- function(x, lang = "cn") {
  # initiate 
  code <- NA
  grepcode <- function(x){
    for (occu in occu_map$cname) {
      if (grepl(occu, x)) {
        code <- occu_map[occu_map$cname == occu, "code"]
        break
      }
      if(is.na(code)) {code <- 90}
    }
    return(code)
  }
  code <- unlist(lapply(x, grepcode))
  if (tolower(lang) %in% c("cn","zh","zh-cn")){
    code <- factor(code,
                   levels = occu_map$code,
                   labels = occu_map$cname)
  } else if (tolower(lang) %in% c("eng", "en", "english")){
    code <- factor(code,
                   levels = occu_map$code,
                   labels = occu_map$ename)
  }
  print("The convert was under GB/T 2261")
  return(code)
}
