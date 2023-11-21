#' Tidy gender variable
#'
#' @param x Vector contains gender information.
#' @param lang Character, specify the output language, options are 'cn',
#'        or 'en', default is 'cn'.
#'
#' @return A factor vector contains gender information.
#' @export
#'
#' @examples
#' gender <- c("male", "men", "women", "female", "women", "man", "1", "2")
#' tidy_sex(gender)
tidy_sex <- function(x, lang = "cn") {
  x <- tolower(x)
  mks <- c("\u7537", "male", "man", "men")
  fks <- c("\u5973", "female", "women", "woman")
  x[grepl(paste(fks, collapse = "|"), x)] <- 2
  x[grepl(paste(mks, collapse = "|"), x)] <- 1
  x[!(x %in% c(1, 2))] <- NA
  if (tolower(lang) %in% c("cn", "zh-cn")) {
    labels <- c("\u7537\u6027", "\u5973\u6027")
  } else if (tolower(lang) %in% c("en", "eng", "english")) {
    labels <- c("Male", "Female")
  } else {
    warning("Unsupported language. Using default labels.")
    labels <- c("Male", "Female")
  }
  factor(as.integer(x), levels = c(1, 2), labels = labels)
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

#' Query address code 
#'
#' @param x A string vector that describe the address.
#' @param api_key api_key for service provider.
#'
#' @return A data frame contains formatted address information.
#' @export
#'
#' @importFrom "utils" "URLencode"
#' @importFrom httr content GET
tidy_address <- function(x, api_key = api_key) {
  #query address for one element of vector
  query <- function(x) {
    url <- paste0("https://restapi.amap.com/v3/geocode/geo?key=",
                  api_key, "&address=", URLencode(x))
    data <- content(GET(url), "parsed")$geocodes[[1]]
    data <- data.frame(country = data$country,
                       province = data$province,
                       city = data$city,
                       distinct = data$district,
                       adcode = data$adcode,
                       location = data$location,
                       address = data$formatted_address)
    return(data)
  }
  #apply function to all elements of vector, and combine rows.
  res <- lapply(x, query) %>%
    bind_rows()
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
