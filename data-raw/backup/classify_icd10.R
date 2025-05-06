#' Classify ICD10 Codes to Cancer Categories.
#' 
#' @description
#' Classify ICD10 codes into Cancer Categories according to the specified
#' category type and language.
#' 
#' 
#' @param x The ICD10 codes of cancer part (C00-C98 and D00-D48) collected
#'        by the Population-Based Cancer Registration (PBCR).
#' @inheritParams cancer_type
#' @param as_factor logical indicate that the output value as factor.
#'
#' @return Cancer code
#' @export
#'
#' @examples
#' icd10 <- c("C15.2", "C33.4", "C80.9", "C26.2", "C16.3")
#' classify_icd10(icd10, cancer_type = "big")
classify_icd10 <- function(x, cancer_type = "big", as_factor = FALSE) {
  cancer_type <- toupper(cancer_type)
  x <- toupper(substr(x, 1, 3))
  if (cancer_type %in% c("BIG", "SMALL")) {
    res <- case_match(
      x,
      c("C00") ~ 1,
      c("C01", "C02") ~ 2,
      c(paste0("C0", seq(3, 6))) ~ 3,
      c("C07", "C08") ~ 4,
      c("C09") ~ 5,
      c("C10") ~ 6,
      c("C11") ~ 7,
      c("C12", "C13") ~ 8,
      c("C14") ~ 9,
      c("C15") ~ 10,
      c("C16") ~ 11,
      c("C17") ~ 12,
      c("C18") ~ 13,
      c("C19", "C20") ~ 14,
      c("C21") ~ 15,
      c("C22") ~ 16,
      c("C23", "C24") ~ 17,
      c("C25") ~ 18,
      c("C30", "C31") ~ 19,
      c("C32") ~ 20,
      c("C33", "C34") ~ 21,
      c("C37", "C38") ~ 22,
      c("C40", "C41") ~ 23,
      c("C43") ~ 24,
      c("C44") ~ 25,
      c("C45") ~ 26,
      c("C46") ~ 27,
      c("C47", "C49") ~ 28,
      c("C50") ~ 29,
      c("C51") ~ 30,
      c("C52") ~ 31,
      c("C53") ~ 32,
      c("C54") ~ 33,
      c("C55") ~ 34,
      c("C56") ~ 35,
      c("C57") ~ 36,
      c("C58") ~ 37,
      c("C60") ~ 38,
      c("C61") ~ 39,
      c("C62") ~ 40,
      c("C63") ~ 41,
      c("C64") ~ 42,
      c("C65") ~ 43,
      c("C66") ~ 44,
      c("C67") ~ 45,
      c("C68") ~ 46,
      c("C69") ~ 47,
      c(paste0("C", seq(70, 72)), "D32", "D33", "D42", "D43") ~ 48,
      c("C73") ~ 49,
      c("C74") ~ 50,
      c("C75") ~ 51,
      c("C81") ~ 52,
      c(paste0("C", c(seq(82, 85), 96))) ~ 53,
      c("C88") ~ 54,
      c("C90") ~ 55,
      c("C91") ~ 56,
      c(paste0("C", seq(92, 94)), paste0("D", seq(45, 47))) ~ 57,
      c("C95") ~ 58,
      c(paste0(
        "C", c(seq(26, 29), 35, 36, 39, 48, 59, seq(76, 80), 86, 87, 89)
      )) ~ 59,
      NA ~ NA
    )
    res <- as.integer(res)
    if (cancer_type == "BIG") {
      res <- case_match(
        res,
        c(1:6, 8, 9) ~ 101,
        7 ~ 102,
        10 ~ 103,
        11 ~ 104,
        c(13:15) ~ 105,
        16 ~ 106,
        17 ~ 107,
        18 ~ 108, 
        20 ~ 109,
        21 ~ 110,
        22 ~ 111,
        23 ~ 112,
        24 ~ 113,
        29 ~ 114,
        32 ~ 115,
        c(33, 34) ~ 116,
        35 ~ 117,
        39 ~ 118,
        40 ~ 119,
        c(42:44, 46) ~ 120,
        45 ~ 121,
        48 ~ 122,
        49 ~ 123,
        c(52:55) ~ 124,
        c(56:58) ~ 125,
        c(12, 19, 25:28, 30, 31, 36:38, 41, 47, 50, 51, 59) ~ 126,
        .default = NA
      )
      res <- as.integer(res)
      if (as_factor){
        res <- factor(res, levels = c(101:126), labels = c(101:126))
      }
    }
  } else if (cancer_type == "SYSTEM"){
    res <- case_match(
      x,
      c(paste0("C0", 1:9), paste0("C", 10:14)) ~ 1,
      c(paste0("C", 15:26)) ~ 2,
      c(paste0("C", 30:39)) ~ 3,
      c(paste0("C", 40:44)) ~ 4,
      c("C50") ~ 5,
      c(paste0("C", 51:58)) ~ 6,
      c(paste0("C", 60:63)) ~ 7,
      c(paste0("C", 64:68)) ~ 8,
      c(paste0("C", 69:75)) ~ 9,
      c(paste0("C", 81:96)) ~ 10,
      c(paste0("C", c(27:29, 45:49, 59, 76:80, 97))) ~ 11,
      .default = NA
    )
    res <- res + 200
    res <- as.integer(res)
    if (as_factor){
      res <- factor(res, levels = c(201:211), labels = c(201:211))
    }
  } else if (cancer_type == "GCO"){
    res <- case_match(
      x,
      c(paste0("C0", 1:6)) ~ 301,
      c(paste0("C0", 7:8)) ~ 302,
      c(paste0("C", c("09","10"))) ~ 303,
      c("C11") ~ 304,
      c(paste0("C", 12:13)) ~ 305,
      c("C15") ~ 306,
      c("C16") ~ 307,
      c("C18") ~ 308,
      c(paste0("C", 19:20)) ~ 309,
      c("C21") ~ 310,
      c("C22") ~ 311,
      c("C23") ~ 312,
      c("C25") ~ 313,
      c("C32") ~ 314,
      c(paste0("C", 33:34)) ~ 315,
      c("C43") ~ 316,
      c("C44") ~ 317,
      c("C45") ~ 318,
      c("C46") ~ 319,
      c("C50") ~ 320,
      c("C51") ~ 321,
      c("C52") ~ 322,
      c("C53") ~ 323,
      c("C54") ~ 324,
      c("C56") ~ 325,
      c("C60") ~ 326,
      c("C61") ~ 327,
      c("C62") ~ 328,
      c("C64") ~ 329,
      c("C67") ~ 330,
      c(paste0("C", 70:72)) ~ 331,
      c("C73") ~ 332,
      c("C81") ~ 333,
      c(paste0("C", c(82:86, 88))) ~ 334,
      c("C90") ~ 335,
      c(paste0("C", 91:95)) ~ 336,
      c(paste0("C", c(76:80, 96:97))) ~ 337,
      NA ~ NA,
      .default = 338
    )
    res <- as.integer(res)
    if (as_factor){
      res <- factor(res, levels = c(301:338), labels = c(301:338))
    }
  }
  
  if (as_factor&cancer_type=="SMALL"){
    res <- factor(res, levels = c(1:59), labels = c(1:59))
  }
  
  return(res)
}


#' Get attributes affiliated with the areacode
#'
#' @param x Character vector of six digits areacode.
#' @param lang Character string indicate the language.
#' @param as_factor Logical value, indicate if output as factor or not.
#'
#' @return List of atttributes of areacode.
#' @export
#'
tidy_areacode <- function(x,
                          labe_type = "abbr",
                          lang = "cn",
                          as_factor = FALSE){
  x <- as.character(x)
  if (!is.character(x)) {
    stop("Input 'x' must be a character vector.")
  }
  
  # Define valid province codes (first two digits of the area code)
  provs <- sprintf("%02d", c(11:15, 21:23, 31:37, 41:46, 50:54, 61:65, 71, 81, 82))
  prov_logi <- substr(x, 1, 2) %in% provs  # Check if the first two digits match valid province codes
  
  # Validate area code format (must be exactly 6 digits)
  format_logi <- grepl("^\\d{6}$", x)
  check <- prov_logi & format_logi  # Combined validation check
  x[!check] <- NA  # Set invalid area codes to NA
  
  # Extract province codes (first two digits + "0000")
  prov_codes <- paste0(substr(x, 1, 2), "0000")
  
  # Determine urban or rural classification based on the fifth digit of the area code
  county_codes <- ifelse(substr(x, 5, 5) %in% c("0", "1"), "910000", "920000")
  
  # Use cached dictionary to determine urban/rural classification if available
  cache_dir <- tools::R_user_dir("canregtools", "config")
  area_type_dict_file <- file.path(cache_dir, "area_type_dict.rds")
  if (file.exists(area_type_dict_file)) {
    area_type_dict <- readRDS(area_type_dict_file)
    type_logi <- x %in% names(area_type_dict)
    dict_urban_rural <- tolower(unlist(area_type_dict[x[type_logi]]))
    county_codes[type_logi] <- ifelse(dict_urban_rural == "urban", "910000", "920000")
  }
  
  # Extract city codes (first four digits + "00")
  city_codes <- paste0(substr(x, 1, 4), "00")
  
  
  # Handle registry information using a cached dictionary if available
  registry_dict_file <- file.path(cache_dir, "registry_dict.rds")
  registry <- x  # Initialize registry as the input area code
  if (!file.exists(registry_dict_file)) {
    write_registry()
  }
  registry_dict_temp <- readRDS(registry_dict_file)
  regi_logi <- x %in% names(registry_dict_temp)
  registry[regi_logi] <- unlist(registry_dict_temp[x[regi_logi]])  
  registry <- unlist(registry)
  
  
  pp <- as.integer(substr(prov_codes, 1, 2))
  region_code <- case_match(pp, 11:15 ~ 1, 21:23 ~ 2, c(31:34, 37) ~ 3,
                            c(41:43, 36) ~ 4, c(44:46, 35) ~ 5,
                            50:54 ~ 6, 61:65 ~ 7, c(71, 81, 82) ~ 8) + 71
  region_code <- as.character(region_code * 10000)
  
  # Convert to factors if requested
  if (as_factor) {
    if (tolower(lang) %in% c("cn", "zh-cn", "zh", "chinese")) {
      # Convert province codes and area types to factors (Chinese labels)
      prov_name <- factor(prov_codes, levels = paste0(provs, "0000"),
                          labels = prov_label[[1]])
      area_type <- factor(county_codes, levels = c("910000", "920000"), 
                          labels = c("\u57ce\u5e02", "\u519c\u6751"))
      region <- factor(region_code, levels = as.character(72:79*10000),
                       labels = region_label[[1]])
    } else if (tolower(lang) %in% c("en", "eng", "english")) {
      # Convert province codes and area types to factors (English labels)
      prov_name <- factor(prov_codes, levels = paste0(provs, "0000"),
                          labels = prov_label[[2]])
      area_type <- factor(county_codes, levels = c("910000", "920000"),
                          labels = c("Urban", "Rural"))
      region <- factor(region_code, levels = as.character(72:79*10000),
                       labels = region_label[[2]])
    }
  } else {
    # If no factor conversion is needed, use raw values
    prov_name <- prov_codes
    area_type <- county_codes
    region <- region_code
  }
  
  # return result list
  res <- list(areacode = x,
              registry = registry,
              province = prov_name,
              city = city_codes,
              area_type = area_type,
              region =region)
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
  
  
  if (std_lang(lang) == "en") {
    if (label_type == "full") {
      labs <- label_cancer[["site_en"]]
    } else if (label_type == "abbr") {
      labs <- label_cancer[["site_abbr_en"]]
    } else if (label_type == "icd10"){
      labs <- label_cancer[["icd10"]]
    }
  } else if (std_lang(lang) == "cn"){
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