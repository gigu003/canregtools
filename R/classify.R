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
#' @inheritParams lang
#' @inheritParams label_type
#' @inheritParams as_factor
#'
#' @return Cancer code
#' @export
#'
#' @examples
#' icd10 <- c("C15.2", "C33.4", "C80.9", "C26.2", "C16.3")
#' classify_icd10(icd10, cancer_type = "big")
classify_icd10 <- function(x,
                           cancer_type = "big",
                           lang = "code",
                           label_type = "abbr",
                           as_factor = FALSE) {
  cancer_type <- tolower(cancer_type)
  valid_types <- c("small", "big", "system", "gco")
  if (!cancer_type %in% valid_types){
    stop("Invalid cancer_type. Valid options: ", paste(valid_types, collapse = ", "))
  }
  dict_icd10_code <- dict_maps[["icd10_code"]]
  clean_x <- toupper(substr(x, 1, 3))
  idx <- match(clean_x, dict_icd10_code[["icd10"]])
  res <- dict_icd10_code[idx, cancer_type]
  res <- unname(unlist(res))
  if (!lang == "code"){
    res <- tidy_var(res, var_name = "cancer", lang = lang,
                    label_type = label_type, as_factor = as_factor)
  }

  return(res)
}

#' Classify Area Codes
#'
#' Classifies Chinese administrative area codes into different categories,
#' including province, city, area type, and registry information.
#'
#' @param x A vector of six-digit Chinese administrative area codes as numeric
#'        or character.
#'
#' @return A list containing the following elements:
#' 
#' - `areacode`: The input area code with invalid codes replaced by NA.
#' - `registry`: The registry code corresponding to the area code.
#' - `province`: The province-level area code (first two digits + "0000").
#' - `city`: The city-level area code (first four digits + "00").
#' - `area_type`: The area type classification ("910000" for urban, "920000" for rural).
#' - `region`: The regional classification code.
#'
#' @export
#'
#' @examples
#' classify_areacode(c("110000", "320500", "440300"))
classify_areacode <- function(x) {
  x <- as.character(x)
  prov2 <- substr(x, 1, 2)
  city4 <- substr(x, 1, 4)
  digit5 <- substr(x, 5, 5)
  
  # Validation Province code
  valid_prov <- prov2 %in% sprintf("%02d", c(11:15, 21:23, 31:37, 41:46,
                                             50:54, 61:65, 71, 81, 82))
  valid_format <- nchar(x) == 6 & !is.na(x) & !grepl("[^0-9]", x)
  check <- valid_prov & valid_format
  x[!check] <- NA_character_
  
  
  # Generate province, city, and region code
  prov_codes <- ifelse(check, paste0(prov2, "0000"), NA_character_)
  city_codes <- ifelse(check, paste0(city4, "00"), NA_character_)
  # Generate region codes 
  pp <- ifelse(check, prov2, NA_character_)
  region_code <- unname(dict_maps[["prov_region"]][pp])
  region_code <- paste0(region_code, "0000")
  
  # Get address that store the dictionary in canregtools
  cache_dir <- tools::R_user_dir("canregtools", "config")
  dict_area_type_addr <- file.path(cache_dir, "area_type_dict.rds")
  dict_registry_addr <- file.path(cache_dir, "registry_dict.rds")
  
  # Generate area_type codes
  area_type_codes <- ifelse(check & digit5 %in% c("0", "1"), "910000", "920000")
  if (file.exists(dict_area_type_addr)) {
    dict_area_type <- readRDS(dict_area_type_addr)
    type_logi <- x %in% names(dict_area_type)
    urban_rural <- tolower(unlist(dict_area_type[x[type_logi]]))
    area_type_codes[type_logi] <- ifelse(urban_rural == "urban", "910000", "920000")
  }
  
  # Generate registry code
  registry_codes <- x  
  if (!file.exists(dict_registry_addr)) {
    write_registry()
  }
  registry_dict_temp <- readRDS(dict_registry_addr)
  regi_logi <- x %in% names(registry_dict_temp)
  registry_codes[regi_logi] <- unlist(registry_dict_temp[x[regi_logi]])  
  registry_codes <- unlist(registry_codes)
  
  # Return the results
  list(
    areacode = x,
    registry = registry_codes,
    province = prov_codes,
    city = city_codes,
    area_type = area_type_codes,
    region = region_code
  )
}

#' Classify pediatric tumors according to the ICCC3 standards.
#' 
#' @details
#' This function classify the 'topo' and 'morp' parts of ICDO3 codes into
#' ICCC3 (International Classification of Childhood Cancer, Third edition).
#' 
#' @param topo Topography codes of ICDO3, it could be in the format of 'C15.6'
#'        or C156'. 
#' @param morp Morphology codes of ICDO3, it could be in the format of '8000' or
#'        'M8140'.
#' @param beha Behaviour code of ICDO3.
#' @param type Type of Classification for the output, options are 'main' or
#'        'sub', default is 'main'.
#' @param version The ICCC3 standards, "v2005" or "v2017".
#'
#' @return Recode of ICCC3 classification.
#' @export
#'
classify_childhood <- function(topo,
                               morp,
                               beha,
                               type = "sub",
                               version = "v2005") {
  topo <- toupper(topo)
  morp <- gsub("[^0-9]", "", morp)
  if (version == "v2005"){
    rule <- ICCC3Rule$v2005$main
  } else if (version == "v2017"){
    rule <- ICCC3Rule$v2017
  }
  
  morp_list <- rule$morp
  topo_list <- rule$topo
  recode_list <- rule$recode
  id_groups <- function(x) {
    morp %in% morp_list[[x]] & topo %in% topo_list[[x]]
  }
  result <- lapply(1:69, id_groups)
  res_matrix <- do.call(rbind, result)
  groups <- apply(res_matrix, 2, find_pos)
  recodes <- unlist(recode_list)[groups]
  recodes <- ifelse(is.na(recodes), 999, recodes)
  sub <- recodes
  main <- floor(as.numeric(recodes) / 10)
  if (type == "sub") {
    return(sub)
  } else if (type == "main") {
    return(main)
  }
}

find_pos <- function(x) {
  which(x == TRUE, arr.ind = TRUE)[1]
}
