#' Classify ICD10 codes to cancer categories
#'
#' Classify ICD10 codes into Cancer Categories according to the specified
#' category type and language.
#'
#' @param x The ICD10 codes of cancer part ('C00-C98 and D00-D48') which used in
#'   by the Population-Based Cancer Registration ('PBCR').
#' @template cancer_type
#' @template lang
#' @template label_type
#' @template as_factor
#' @return Cancer code.
#' @export
#'
#' @examples
#' icd10 <- c("C15.2", "C33.4", "C80.9", "C26.2", "C16.3")
#' classify_icd10(icd10, cancer_type = "big")
#' classify_icd10(icd10, cancer_type = "small")
#' classify_icd10(icd10, cancer_type = "system")
#' classify_icd10(icd10, cancer_type = "gco")
#' classify_icd10(icd10, cancer_type = "ci5")
classify_icd10 <- function(
  x,
  cancer_type = "big",
  lang = "code",
  label_type = "abbr",
  as_factor = FALSE
) {
  cancer_type <- tolower(cancer_type)
  valid_types <- c("small", "big", "system", "gco", "ci5")
  if (!cancer_type %in% valid_types) {
    stop(
      "Invalid cancer_type. Valid options: ",
      paste(valid_types, collapse = ", ")
    )
  }
  dict_icd10_code <- dict_maps[["icd10_code"]]
  clean_x <- toupper(substr(x, 1, 3))
  idx <- match(clean_x, dict_icd10_code[["icd10"]])
  res <- unname(dict_icd10_code[[cancer_type]][idx])
  res <- tidy_var(
    res,
    var_name = "cancer",
    lang = lang,
    label_type = label_type,
    as_factor = as_factor
  )

  return(res)
}

#' Classify codes for the administrative divisions of China
#'
#' Categorizes six-digit administrative division codes of the People's Republic
#' of China (as per GB/T 2260-2007) into several structured components,
#' including province, city, area type (urban/rural), registry code, and
#' regional classification.
#'
#' This function standardizes and validates area codes, identifies their
#' administrative levels, and attaches metadata used in cancer registration
#' systems. It also supports external dictionaries (from the `canregtools`
#' configuration folder) to provide more accurate classification of area types
#' and registry mapping. Classify Codes for the administrative divisions of
#' the People's Republic of China(GB/T 2260-2007) into different categories,
#' including 'province', city', 'area_type', and 'registry' attributes.
#'
#' @param x A vector of six-digit Chinese administrative area codes, either as numeric
#'   or character strings.
#' @param attr A character vector of attributes to return. Defaults to all
#'   built-in area-code attributes. Options include:
#'   - `"regicode"`: Original area codes.
#'   - `"registry"`: Cancer registry codes.
#'   - `"province"`: Province-level codes.
#'   - `"city"`: City-level codes.
#'   - `"area_type"`: Urban/rural classification.
#'   - `"region"`: Regional classification based on province.
#'   - Any other string will be treated as a custom dictionary name written
#'     using [cr_map()].
#'
#' @return A list with the following named elements:
#'
#' * `regicode`: Validated area codes. Invalid entries are replaced with `NA`.
#' * `registry`: Registry codes corresponding to each area, using a built-in
#'    or cached dictionary.
#' * `province`: Province-level codes formed by taking the first two digits and
#'    appending `"0000"`.
#' * `city`: City-level codes formed by taking the first four digits and
#'    appending `"00"`.
#' * `area_type`: Urban-rural classification codes: `"910000"` for urban,
#'    `"920000"` for rural. This can be updated using [cr_map()]
#'    with `attr = "area_type"`.
#' * `region`: Region classification codes derived from province codes,
#'    ending in `"0000"`.
#'
#' @export
#'
#' @seealso [cr_map()], [write_registry()]
#' @examples
#' classify_areacode(c("110000", "320500", "440300"))
#' classify_areacode(c("110000", "320500", "440300"), attr = "registry")
#'
classify_areacode <- function(x, attr = regicode_vars()) {
  if (
    !is.character(attr) ||
      length(attr) == 0 ||
      any(is.na(attr)) ||
      any(!nzchar(attr))
  ) {
    stop(
      "'attr' must be a non-empty character vector without NA or empty values."
    )
  }

  x <- as.character(x)
  raw_x <- x
  prov2 <- substr(x, 1, 2)
  # Validation Province code
  valid_prov <- prov2 %in%
    sprintf(
      "%02d",
      c(
        11:15,
        21:23,
        31:37,
        41:46,
        50:54,
        61:65,
        71,
        81,
        82
      )
    )
  valid_format <- nchar(x) == 6 & !is.na(x) & !grepl("[^0-9]", x)
  check <- valid_prov & valid_format
  x[!check] <- NA_character_

  get_codes <- function(f) {
    if (f == "regicode") {
      return(ifelse(check, raw_x, NA_character_))
    } else if (f == "region") {
      pp <- ifelse(check, prov2, NA_character_)
      region_code <- unname(dict_maps[["prov_region"]][pp])
      return(ifelse(
        is.na(region_code),
        NA_character_,
        paste0(region_code, "0000")
      ))
    } else if (f == "province") {
      return(ifelse(check, paste0(prov2, "0000"), NA_character_))
    } else if (f == "city") {
      city4 <- substr(x, 1, 4)
      return(ifelse(check, paste0(city4, "00"), NA_character_))
    } else if (f == "registry") {
      registry_codes <- x
      registry_dict_temp <- read_registry_dictionary("registry")
      regi_logi <- x %in% names(registry_dict_temp)
      registry_codes[regi_logi] <- unlist(registry_dict_temp[x[regi_logi]])
      return(unlist(registry_codes))
    } else if (f == "area_type") {
      digit5 <- substr(x, 5, 5)
      area_codes <- ifelse(
        check,
        ifelse(digit5 %in% c("0", "1"), "910000", "920000"),
        NA_character_
      )
      dict <- read_registry_dictionary(f)
      type_logi <- x %in% names(dict)
      area_codes[type_logi] <- tolower(unname(dict[x[type_logi]]))
      return(area_codes)
    } else {
      custom_codes <- x
      dict_file <- dict_addr(f)
      if (!file.exists(dict_file)) {
        stop(paste0(
          "Custom dictionary not found. ",
          "Use `cr_map(..., attr = '",
          f,
          "')` to create one."
        ))
      }
      dict <- read_registry_dictionary(f)
      custom_logi <- x %in% names(dict)
      codes_in_dict <- unlist(dict[x[custom_logi]])
      custom_codes[custom_logi] <- codes_in_dict
      return(custom_codes)
    }
  }

  res <- purrr::map(attr, get_codes)
  names(res) <- attr
  return(res)
}

regicode_vars <- function() {
  c("regicode", "registry", "province", "city", "area_type", "region")
}


#' Classify childhood cancer according to the ICCC3 standards
#'
#' `classify_childhood()`classifies childhood cancer based on ICD-O-3 codes,
#' which include topography, morphology, and behavior codes, using the
#' International Classification of Childhood Cancer, Third Edition (ICCC3).
#'
#' @param topo A character vector of ICD-O-3 topography codes
#'    (e.g., `"C15.6"` or `"C156"`).
#' @param morp A character vector of ICD-O-3 morphology codes
#'    (e.g., `"8000"` or `"M8140"`).
#' @param beha A numeric or character vector representing ICD-O-3 behavior
#'    codes.
#' @param type A string specifying the type of classification to return:
#'    `"main"` for main groups or `"sub"` for subgroups. Defaults to `"main"`.
#' @param version A string specifying the version of the ICCC-3 rules to use:
#'    either `"v2005"` or `"v2017"`. Defaults to `"v2005"`.
#'
#' @references
#' Steliarova-Foucher, E., Stiller, C., Lacour, B. and Kaatsch, P. (2005),
#' International Classification of Childhood Cancer, third edition†‡. Cancer,
#' 103: 1457-1467. \doi{10.1002/cncr.20910}
#'
#' @return A numeric vector of ICCC-3 classification codes. If `type = "sub"`,
#'    returns subgroup codes; if `type = "main"`, returns main group codes.
#' @export
#' @examples
#' topo <- c("C15.2", "C16.2", "C34.2")
#' morp <- c("8000", "8040", "8170")
#' beha <- c("3", "3", "3")
#' child_code <- classify_childhood(topo, morp, beha, type = "main")
#'
classify_childhood <- function(
  topo,
  morp,
  beha,
  type = "sub",
  version = "v2005"
) {
  topo <- toupper(topo)
  morp <- gsub("[^0-9]", "", morp)
  if (version == "v2005") {
    rule <- ICCC3Rule$v2005$main
  } else if (version == "v2017") {
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

#' Classify ICD-O-3 morphology codes into categories
#'
#' @param x Character vector of ICD-O-3 morphology codes.
#'   Values not present in the internal dictionary will be returned as `"99"`.
#' @param group Morphology grouping system. `"60"` uses the broad
#'   morphology grouping dictionary; `"125"` uses leukemia-specific
#'   morphology groups.
#'
#' @returns Character vector of categories corresponding to `x`.
#' @export
#'
#' @examples
#' morps <- c("8140","8070","8050","8051","9900","9800","9993")
#' classify_morp(morps)
#' classify_morp(c("9811", "9840", "9801"), group = "125")
classify_morp <- function(x, group = c("60", "125")) {
  group <- match.arg(group)
  morp_groups <- dict_maps[["morp_group"]]
  if (group %in% names(morp_groups)) {
    morp_dict <- morp_groups[[group]]
  } else {
    morp_dict <- dict_maps[["60"]]
  }

  res <- unname(morp_dict[as.character(x)])
  res[is.na(res)] <- "99"
  res
}

#' Classify ICD-O-3 topography codes into categories
#'
#' @param x Character vector of ICD-O-3 topography codes.
#'   Values not present in the internal dictionary will be returned as `NA`.
#' @param cancer_type Cancer type.
#'
#' @returns Character vector of categories corresponding to `x`.
#' @export
#'
classify_topo <- function(x, cancer_type = "big") {
  x <- toupper(x)
  topo <- x
  topo[topo == "C15.0"] <- "C15.3"
  topo[topo == "C15.1"] <- "C15.4"
  topo[topo == "C15.2"] <- "C15.5"

  if (cancer_type == "big") {
    single_topo <- paste0(
      "C",
      c(
        11,
        15,
        16,
        22,
        25,
        32,
        33,
        34,
        43,
        50,
        53,
        56,
        61,
        62,
        67,
        73
      )
    )

    topo_prefix <- substr(topo, 1, 3)
    res <- topo_prefix
    keep_full_topo <- topo_prefix %in% single_topo
    res[keep_full_topo] <- topo[keep_full_topo]
  } else {
    res <- substr(topo, 1, 3)
  }

  return(res)
}
