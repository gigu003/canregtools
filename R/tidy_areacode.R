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