## code to prepare `dict_maps` dataset goes here
library(readxl)
library(dplyr)
library(here)
library(crcheck)
names <- c(
  "areacode_registry", "areacode_area_type", "areacode_custom", "registry_type",
  "std_pop", "prov_region", "icd10_code", "lang", "morp_multi"
  )
dict_maps <- lapply(names, function(x) {
  res_temp <- read_excel(here("data-raw/dict_maps.xlsx"), sheet = x)
  if (x %in% c(
    "areacode_registry", "areacode_area_type", "areacode_custom",
    "registry_type", "prov_region", "lang"
  )) {
    res <- as.character(res_temp$value)
    names(res) <- res_temp$name
    res
  } else if (x == "morp_multi"){
    names <- res_temp$name
    res <- lapply(1:17, function(x){
      namex <- recode_morp(strsplit(names[x], ",")[[1]])
      setNames(rep(as.character(x), length(namex)), namex)
    })
    unlist(res)
  } else if (x %in% c("std_pop")) {
    res <- res_temp |>
      mutate(agegrp = factor(agegrp,
        levels = as.character(1:19),
        labels = as.character(1:19)
      ))
    res
  } else {
    res_temp
  }
})
names(dict_maps) <- names
