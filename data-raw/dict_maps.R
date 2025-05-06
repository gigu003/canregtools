## code to prepare `dict_maps` dataset goes here
library(readxl)
library(dplyr)
library(here)
names <- c("areacode_registry", "areacode_area_type", "registry_type", "std_pop",
           "prov_region","icd10_code")
dict_maps <- lapply(names, function(x){
  res_temp <- read_excel(here("data-raw/dict_maps.xlsx"), sheet = x)
  if (x %in% c("areacode_registry", "areacode_area_type",
               "registry_type", "prov_region")){
    res <- as.character(res_temp$value)
    names(res) <- res_temp$name
    return(res)
  } else if (x %in% c("std_pop")){
    res <- res_temp |> 
      mutate(agegrp = factor(agegrp, levels = as.character(1:19),
                             labels = as.character(1:19)))
    return(res)
  } else {
    return(res_temp)
  }
})
names(dict_maps) <- names


