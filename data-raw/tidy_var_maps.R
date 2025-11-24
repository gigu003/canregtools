## code to prepare `tidy_map` dataset goes here
library(readxl)
library(here)
var_names <- c(
  "cancer", "region", "province", "areacode", "area_type",
  "sex", "edu", "trib", "occu", "marri",
  "grad", "beha", "basi", "treat",
  "status", "caus", "deadplace", "lost",
  "stats", "summary", "reframe", "std",
  "morp_group", "icd10"
)
tidy_var_maps <- lapply(var_names, function(x) {
  read_excel(here("data-raw/vars_label.xlsx"), sheet = x)
})
names(tidy_var_maps) <- var_names
