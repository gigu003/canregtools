## code to prepare `dictionary` dataset goes here
library(devtools)
load_all()
library(dplyr)
library(readxl)
source("./data-raw/tidy_var_maps.R")
source("./data-raw/dict_maps.R")
source("./data-raw/ICCC3Rule.R")
usethis::use_data(
  tidy_var_maps,
  dict_maps,
  ICCC3Rule,
  internal = TRUE, overwrite = TRUE
)
