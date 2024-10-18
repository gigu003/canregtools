## code to prepare `dictionary` dataset goes here
library(devtools)
load_all()
library(dplyr)
library(readxl)
source("./data-raw/std_pop.R")
source("./data-raw/label_cancer.R")
source("./data-raw/ethinic.R")
source("./data-raw/label_child.R")
source("./data-raw/occu.R")
source("./data-raw/label_region.R")
source("./data-raw/registry_dict.R")
source("./data-raw/ICCC3Rule.R")
source("./data-raw/label_stat_var.R")
source("./data-raw/dict_registry.R")
usethis::use_data(
  label,
  std_pop,
  ethnic_map,
  label_child,
  label_cancer,
  label_stat_var,
  occu_map,
  prov_label,
  region_label,
  registry_dict,
  ICCC3Rule,
  dict_registry,
  internal = TRUE, overwrite = TRUE)

