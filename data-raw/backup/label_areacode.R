## code to prepare `label_areacode` dataset goes here
library(readxl)
label_areacode <- read_xlsx("data-raw/labels.xlsx", sheet = "areacode")

