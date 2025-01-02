## code to prepare `label_cancer` dataset goes here
library(readxl)
label_cancer <- read_xlsx("data-raw/labels.xlsx", sheet = "cancer_category")
label_cancer <- as.list(label_cancer)
