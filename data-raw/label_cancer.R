## code to prepare `label_cancer` dataset goes here
library(readxl)
label_cancer <- read_xlsx("data-raw/cancer_category.xlsx")
label_cancer <- as.list(label_cancer)
