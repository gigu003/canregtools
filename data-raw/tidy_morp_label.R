library(readxl)
library(dplyr)
library(tidyr)
cn <- read_excel("/Users/qc0824/Documents/3_Resources/Website/db/data/ICDO3/ICDO3.xlsx",
                 sheet = "ICD-O-M")
colnames <- c("mark","icdo3", "des_cn", "trans_code")
colnames(cn) <- colnames
en <- read_excel("/Users/qc0824/Documents/3_Resources/Website/db/data/ICDO3/2023.ICD03toICD9CM-ICD10-ICD10CM.xlsx",
                 sheet = "2023.Histologies (ICD-O-3)")
colnames <- c("mark","icdo3", "des_en", "beha","site","icd9cm","icd10","icd10cm", "comments")
colnames(en) <- colnames
en2 <- en |> 
  filter(comments == "Not listed in ICD-O-3.2")



icdo3 <- en |>
  select(icdo3, des_en, comments) |>
  left_join(cn, by = "icdo3")

icdo3_1 <- icdo3 |> 
  filter(!is.na(mark))


icdo3_2 <- icdo3 |> 
  filter(is.na(mark))
