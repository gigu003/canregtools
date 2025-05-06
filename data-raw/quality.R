## code to prepare `quality` dataset goes here
library(here)
library(readxl)
quality <- read_excel(here("data-raw", "quality.xlsx")) |> 
  mutate(year = as.integer(year))
usethis::use_data(quality, overwrite = TRUE)
