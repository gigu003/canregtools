## code to prepare `quality` dataset goes here
library(here)
library(readxl)
quality <- read_excel(here("data-raw", "quality.xlsx")) |>
  mutate(year = as.integer(year))
usethis::use_data(quality, compress = "xz", overwrite = TRUE)

## code to prepare `canregs` dataset goes here
files <- list.files("data-raw/data", pattern = "\\.xlsx$", full.names = TRUE)
canregs <- read_canreg(files)
usethis::use_data(canregs, compress = "xz", overwrite = TRUE)
