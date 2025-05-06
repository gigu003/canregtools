## code to prepare `canregs` dataset goes here
addr <- here::here("data-raw", "data")
files <- list.files(addr, full.names = TRUE)
canregs <- read_canreg(files)
usethis::use_data(canregs, overwrite = TRUE)
