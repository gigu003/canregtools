## code to prepare `canregs` dataset goes here
addr <- system.file("extdata", package = "canregtools")
files <- list.files(addr, pattern = "\\.xlsx$", full.names = TRUE)
canregs <- read_canreg(files)
usethis::use_data(canregs, overwrite = TRUE)
