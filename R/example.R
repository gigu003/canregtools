example <- function(){
  file <- system.file("extdata", "410581.xlsx", package = "canregtools")
  res <- read_canreg(file)
  return(res)
}