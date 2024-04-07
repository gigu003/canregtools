check_areacode <- function(x){
  # Check if the province code existed.
  provs <- c(11:15, 21:23, 31:37, 41:46, 50:54, 61:65, 71, 81, 82)
  prov_logi <- substr(x, 1, 2) %in% provs
  # Check if the district code is valid (6 digits)
  format_logi <- grepl("^\\d{6}$", x)
  
  ## check if the area codes actually existed.
  provs_x <- unique(substr(x, 1, 2))
  areacodes <- read_areacode(prov = provs_x)
  areacodes <- substr(areacodes$areacode, 1, 6)
  exist <- x %in% areacodes
  check <- prov_logi & format_logi & exist
  return(check)
}