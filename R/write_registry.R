#' Write Six-digit administrative division code to the user defined dict.
#'
#' @param areacode Six-digit administrative division code.
#' @param name Label for the Six-digit administrative division code.
#'
#' @return Null
#' @export
#'
write_areacode <- function(areacode, name){
  if (!length(areacode) == length(name)) {
    cat("Lengths of 'areacode' and 'name' must be the same.")
    return(NULL)
    }
  # create dict directory if it doesn't exist.
  if(!dir.exists("~/.canregtools/.cache_dict")) {
    dir.create("~/.canregtools/.cache_dict")
  }
  # read dict file if it exist or create an empty one.
  if(file.exists("~/.canregtools/.cache_dict/areacode_dict.rds")) {
    areacode_dict <- readRDS("~/.canregtools/.cache_dict/areacode_dict.rds")
  } else {
    areacode_dict <- data.frame(areacode = character(),
                                name = character(),
                                stringsAsFactors = FALSE)
  }
  #check if the input area codes existed in the dict.
  existing_rows <- areacode %in% areacode_dict$areacode
  #delete the existed rows in the stored area code dict.
  if(any(existing_rows)) {
    areacode_dict <- areacode_dict[!areacode_dict$areacode %in% areacode[existing_rows], ]
  }
  #add the input area codes to the dict.
  areacode_dict <- rbind(areacode_dict, data.frame(areacode = areacode,
                                                   name = name,
                                                   stringsAsFactors = FALSE))
  saveRDS(areacode_dict, file = "~/.canregtools/.cache_dict/areacode_dict.rds")
  print(areacode_dict)
}


#' Write area type to the user defined dict.
#'
#' @param areacode Six-digit administrative division code.
#' @param area_type Area type 'Urban' or 'Rural' for the input Six-digit
#'                  administrative division code.
#'
#' @return Null
#' @export
#'
write_area_type <- function(areacode, area_type){
  if (!length(areacode) == length(area_type)){
    cat("Lengths of 'areacode' and 'area_type' must be the same.")
    return(NULL)
    }
  # create dict directory if it doesn't exist.
    if(!dir.exists("~/.canregtools/.cache_dict")){
      dir.create("~/.canregtools/.cache_dict")
    }
  # read dict file if it exist or create an empty one.
    if(file.exists("~/.canregtools/.cache_dict/area_type_dict.rds")) {
      area_type_dict <- readRDS("~/.canregtools/.cache_dict/area_type_dict.rds")
    } else {
      area_type_dict <- list()  
    }
    area_type_dict[areacode] <- area_type
    saveRDS(area_type_dict, file = "~/.canregtools/.cache_dict/area_type_dict.rds")
    print(area_type_dict)
}


#' Write the registry definition to the user defined dict.
#'
#' @param areacode Six-digit administrative division code.
#' @param registry The name of the registry corresponding to the six-digit
#'                  administrative division code.
#'
#' @return Null
#' @export
#'
write_registry <- function(areacode, registry){
  if (!length(areacode) == length(registry)){
    cat("Lengths of 'areacode' and 'registry' must be the same.")
    return(NULL)
  }
  # create dict directory if it doesn't exist.
    if(!dir.exists("~/.canregtools/.cache_dict")) {
      dir.create("~/.canregtools/.cache_dict")
    }
  # read dict file if it exist or create an empty one.
    if(file.exists("~/.canregtools/.cache_dict/registry_dict.rds")) {
      registry_dict <- readRDS("~/.canregtools/.cache_dict/registry_dict.rds")
    } 
    registry_dict[areacode] <- registry
    saveRDS(registry_dict, file = "~/.canregtools/.cache_dict/registry_dict.rds")
    print(registry_dict)
}
