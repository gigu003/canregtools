#' Write Six-digit administrative division code to the user defined dict.
#'
#' @param x A data frame contains areacode, label_cn, label_en, label_abbr_cn
#'        label_abbr_en
#'
#' @return Null
#' @export
#'
write_areacode <- function(x) {
  # create dict directory if it doesn't exist.
  if(!file.exists("~/.canregtools/label_areacode.dcf")) {
    write.dcf(label_areacode, "~/.canregtools/label_areacode.dcf")
    } else {
    label_area <- read.dcf("~/.canregtools/label_areacode.dcf")
    label_area <- as.data.frame(label_area)
    }
  # delete the existed rows in the stored area code dict.
  areacode <- rlang::sym("areacode")
  label_area <- label_area |> filter(!!areacode %nin% pluck(x, "areacode"))
  
  ## modify x
  x <- x |> filter(!!areacode %nin% paste0(c(91:92, 72:79), "0000"))
  # add the input area codes label to dict.
  label_area <- rbind(label_area, x)
  write.dcf(label_area, "~/.canregtools/label_areacode.dcf")
  print(label_area)
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
    } else {
      registry_dict <- list()
    }
  areacode <- as.character(areacode)
  registry <- as.character(registry)
  registry_dict[areacode] <- registry
  saveRDS(registry_dict, file = "~/.canregtools/.cache_dict/registry_dict.rds")
  print(as_tibble(data.frame(areacode = names(registry_dict),
                             registry = unlist(registry_dict,
                                               use.names = FALSE))))
}
