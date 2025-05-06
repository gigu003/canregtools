#' Get the areacodes of the registry
#'
#' @param regi_type  Type of registry.
#'
#' @return Character vector
#' @export
#'
#'
show_registry <- function(regi_type = c(1:4)) {
  regi_type <- as.character(regi_type)
  registry_category <- dict_maps[["registry_type"]]
  areacodes <- names(registry_category[registry_category %in% regi_type])
  return(areacodes)
}

