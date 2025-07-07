#' Get the areacodes of the registry
#'
#' Query the area codes corresponding to a given registry type.
#'
#' @param regi_type Numeric or character vector indicating registry types.
#'   Defaults to 1:4.
#'
#' @return A character vector of area codes.
#' @export
#'
#' @examples
#' show_registry(1:4)
#' show_registry(c("1", "2"))
show_registry <- function(regi_type = 1:4) {
  registry_category <- dict_maps[["registry_type"]]
  
  # Coerce both input and dict values to character for comparison
  regi_type <- as.character(regi_type)
  reg_cat_values <- as.character(registry_category)
  names(registry_category)[reg_cat_values %in% regi_type]
}
