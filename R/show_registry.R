#' Title
#'
#' @param registry_type  registry type
#' @param source \code{character} source of areacode
#'
#' @return Character vector
#' @export
#'
#'
show_registry <- function(registry_type = "all", source = "system") {
  
  if (registry_type == "all") {
    type <- c(1L, 2L)
  } else if (registry_type == "national") {
    type <- 1L
  } else if (registry_type == "province") {
    type <- 2L
  }
  areacode <- rlang::sym("areacode")
  areacode2 <- rlang::sym("areacode2")
  if (!source == "system") {
    dict_registry <- dplyr::mutate(dict_registry,
                                   !!areacode := ifelse(!is.na(!!areacode2),
                                                     !!areacode2, !!areacode))
  }
  
  huaihe <- rlang::sym("huaihe")
  registry <- rlang::sym("registry")
  
  if (registry_type == "huaihe") {
    dict_registry |> 
      filter(!!huaihe == 1) |> 
      pull(!!areacode)
  } else if (registry_type == "province") {
    dict_registry |> 
      pull(!!areacode)
  } else {
    dict_registry |> 
      filter(!!registry %in% type) |> 
      pull(!!areacode)
  }
}

