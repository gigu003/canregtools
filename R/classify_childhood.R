#' Classify pediatric tumors according to the ICCC3 standards.
#' 
#' @details
#' This function classify the 'topo' and 'morp' parts of ICDO3 codes into
#' ICCC3 (International Classification of Childhood Cancer, Third edition).
#' 
#' @param topo Topography codes of ICDO3, it could be in the format of 'C15.6'
#'        or C156'. 
#' @param morp Morphology codes of ICDO3, it could be in the format of '8000' or
#'        'M8140'.
#' @param beha Behaviour code of ICDO3.
#' @param type Type of Classification for the output, options are 'main' or
#'        'sub', default is 'main'.
#' @param version The ICCC3 standards, "v2005" or "v2017".
#'
#' @return Recode of ICCC3 classification.
#' @export
#'
classify_childhood <- function(topo,
                               morp,
                               beha,
                               type = "sub",
                               version = "v2005") {
  topo <- toupper(topo)
  morp <- gsub("[^0-9]", "", morp)
  if (version == "v2005"){
    rule <- ICCC3Rule$v2005$main
  } else if (version == "v2017"){
    rule <- ICCC3Rule$v2017
  }
  
  morp_list <- rule$morp
  topo_list <- rule$topo
  recode_list <- rule$recode
  id_groups <- function(x) {
    morp %in% morp_list[[x]] & topo %in% topo_list[[x]]
  }
  result <- lapply(1:69, id_groups)
  res_matrix <- do.call(rbind, result)
  groups <- apply(res_matrix, 2, find_pos)
  recodes <- unlist(recode_list)[groups]
  recodes <- ifelse(is.na(recodes), 999, recodes)
  sub <- recodes
  main <- floor(as.numeric(recodes) / 10)
  if (type == "sub") {
    return(sub)
  } else if (type == "main") {
    return(main)
  }
}

find_pos <- function(x) {
  which(x == TRUE, arr.ind = TRUE)[1]
}

