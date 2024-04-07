#' Reframe the fbswicds.
#'
#' @param x Object of class fbswicds.
#' @param ... Stratification variables used to reframe.
#'
#' @return Object of class fbswicd.
#' @export
#'
reframe_fbswicd <- function(x, ...) {
  UseMethod("reframe_fbswicd", x)
}


#' Reframe the fbswicds.
#'
#' @param x Object of class fbswicds.
#' @param ... Stratification variables used to reframe.
#'
#' @return Object of class fbswicd.
#' @export
#'
reframe_fbswicd.fbswicds <- function(x, ...){
  data <- lapply(x, full_fbswicd)
  areacode <- unlist(purrr::map(data, "areacode"))
  fbswicd <- purrr::reduce(purrr::map(data, "fbswicd"), bind_rows)
  fbswicd <- fbswicd %>%
    group_by(across(c(..., "year", "sex", "agegrp", "icd_cat"))) %>%
    reframe(across(c("fbs","sws","mv","ub","sub","m8000","dco","rks"),
                   ~sum(.x)))
  pop <- purrr::reduce(purrr::map(data, "pop"), bind_rows)
  pop <- pop %>%
    group_by(across(c(..., "year", "sex", "agegrp"))) %>%
    reframe(across(c("rks"), ~sum(.x)))
  
  res <- list(areacode = areacode,
              fbswicd = fbswicd,
              pop = pop)
  class(res) <- c("fbswicd", "list")
  return(res)
}


#' Reframe the object of classs canregs.
#'
#' @param x Object of class canregs.
#' @param ... Stratification variables used to reframe.
#' @param type Method for classify cancer sites.
#' @param lang Character value for specify the language used. options are 'cn'
#'        for Chinese, or 'en' for English.
#'
#' @return Object of class fbswicd.
#' @export
#'
reframe_fbswicd.canregs <- function(x, ..., type = "big", lang = "cn"){
  data__ <- count_canreg.canregs(x, sep_zero = FALSE, type = type, lang = lang)
  res <- reframe_fbswicd.fbswicds(data__, ...)
  return(res)
}


full_fbswicd <- function(x) {
  areacode_info <- as.data.frame(tidy_areacode(x$areacode))
  fbswicd <- bind_cols(areacode_info, x$fbswicd)
  pop <- bind_cols(areacode_info, x$pop)
  res <- list(areacode = x$areacode,
              fbswicd = fbswicd,
              pop = pop)
  class(res) <- c("fbswicd", "list")
  return(res)
}
