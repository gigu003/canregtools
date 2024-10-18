#' Merge elements from objects of class canregs, fbswicds, or asrs
#' 
#' @description
#' Merge elements from object of class 'canregs', 'fbswicds', or 'asrs' based
#' on provided index or conditions. The class of the returned object will be the
#' same with the input object.
#' 
#' @rdname cr_merge
#' @param data An object of class 'canregs', 'asrs', or 'fbswicds'.
#' 
#' @return An object with merged elements.
#' @export
#'
cr_merge <- function(data) {
  UseMethod("cr_merge", data)
}

#' @rdname cr_merge
#' @method cr_merge canregs
#' @export
cr_merge.canregs <- function(data){
  FBcases <- cr_merge(data, nested = "FBcases")
  SWcases <- cr_merge(data, nested = "SWcases")
  POP <- cr_merge(data, nested = "POP")
  res <- list(
    areacode = names(data)[1],
    FBcases = FBcases,
    SWcases = SWcases,
    POP= POP
  )
  class(res) <- c("canreg", "list")
  return(res)
}

#' @rdname cr_merge
#' @method cr_merge asrs
#' @export
cr_merge.asrs <- function(data){
  res <- cr_merge(data)
  return(res)
}

#' @rdname cr_merge
#' @method cr_merge qualities
#' @export
cr_merge.qualities <- function(data){
  res <- cr_merge(data)
  return(res)
}

#' Merge list
#'
#' @param data list need to be merged.
#' @param nested Nested list.
#'
#' @return object
#' @export
#'
cr_merge <- function(data, nested = NULL){
  areacode <- rlang::sym("areacode")
  purrr::reduce(
    purrr::map(names(data), function(f){
      if (is.null(nested)){
        data |> 
          purrr::pluck(f) |> 
          mutate(!!areacode := as.integer(f)) |> 
          select(!!areacode, everything())
      } else {
        data |> 
          purrr::pluck(f) |> 
          purrr::pluck(nested) |> 
          mutate(!!areacode := as.integer(f)) |> 
          select(!!areacode, everything())
      }
    }),
    bind_rows
  )
}
