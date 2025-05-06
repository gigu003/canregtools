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
  byvars <- rlang::syms(c("year", "sex", "agegrp"))
  sumvar <- rlang::sym("rks")
  areacode <- unique(unlist(map(data, pluck("areacode"))))
  fb <- map_dfr(map(data, pluck("FBcases")), bind_rows)
  sw <- map_dfr(map(data, pluck("SWcases")), bind_rows)
  pop <- map_dfr(map(data, pluck("POP")), bind_rows) |> 
    group_by(!!!byvars) |> 
    reframe(!!sumvar := sum(!!sumvar))
  class(pop) <- c("population", class(pop))
  res <- list(
    areacode = areacode,
    FBcases = fb,
    SWcases = sw,
    POP = pop
    )
  class(res) <- c("canreg", "list")
  return(res)
}


#' @rdname cr_merge
#' @method cr_merge fbswicds
#' @export
cr_merge.fbswicds <- function(data){
  site <- rlang::sym("site")
  morp <- rlang::sym("morp")
  accept_vars <- names(classify_areacode("10302"))
  areacode <- unique(unlist(map(data, pluck("areacode"))))
  gvars <- c("year", "sex", "agegrp", "cancer")
  svars <- c("fbs", "sws", "mv", "ub", "sub", "m8000", "dco")
  fbswicd <- cmerge(data, nested = "fbswicd")
  fbswicd <- fbswicd |> group_by(!!!rlang::syms(gvars)) |> 
    reframe(across(c(!!!rlang::syms(svars)), ~ sum(.x, na.rm = TRUE)))
  pop <- cmerge(data, nested = "pop")
  sum_vars <- setdiff(names(pop), c(accept_vars, "year", "sex", "agegrp")) |> 
    rlang::syms()
  pop <- pop |>  group_by(!!!rlang::syms(setdiff(gvars, "cancer"))) |> 
    reframe(across(c(!!!sum_vars), ~ sum(.x, na.ram = TRUE)))
  sitemorp <- cmerge(data, nested = "sitemorp")
  sitemorp <- sitemorp |> 
    group_by(!!!rlang::syms(setdiff(gvars, "agegrp"))) |> 
    reframe(!!site := list(combine_tp(!!site)),
            !!morp := list(combine_tp(!!morp)))
  res <- list(
    areacode = areacode,
    fbswicd = fbswicd,
    sitemorp = sitemorp,
    pop = pop
  )
  class(res) <- c("fbswicd", "list")
  return(res)
}


#' @rdname cr_merge
#' @method cr_merge asrs
#' @export
cr_merge.asrs <- function(data){
  res <- cmerge(data)
  return(res)
}

#' @rdname cr_merge
#' @method cr_merge qualities
#' @export
cr_merge.qualities <- function(data){
  res <- cmerge(data)
  return(res)
}

#' @rdname cr_merge
#' @method cr_merge age_rates
#' @export
cr_merge.age_rates <- function(data){
  res <- cmerge(data)
  return(res)
}

#' @rdname cr_merge
#' @method cr_merge summaries
#' @export
cr_merge.summaries <- function(data){
  res <- purrr::map(data, function(f){
    purrr::map(f, function(x){
      if (length(x) > 1) {
        return(paste(x, collapse = ", "))
      } else if (length(x) == 0) {
        return(" ")
      } else{
        return(x)
      }
    }) |> 
      as.data.frame()
  })
  res <- cmerge(res)
  class(res) <- c("summary", "list")
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
cmerge <- function(data, nested = NULL){
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
