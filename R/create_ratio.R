#' Title
#'
#' @rdname create_ratio
#' @param x Data of class 'fbswicd'.
#' @param ... Stratification variables.
#'
#' @return A data frame.
#' @export
#'
create_ratio <- function(x, ...) {
  UseMethod("create_ratio", x)
}

#' @rdname create_ratio
#' @method create_ratio fbswicd
#' @export
create_ratio.fbswicd <- function(x, ...) {
  sitemorp <- x$sitemorp 
  
  # Group by the specified variables
  sitemorp <- sitemorp |> 
    group_by(...) |> 
    reframe(
      site = list(combine_tp(site)),
      morp = list(combine_tp(morp))
    )
  return(sitemorp)
}


combine_tp <- function(x){
  res <- purrr::reduce(x, bind_rows)
  col_names <- colnames(res)
  var1 <- rlang::sym(col_names[1])
  var2 <- rlang::sym(col_names[2])
  res <- res |> 
    group_by(!!var1) |> 
    reframe(!!var2 := sum(!!var2)) |> 
    arrange(!!var1)
  return(res)
}
