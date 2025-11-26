#' Clean canreg data.
#'
#' @rdname cr_write
#' @param x Object with class of `canreg` or `canregs`.
#'
#' @return Object with class of `canreg` or `canregs`.
#' @export
#' 
cr_write <- function(x) {
  UseMethod("cr_write", x)
}

#' @rdname cr_write
#' @method cr_write canregs
#' @export
cr_write.canregs <- function(x) {
  structure(
    purrr::map(
      x,
      cr_write
    ),
    class = class(x)
  )
}

#' @rdname cr_write
#' @method cr_write canreg
#' @export
cr_write.canreg <- function(x) {
  res <- x
  areacode <- pluck(x, "areacode")
  areacode <- data.frame(
    areacode = areacode,
    name = tidy_var(areacode,
                    var_name = "areacode",
                    lang = "cn"),
    registry = tidy_var(classify_areacode(areacode)$registry,
                        var_name = "areacode", lang = "cn"),
    city = tidy_var(classify_areacode(areacode)$city,
                    var_name = "areacode", lang = "cn")
    )
  structure(
    list(
      areacode = areacode,
      FB = pluck(x, "FBcases"),
      SW = pluck(x, "SWcases"),
      POP = pluck(x, "POP")
    ),
    class = class(x)
  )
  
}
