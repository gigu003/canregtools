#' Reframe data of class canregs or fbswicds
#'
#' @rdname cr_reframe
#' @param x Object with class of canregs or fbswicds.
#' @param strat Stratification variables used to reframe 'canregs' or
#'        'fbswicds'.
#'
#' @return Reframed canregs or fbswicds.
#' @export
#' @examples
#' # list reframe vars that could be used in `strat` parameter
#' ls_vars("reframe")
#' data("canregs")
#' # Reframe the `canregs` data according to `area_type` attribute
#' city <- cr_reframe(canregs, strat = "area_type")
#'
cr_reframe <- function(x, strat = "registry") {
  UseMethod("cr_reframe", x)
}

#' @rdname cr_reframe
#' @method cr_reframe canregs
#' @export
#'
#' @examples
#' # Reframe object with class of `canregs`
#' # Reframe the `canregs` according to the `province` attribute
#' province <- cr_reframe(canregs, strat = "province")
#' class(province)
#' names(province)
#'
cr_reframe.canregs <- function(x, strat = "registry") {
  areacodes <- unique(unlist(map(x, pluck("areacode"))))
  new <- map(strat, function(a) {
    pluck(classify_areacode(areacodes), a)
  })
  nnew <- map(new, unique)
  res <- map(seq_along(nnew), function(b) {
    rr <- map(pluck(nnew, b), function(i) {
      loc <- which(pluck(new, b) == i)
      res <- cr_merge(cr_select(x, index = loc))
      res$areacode <- i
      res
    })
    names(rr) <- pluck(nnew, b)
    rr
  })
  res1 <- do.call(c, res)
  if (length(res1) == 1) {
    res1 <- pluck(res1, 1)
  } else {
    res1 <- res1[!duplicated(names(res1))]
    class(res1) <- c("canregs", "list")
  }
  res1
}

#' @rdname cr_reframe
#' @method cr_reframe fbswicds
#' @export
#'
#' @examples
#' # Reframe object with class of `fbswicds`
#' # Convert object with class of `canregs` into object with class of `fbswicds`
#' fbsw <- count_canreg(canregs, cancer_type = "small")
#' # Reframe the `fbswicds` according to the `city` attribute
#' city <- cr_reframe(fbsw, strat = "city")
#'
cr_reframe.fbswicds <- function(x, strat = "registry") {
  areacodes <- unique(unlist(map(x, pluck("areacode"))))
  new_code <- map(strat, function(a) {
    pluck(classify_areacode(areacodes), a)
  })
  tran_code <- map(new_code, unique)
  res <- map(seq_along(tran_code), function(b) {
    rr <- map(pluck(tran_code, b), function(i) {
      loc <- which(pluck(new_code, b) == i)
      res <- cr_merge(cr_select(x, index = loc))
      res$areacode <- i
      res
    })
    names(rr) <- pluck(tran_code, b)
    rr
  })

  output <- do.call(c, res)
  if (length(output) == 1) {
    output <- pluck(output, 1)
  } else {
    class(output) <- c("fbswicds", "list")
  }
  output
}



#' Merge registry attributes into an `fbswicd` object
#'
#' @param x An object with class of `fbswicd`
#'
#' @returns A `fbswicd` object is augmented with registry metadata
#' @noRd
full_fbswicd <- function(x) {
  areacode_info <- as.data.frame(classify_areacode(x[["areacode"]]))
  fbswicd <- bind_cols(areacode_info, x[["fbswicd"]])
  sitemorp <- bind_cols(areacode_info, x[["sitemorp"]])
  pop <- bind_cols(areacode_info, x[["pop"]])
  structure(
    list(areacode = x[["areacode"]], fbswicd = fbswicd, sitemorp = sitemorp,
         pop = pop),
    class = c("fbswicd", "list")
  )
}

#' Sum named numeric vectors
#'
#' Take a list of named numeric vectors and sum values with the same names
#'
#' @param object A list where each element is a named numeric vector
#'
#' @return A named integer vector with names sorted alphabetically and values
#'    summed across vectors.
#' @export
combine_tp <- function(object) {
  if (!is.list(object)) stop("input object must be a list.")
  if (!all(sapply(object, function(x) {
    is.numeric(x) && !is.null(names(x))
  }))) {
    stop("All elements in the list must be named numeric vectors.")
  }
  all_names <- unique(unlist(lapply(object, names)))
  res <- setNames(integer(length(all_names)), all_names)
  for (vec in object) {
    res[names(vec)] <- res[names(vec)] + vec
  }

  res[order(names(res))]
}
