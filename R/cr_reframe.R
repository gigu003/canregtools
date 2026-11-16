#' Reframe data of class canregs or fbswicds
#'
#' @rdname cr_reframe
#' @param x Object with class of canregs or fbswicds.
#' @param strat Stratification variables used to reframe 'canregs' or
#'        'fbswicds'. Use [cr_map()] to configure registry, area-type, or
#'        custom grouping mappings.
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
  regicodes <- vapply(
    x,
    function(object) {
      as.character(object[["regicode"]])
    },
    character(1)
  ) |>
    unname()
  new <- reframe_group_codes(strat, regicodes)
  res <- unname(map(new, reframe_groups, x = x))
  res1 <- do.call(c, res)
  if (length(res1) == 1) {
    res1 <- pluck(res1, 1)
    class(res1) <- c("canreg", "list")
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
  regicodes <- vapply(
    x,
    function(object) {
      as.character(object[["regicode"]])
    },
    character(1)
  ) |>
    unname()
  new_code <- reframe_group_codes(strat, regicodes)
  res <- unname(map(new_code, reframe_groups, x = x))

  output <- do.call(c, res)
  if (length(output) == 1) {
    output <- pluck(output, 1)
  } else {
    class(output) <- c("fbswicds", "list")
  }
  output
}


reframe_codes <- function(strat, regicodes) {
  if (identical(strat, "regicode")) {
    return(regicodes)
  }

  grouped_codes <- pluck(classify_areacode(regicodes, attr = strat), strat)
  ifelse(is.na(grouped_codes), regicodes, grouped_codes)
}


reframe_group_codes <- function(strat, regicodes) {
  custom_strat <- setdiff(strat, "regicode")
  classified <- if (length(custom_strat) > 0L) {
    classify_areacode(regicodes, attr = custom_strat)
  } else {
    list()
  }

  setNames(
    lapply(strat, function(variable) {
      if (identical(variable, "regicode")) {
        regicodes
      } else {
        grouped_codes <- classified[[variable]]
        ifelse(is.na(grouped_codes), regicodes, grouped_codes)
      }
    }),
    strat
  )
}


reframe_groups <- function(group_codes, x) {
  group_codes <- unname(group_codes)
  group_names <- unique(group_codes)
  groups <- lapply(group_names, function(group) {
    loc <- if (is.na(group)) {
      which(is.na(group_codes))
    } else {
      which(group_codes == group)
    }
    selected <- structure(x[loc], class = class(x))
    result <- cr_merge(selected)
    result$regicode <- group
    result
  })
  names(groups) <- group_names
  groups
}


#' Merge registry attributes into an `fbswicd` object
#'
#' @param x An object with class of `fbswicd`
#'
#' @returns A `fbswicd` object is augmented with registry metadata
#' @noRd
full_fbswicd <- function(x) {
  regicode_info <- as.data.frame(classify_areacode(x[["regicode"]]))
  regicode_info$regicode <- as.character(x[["regicode"]])
  fbswicd <- bind_cols(regicode_info, x[["fbswicd"]])
  sitemorp <- bind_cols(regicode_info, x[["sitemorp"]])
  pop <- bind_cols(regicode_info, x[["pop"]])
  structure(
    list(
      regicode = x[["regicode"]],
      fbswicd = fbswicd,
      sitemorp = sitemorp,
      pop = pop
    ),
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
  if (!is.list(object)) {
    stop("input object must be a list.")
  }
  if (length(object) == 0L) {
    return(stats::setNames(integer(), character()))
  }
  if (
    !all(sapply(object, function(x) {
      is.numeric(x) && !is.null(names(x))
    }))
  ) {
    stop("All elements in the list must be named numeric vectors.")
  }
  all_names <- unique(unlist(lapply(object, names)))
  res <- setNames(integer(length(all_names)), all_names)
  for (vec in object) {
    res[names(vec)] <- res[names(vec)] + vec
  }

  res[order(names(res))]
}
