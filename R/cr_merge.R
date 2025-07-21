#' Merge elements from objects of class `canregs`, `fbswicds`, or `asrs`
#'
#' @description
#' Merge elements from object with class of `canregs`, `fbswicds`, `asrs`,
#' `qualities`, `age_rates`, or `summaries` into object with class of `canreg`,
#' `fbswicd`, `asr`, `quality`, `age_rate`, or `summary`.
#'
#' @rdname cr_merge
#' @param data An object with class of `canregs`, `fbswicds`, `asrs`,
#'        `qualities`, `age_rates`, or `summaries`.
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
#' @examples
#' data("canregs")
#' canreg <- cr_merge(canregs)
#' class(canreg)
#'
cr_merge.canregs <- function(data) {
  byvars <- rlang::syms(c("year", "sex", "agegrp"))
  sumvar <- get_sumvar(pluck(data, 1))
  sum_sym <- rlang::syms(sumvar)
  areacode <- map_chr(data, "areacode") |> unique()
  fb <- cmerge(data, "FBcases")
  sw <- cmerge(data, "SWcases")
  pop <- cmerge(data, "POP") |>
    group_by(!!!byvars) |>
    reframe(across(!!rlang::sym("rks"), sum, na.rm = TRUE))
  
  class(pop) <- c("POP", class(pop))

  structure(
    list(areacode = areacode, FBcases = fb, SWcases = sw, POP = pop),
    class = c("canreg", "list")
  )
}

#' @rdname cr_merge
#' @method cr_merge fbswicds
#' @export
#' @examples
#' # Merge obejct with class of `fbswicds` into obejct with class of `fbswicd`
#' fbsws <- count_canreg(canregs)
#' fbsw <- cr_merge(fbsws)
#'
cr_merge.fbswicds <- function(data) {
  site <- rlang::sym("site")
  morp <- rlang::sym("morp")
  accept_vars <- names(classify_areacode("10302"))
  areacode <- unique(unlist(map(data, pluck("areacode"))))
  gvars <- c("year", "sex", "agegrp", "cancer")
  svars <- c("fbs", "sws", "mv", "ub", "sub", "mvs", "dco")
  fbswicd <- cmerge(data, nested = "fbswicd")
  fbswicd <- fbswicd |>
    group_by(!!!rlang::syms(gvars)) |>
    reframe(across(c(!!!rlang::syms(svars)), ~ sum(.x, na.rm = TRUE)))
  pop <- cmerge(data, nested = "pop")
  sum_vars <- setdiff(names(pop), c(accept_vars, "year", "sex", "agegrp")) |>
    rlang::syms()
  pop <- pop |>
    group_by(!!!rlang::syms(setdiff(gvars, "cancer"))) |>
    reframe(across(c(!!!sum_vars), ~ sum(.x, na.ram = TRUE)))
  sitemorp <- cmerge(data, nested = "sitemorp")
  sitemorp <- sitemorp |>
    group_by(!!!rlang::syms(setdiff(gvars, "agegrp"))) |>
    reframe(
      !!site := list(combine_tp(!!site)),
      !!morp := list(combine_tp(!!morp))
    )
  exclude = cmerge(data, nested = "exclude")

  structure(
    list(areacode = areacode, fbswicd = fbswicd, sitemorp = sitemorp,
         pop = pop, exclude = exclude),
    class = c("fbswicd", "list")
  )
}


#' @rdname cr_merge
#' @method cr_merge asrs
#' @export
#' @examples
#' # Merge obejct with class of `asrs` into object with class of `asr`
#' asrs <- create_asr(canregs, year, sex, cancer, collapse = FALSE)
#' asr <- cr_merge(asrs)
#'
cr_merge.asrs <- function(data) {
  cmerge(data)
}

#' @rdname cr_merge
#' @method cr_merge qualities
#' @export
#' @examples
#' # Merge obejct with class of `qualities` into object with class of `quality`
#' quas <- create_quality(canregs, year, sex, cancer, collapse = FALSE)
#' qua <- cr_merge(quas)
#'
cr_merge.qualities <- function(data) {
  cmerge(data)
}

#' @rdname cr_merge
#' @method cr_merge age_rates
#' @export
#' @examples
#' # Merge obejct with class of `age_rates` into object with class of `age_rate`
#' agerates <- create_age_rate(canregs, year, sex, cancer, collapse = FALSE)
#' agerate <- cr_merge(agerates)
#'
cr_merge.age_rates <- function(data) {
  cmerge(data)
}

#' @rdname cr_merge
#' @method cr_merge summaries
#' @export
#' @examples
#' # Merge obejct with class of `summaries` into object with class of `summary`
#' summs <- summary(canregs, collapse = FALSE)
#' summ <- cr_merge(summs)
#'
cr_merge.summaries <- function(data) {
  res <- purrr::map(data, function(f) {
    purrr::map(f, function(x) {
      if (length(x) > 1) {
        paste(x, collapse = ", ")
      } else if (length(x) == 0) {
        " "
      } else {
        x
      }
    }) |>
      as.data.frame()
  })

  structure(cmerge(res), class = c("summary", "list"))
}

#' Merge list
#'
#' @param data A list to be merged.
#' @param nested A nested list element name, if applicable.
#'
#' @return A merged data frame with an `areacode` column as the first column.
#' @noRd
cmerge <- function(data, nested = NULL) {
  areacode <- rlang::sym("areacode")
  purrr::map_dfr(
    names(data),
    function(f) {
      df <- if (is.null(nested)) {
        purrr::pluck(data, f)
      } else {
        purrr::pluck(data, f, nested)
      }
      df |>
        mutate(!!areacode := as.integer(f), .before = everything())
    }
  )
}
