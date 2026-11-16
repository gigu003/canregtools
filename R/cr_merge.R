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
#' @details Merged `canreg` objects store original registry identifiers in
#'   `source_regicodes`. For multiple sources, the scalar `regicode` is
#'   `merged:` followed by URL-encoded source codes joined with `+`. A single
#'   source retains its identifier. Case-level source identifiers are retained.
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
  source_regicodes <- unique(unlist(
    lapply(data, function(x) {
      if (is.null(x$source_regicodes)) x$regicode else x$source_regicodes
    }),
    use.names = FALSE
  ))
  if (
    length(source_regicodes) == 0L ||
      anyNA(source_regicodes) ||
      any(!nzchar(trimws(source_regicodes)))
  ) {
    stop("Merged registries must have non-empty source identifiers.")
  }
  source_regicodes <- as.character(source_regicodes)
  regicode <- if (length(source_regicodes) == 1L) {
    source_regicodes
  } else {
    paste0(
      "merged:",
      paste(
        vapply(
          source_regicodes,
          utils::URLencode,
          character(1),
          reserved = TRUE,
          repeated = TRUE
        ),
        collapse = "+"
      )
    )
  }
  fb <- cmerge(data, "FBcases")
  sw <- cmerge(data, "SWcases")
  has_sf <- vapply(data, function(x) is.data.frame(x$SFcases), logical(1))
  if (any(has_sf)) {
    sf <- cmerge(data[has_sf], "SFcases")
  }

  pop_raw <- cmerge(data, "POP")
  possible_vars <- c("rks", "death")
  vars_exist <- intersect(possible_vars, names(pop_raw))
  if (length(vars_exist) == 0) {
    stop("POP data contains neither 'rks' nor 'death'. Cannot merge.")
  }
  pop <- pop_raw |>
    dplyr::group_by(!!!byvars) |>
    dplyr::reframe(
      dplyr::across(all_of(vars_exist), \(x) sum(x, na.rm = TRUE))
    ) |>
    standardize_year_sex()

  class(pop) <- c("POP", class(pop))

  res <- structure(
    list(regicode = regicode, FBcases = fb, SWcases = sw),
    class = c("canreg", "list")
  )

  if (any(has_sf)) {
    res <- append(res, list(SFcases = sf))
  }

  res <- append(res, list(POP = pop))
  res$source_regicodes <- source_regicodes
  class(res) <- c("canreg", "list")
  return(res)
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
  site <- rlang::sym("icd10")
  morp <- rlang::sym("morp")
  accept_vars <- regicode_vars()
  regicode <- unique(unlist(map(data, pluck("regicode"))))
  gvars <- c("year", "sex", "agegrp", "cancer")

  supported_stats <- c(
    "fbs",
    "sws",
    "mv",
    "ub",
    "sub",
    "mvs",
    "dco",
    "ids",
    "mult"
  )
  statistic_sets <- lapply(data, function(x) {
    intersect(supported_stats, names(x$fbswicd))
  })
  same_schema <- vapply(
    statistic_sets,
    setequal,
    logical(1),
    y = statistic_sets[[1]]
  )
  if (!all(same_schema)) {
    stop("All `fbswicd` objects must contain the same statistic columns.")
  }
  svars <- supported_stats[supported_stats %in% statistic_sets[[1]]]
  if (!"fbs" %in% svars) {
    stop("Each `fbswicd` object must contain the required `fbs` column.")
  }

  fbswicd <- cmerge(data, nested = "fbswicd")
  fbswicd <- fbswicd |>
    group_by(!!!rlang::syms(gvars)) |>
    reframe(across(c(!!!rlang::syms(svars)), ~ sum(.x, na.rm = TRUE))) |>
    standardize_year_sex()
  pop <- cmerge(data, nested = "pop")
  sum_vars <- dplyr::setdiff(
    names(pop),
    c(accept_vars, "year", "sex", "agegrp")
  ) |>
    rlang::syms()
  pop <- pop |>
    group_by(!!!rlang::syms(dplyr::setdiff(gvars, "cancer"))) |>
    reframe(across(c(!!!sum_vars), \(x) sum(x, na.rm = TRUE))) |>
    standardize_year_sex()
  result <- list(regicode = regicode, fbswicd = fbswicd)
  has_sitemorp <- vapply(data, function(x) "sitemorp" %in% names(x), logical(1))
  if (any(has_sitemorp)) {
    sitemorp <- cmerge(data[has_sitemorp], nested = "sitemorp")
    result$sitemorp <- sitemorp |>
      group_by(!!!rlang::syms(dplyr::setdiff(gvars, "agegrp"))) |>
      reframe(
        !!site := list(combine_tp(!!site)),
        !!morp := list(combine_tp(!!morp))
      ) |>
      standardize_year_sex()
  }
  result$pop <- pop
  has_exclude <- vapply(data, function(x) "exclude" %in% names(x), logical(1))
  if (any(has_exclude)) {
    result$exclude <- cmerge(data[has_exclude], nested = "exclude") |>
      standardize_year_sex()
  }
  structure(result, class = c("fbswicd", "list"))
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
#' @method cr_merge site_morps
#' @export
cr_merge.site_morps <- function(data) {
  if (
    !is.data.frame(data[[1]]) &&
      is.list(data[[1]]) &&
      all(c("icd10", "morp") %in% names(data[[1]]))
  ) {
    return(list(
      icd10 = cmerge(data, nested = "icd10"),
      morp = cmerge(data, nested = "morp")
    ))
  }

  cmerge(data)
}

#' @rdname cr_merge
#' @method cr_merge checks
#' @export
#'
cr_merge.checks <- function(data) {
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
#' @return A merged data frame with an `regicode` column as the first column.
#' @noRd
cmerge <- function(data, nested = NULL) {
  if (length(data) == 0L) {
    return(dplyr::bind_rows(data))
  }

  data_names <- names(data)
  if (is.null(data_names)) {
    data_names <- as.character(seq_along(data))
  }
  index <- seq_along(data)
  frames <- lapply(index, function(i) {
    if (is.null(nested)) data[[i]] else data[[i]][[nested]]
  })
  has_source_codes <- vapply(
    index,
    function(i) {
      ((inherits(data[[i]], "canreg") &&
        "source_regicodes" %in% names(data[[i]])) ||
        (inherits(data[[i]], "fbswicd") &&
          length(data[[i]]$regicode) > 1L)) &&
        "regicode" %in% names(frames[[i]])
    },
    logical(1)
  )

  if (all(has_source_codes)) {
    return(standardize_year_sex(dplyr::bind_rows(frames)))
  }

  codes <- vapply(
    index,
    function(i) {
      code <- data[[i]][["regicode"]]
      if (has_source_codes[[i]]) {
        return("")
      }
      if (is.null(code)) {
        data_names[[i]]
      } else if (length(code) > 1L) {
        paste0(
          "merged:",
          paste(
            vapply(
              code,
              utils::URLencode,
              character(1),
              reserved = TRUE,
              repeated = TRUE
            ),
            collapse = "+"
          )
        )
      } else {
        code
      }
    },
    character(1)
  )

  if (any(has_source_codes)) {
    frames <- lapply(index, function(i) {
      if (has_source_codes[[i]]) {
        frames[[i]]
      } else {
        df <- frames[[i]]
        df[["regicode"]] <- rep(codes[[i]], nrow(df))
        df[c("regicode", setdiff(names(df), "regicode"))]
      }
    })
  } else {
    merged <- dplyr::bind_rows(frames)
    merged[["regicode"]] <- rep(
      codes,
      times = vapply(frames, nrow, integer(1))
    )
    merged <- merged[c("regicode", setdiff(names(merged), "regicode"))]
    return(standardize_year_sex(merged))
  }

  standardize_year_sex(dplyr::bind_rows(frames))
}
