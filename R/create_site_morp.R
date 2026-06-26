#' Count ICD-10 site and morphology groups by strata
#'
#' `create_site_morp()` counts ICD-10 site codes or morphology groups within
#' user-defined strata. It can be applied to objects with class `canreg`,
#' `canregs`, `fbswicd`, or `fbswicds`.
#'
#' For `canreg` and `canregs` objects, the function first calls
#' [count_canreg()] to create `fbswicd`/`fbswicds` objects, and then counts the
#' `icd10` and/or `morp` frequency tables stored in the `sitemorp` element.
#'
#' @param x An object with class `canreg`, `canregs`, `fbswicd`, or `fbswicds`.
#' @param ... Variables used to define strata. For example, `year`, `sex`, and
#'   `cancer` will return frequencies within each combination of year, sex, and
#'   cancer group.
#' @param type Character. Type of frequency table to return when
#'   `flatten = TRUE`. Options are `"icd10"`, `"morp"`, and `"both"`.
#'   `"icd10"` returns ICD-10 site-code frequencies and is the default;
#'   `"morp"` returns morphology-code or morphology-class frequencies;
#'   `"both"` returns both results as a named list with elements `icd10` and
#'   `morp`.
#' @param flatten Logical. If `TRUE`, flatten the `icd10` or `morp` list-column
#'   frequency tables in a single `fbswicd` object into ordinary data frames
#'   with one row per stratum-code combination. If `FALSE`, keep the
#'   intermediate list-column structure with one row per stratum and two
#'   list-columns, `icd10` and `morp`.
#' @param collapse Logical. Only used for `canregs` and `fbswicds`. If `TRUE`,
#'   merge multiple registry objects before creating site/morphology frequency
#'   tables. If `FALSE`, return one result per registry object as a list.
#' @param wrap_subsite Logical. If `TRUE`, collapse ICD-10 subsite codes into
#'   two broad groups: codes ending with `.9` are coded as `"CXX.9"`; all other
#'   subsite codes are coded as `"CXX.0"`. Counts are then summed within these
#'   broad groups.
#' @param class_morp Logical. If `TRUE`, classify morphology codes with
#'   [classify_morp()] before summing counts. If `FALSE`, counts are returned
#'   by the original morphology code. When `cancer` is included in the
#'   stratification variables, its value is used as the `group` argument passed
#'   to [classify_morp()], currently `"60"` or `"125"`.
#' @param drop_nos Logical. Reserved for future use. Currently this argument is
#'   accepted but not used by the function.
#' @param cancer_type Character. Cancer classification type passed to
#'   [count_canreg()] when `x` is a `canreg` or `canregs` object.
#'
#' @returns
#' The return value depends on input class, `flatten`, `collapse`, and `type`:
#'
#' - For a single `fbswicd` or `canreg` object with `flatten = TRUE` and
#'   `type = "icd10"`, returns a data frame with the stratification variables,
#'   `icd10`, and `count`.
#' - With `flatten = TRUE` and `type = "morp"`, returns a data frame with the
#'   stratification variables, `morp`, and `count`.
#' - With `flatten = TRUE` and `type = "both"`, returns a named list containing
#'   `icd10` and `morp` data frames.
#' - With `flatten = FALSE`, returns a data frame with the stratification
#'   variables, `icd10`, and `morp`, where `icd10` and `morp` are list-columns
#'   of frequency tables.
#' - For `canregs` or `fbswicds`, `collapse = TRUE` returns a merged result;
#'   `collapse = FALSE` returns one result per registry object as a list.
#'
#' @examples
#' \dontrun{
#' data("canregs")
#'
#' create_site_morp(canregs, year, sex, cancer)
#' create_site_morp(canregs, year, sex, cancer, type = "morp")
#' create_site_morp(canregs, year, sex, cancer, type = "both")
#' create_site_morp(canregs, year, sex, cancer, collapse = FALSE)
#'
#' fbsw <- count_canreg(canregs[[1]])
#' create_site_morp(fbsw, year, sex, cancer, flatten = FALSE)
#' }
#' @export
create_site_morp <- function(x,
                             ...,
                             type = c("icd10", "morp", "both"),
                             flatten = TRUE,
                             collapse = TRUE,
                             wrap_subsite = FALSE,
                             class_morp = TRUE,
                             drop_nos = TRUE,
                             cancer_type = "big") {
  UseMethod("create_site_morp", x)
}

#' @rdname create_site_morp
#' @method create_site_morp canreg
#' @export
create_site_morp.canreg <- function(x,
                                    ...,
                                    type = c("icd10", "morp", "both"),
                                    flatten = TRUE,
                                    wrap_subsite = FALSE,
                                    class_morp = TRUE,
                                    drop_nos = TRUE,
                                    cancer_type = "big") {
  type <- match.arg(type)
  count_canreg(x, cancer_type = cancer_type) |>
    create_site_morp(
      ...,
      type = type,
      flatten = flatten,
      wrap_subsite = wrap_subsite,
      class_morp = class_morp,
      drop_nos = drop_nos
    )
}

#' @rdname create_site_morp
#' @method create_site_morp canregs
#' @export
create_site_morp.canregs <- function(x,
                                     ...,
                                     type = c("icd10", "morp", "both"),
                                     flatten = TRUE,
                                     collapse = TRUE,
                                     wrap_subsite = FALSE,
                                     class_morp = TRUE,
                                     drop_nos = TRUE,
                                     cancer_type = "big") {
  type <- match.arg(type)
  count_canreg(x, cancer_type = cancer_type) |>
    create_site_morp(
      ...,
      type = type,
      flatten = flatten,
      collapse = collapse,
      wrap_subsite = wrap_subsite,
      class_morp = class_morp,
      drop_nos = drop_nos
    )
}

#' @rdname create_site_morp
#' @method create_site_morp fbswicds
#' @export
create_site_morp.fbswicds <- function(x,
                                      ...,
                                      type = c("icd10", "morp", "both"),
                                      flatten = TRUE,
                                      collapse = TRUE,
                                      wrap_subsite = FALSE,
                                      class_morp = TRUE,
                                      drop_nos = TRUE) {
  type <- match.arg(type)
  res <- purrr::map(
    x,
    create_site_morp.fbswicd,
    ...,
    type = type,
    flatten = flatten,
    wrap_subsite = wrap_subsite,
    class_morp = class_morp,
    drop_nos = drop_nos,
    .progress = "Counting site and morphology #"
  )

  class(res) <- c("site_morps", "list")
  if (collapse) {
    return(cr_merge(res))
  } else {
    res
  }
}

#' @rdname create_site_morp
#' @method create_site_morp fbswicd
#' @export
create_site_morp.fbswicd <- function(x,
                                     ...,
                                     type = c("icd10", "morp", "both"),
                                     flatten = TRUE,
                                     wrap_subsite = FALSE,
                                     class_morp = TRUE,
                                     drop_nos = TRUE) {
  type <- match.arg(type)
  sitemorp <- purrr::pluck(x, "sitemorp")
  input_strat_vars <- get_expr_vars(...)
  strat_vars <- unique(c(input_strat_vars, "year", "sex", "cancer"))

  special_c <- c(1, 7, 12, 19, 20, 23, 33, 37, 52, 55, 56,
                   58, 61, 64, 65, 66, 73, 80, 97)
  special_d <- c(24, 27, 34, 45)
  special_icd10 <- c(sprintf("C%02d", special_c), sprintf("D%02d", special_d))

  normalise_icd10_code <- function(icd10) {
    icd10_3 <- substr(icd10, 1, 3)
    dplyr::case_when(
      icd10_3 %in% special_icd10 ~ icd10_3,
      grepl("^C[0-9]{2}$", icd10) ~ paste0(icd10, ".9"),
      TRUE ~ icd10
    )
  }

  summarise_tp_column <- function(data, group_vars, var) {
    data |>
      dplyr::group_by(!!!rlang::syms(group_vars)) |>
      dplyr::reframe(!!rlang::sym(var) := list(combine_tp(!!rlang::sym(var)))) |>
      dplyr::ungroup()
  }

  create_total_rows <- function(data, total_var, total_value) {
    data <- dplyr::filter(
      data,
      as.character(!!rlang::sym(total_var)) != as.character(total_value)
    )
    group_vars <- dplyr::setdiff(strat_vars, total_var)
    icd10_total <- summarise_tp_column(data, group_vars, "icd10")
    morp_total <- summarise_tp_column(data, group_vars, "morp")

    dplyr::left_join(icd10_total, morp_total, by = group_vars) |>
      dplyr::mutate(!!rlang::sym(total_var) := total_value) |>
      dplyr::select(!!!rlang::syms(c(strat_vars, "icd10", "morp")))
  }

  if (!"year" %in% input_strat_vars) {
    sitemorp <- dplyr::bind_rows(
      sitemorp,
      create_total_rows(sitemorp, "year", 9000L)
    )
  }

  sitemorp <- dplyr::bind_rows(
    sitemorp,
    create_total_rows(sitemorp, "sex", 0L)
  )

  sitemorp <- dplyr::bind_rows(
    sitemorp,
    create_total_rows(sitemorp, "cancer", "60")
  )

  get_morp_group <- function() {
    cancer_value <- as.character(dplyr::cur_group()[["cancer"]])

    if (length(cancer_value) == 0 || is.na(cancer_value)) {
      return("60")
    }

    if (cancer_value %in% c("60", "125")) {
      cancer_value
    } else {
      "60"
    }
  }

  is_three_digit_icd10_group <- function() {
    three_digit_icd10_groups <- c(
      "126", "101", "124", "125", "122", "105",
      "120", "107", "116", "110", "111", "112",
      "60"
    )
    cancer_value <- as.character(dplyr::cur_group()[["cancer"]])
    length(cancer_value) > 0 &&
      !is.na(cancer_value) &&
      cancer_value %in% three_digit_icd10_groups
  }

  res <- sitemorp |>
    group_by(!!!rlang::syms(strat_vars)) |>
    reframe(
      across(all_of(c("icd10", "morp")), function(x) {
        if (cur_column() == "morp") {
          res <- combine_tp(x)
          res <- tibble(
            !!sym("morp") := names(res),
            count = unname(res)
          )
          if (class_morp) {
            morp_group <- get_morp_group()
            res <- dplyr::mutate(
              res,
              !!sym("morp") := classify_morp(!!sym("morp"), group = morp_group)
            ) |>
              group_by(!!sym("morp")) |>
              summarise(count = sum(count), .groups = "drop")
          }
          list(res)
        } else {
          res <- combine_tp(x)
          res <- tibble(
            !!sym("icd10") := names(res),
            count = unname(res))
          res <- dplyr::mutate(
            res,
            !!sym("icd10") := normalise_icd10_code(!!sym("icd10"))
          ) |>
            group_by(!!sym("icd10")) |>
            summarise(count = sum(count), .groups = "drop")
          if (is_three_digit_icd10_group()) {
            res <- dplyr::mutate(
              res,
              !!sym("icd10") := substr(!!sym("icd10"), 1, 3)
            ) |>
              group_by(!!sym("icd10")) |>
              summarise(count = sum(count), .groups = "drop")
          }
          if (wrap_subsite) {
            res <- dplyr::mutate(
              res,
              !!sym("icd10") := ifelse(
                grepl("\\.9", !!sym("icd10")),
                "CXX.9",
                "CXX.0"
              )
            ) |>
              group_by(!!sym("icd10")) |>
              summarise(count = sum(count), .groups = "drop")
          }
          list(res)
        }
      })
    )

  if (!"year" %in% input_strat_vars) {
    res <- dplyr::filter(res, !!rlang::sym("year") == 9000L)
  }

  if (!"sex" %in% input_strat_vars) {
    res <- dplyr::filter(res, !!rlang::sym("sex") == 0L)
  }

  if (!"cancer" %in% input_strat_vars) {
    res <- dplyr::filter(res, !!rlang::sym("cancer") == "60")
  }

  if (!flatten) {
    return(res)
  }

  collapse_site_morp <- function(data, var) {
    purrr::map_dfr(seq_len(nrow(data)), function(i) {
      counts <- data[[var]][[i]]

      if (nrow(counts) == 0) {
        return(counts)
      }

      strata <- data[i, strat_vars, drop = FALSE]
      strata <- strata[rep(1, nrow(counts)), , drop = FALSE]
      dplyr::bind_cols(strata, counts)
    })
  }

  if (type == "both") {
    return(list(
      icd10 = collapse_site_morp(res, "icd10"),
      morp = collapse_site_morp(res, "morp")
    ))
  }

  collapse_site_morp(res, type)
}
