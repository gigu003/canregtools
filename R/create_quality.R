#' Calculate quality indicators
#'
#' `create_quality()`calculate quality indicators from object with class
#' of `canreg`, `canregs`, `fbswicd`, or `fbswicds`. The quality indicators for
#' population-based cancer registries (PBCRs) including:
#' - `fbs`: Number of incident cases.
#' - `inci`: Cancer incidence rate.
#' - `sws`: Number of death cases.
#' - `mort`: Mortality rate.
#' - `mv`: Percentage of cases with microscopic verification.
#' - `mi`: Mortality-to-incidence ratio.
#' - And other relevant quality metrics for cancer data evaluation.
#'
#' @rdname create_quality
#' @template data
#' @template strat_vars
#' @template cancer_type
#' @param decimal The number of decimal places to include in the resulting
#'   quality indicator values. Defaults to 2.
#' @param collapse Logical value whether output result as quality or qualites.
#'
#' @return A data frame (if applied to a single registry
#'   object, 'canreg' or 'fbswicd') or a list of data frames (if applied to a
#'   grouped registry object, 'canregs' or 'fbswicds') with a class of either
#'   `'quality'` or `'qualities'`.
#'
#' @export
#'
#' @examples
#' data("canregs")
#' fbsws <- count_canreg(canregs, cancer_type = "system")
#' qua2 <- create_quality(fbsws, year, sex, cancer)
#' head(qua2)
#'
create_quality <- function(x, ..., decimal = 2, collapse = TRUE) {
  UseMethod("create_quality", x)
}

#' @rdname create_quality
#' @method create_quality canreg
#' @export
#' @examples
#' # Calculate the quality indicators based on object with class of `canreg`
#' data <- canregs[[1]]
#' qua <- create_quality(data, year, sex, cancer, cancer_type = "big")
#' head(qua)
#'
create_quality.canreg <- function(x,
                                  ...,
                                  cancer_type = "big") {
  count_canreg(x, cancer_type = cancer_type) |>
    create_quality(...)
}

#' @method create_quality canregs
#' @rdname create_quality
#' @export
#' @examples
#' # Calculate the quality indicators based on object with class of `canregs`
#' qua <- create_quality(canregs, year, sex, cancer, cancer_type = "big")
#' head(qua)
#'
create_quality.canregs <- function(x,
                                   ...,
                                   cancer_type = "big",
                                   collapse = TRUE) {
  count_canreg(x, cancer_type = cancer_type) |>
    create_quality(..., collapse = collapse)
}

#' @method create_quality fbswicds
#' @rdname create_quality
#' @export
#' @examples
#' # Calculate the quality indicators based on object with class of `fbswicds`
#' fbsws <- count_canreg(canregs, cancer_type = "small")
#' qua <- create_quality(fbsws, year, sex, cancer)
#' head(qua)
#'
create_quality.fbswicds <- function(x,
                                    ...,
                                    decimal = 2,
                                    collapse = TRUE) {
  res <- purrr::map(x,
    create_quality.fbswicd,
    ...,
    .progress = "Calculating quality indicies #"
  )
  class(res) <- c("qualities", "list")
  if (collapse) {
    return(cr_merge(res))
  } else {
    res
  }
}

#' @method create_quality fbswicd
#' @rdname create_quality
#' @export
#' @examples
#' # Calculate the quality indicators based on object with class of `fbswicd`
#' fbsw <- count_canreg(canregs[[1]], cancer_type = "big")
#' qua <- create_quality(fbsw, year, sex, cancer)
#' head(qua)
#'
create_quality.fbswicd <- function(x, ..., decimal = 2) {
  sex <- rlang::sym("sex")
  stats <- c("fbs", "sws", "mv", "dco", "ub", "sub", "m8000")
  s_vars1 <- rlang::syms(stats)
  s_vars2 <- s_vars1[3:length(s_vars1)]
  fbs <- rlang::sym("fbs")
  sws <- rlang::sym("sws")
  inci <- rlang::sym("inci")
  mort <- rlang::sym("mort")
  popu <- rlang::sym("rks")
  mi <- rlang::sym("mi")

  gvars <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  sex_not_pass <- ifelse("sex" %nin% gvars, TRUE, FALSE)
  cancer_not_pass <- ifelse("cancer" %nin% gvars, TRUE, FALSE)
  if ("sex" %nin% gvars) {
    gvars <- c(gvars, "sex")
  }
  gvars2 <- setdiff(gvars, "cancer")

  # 预处理统计数据和人口数据
  pop_modi <- pre_deal_pop(x, ...)
  data <- pre_deal_fbswicd(x, ...)

  output <- data |>
    group_by(!!!rlang::syms(gvars), !!rlang::sym("agegrp")) |>
    reframe(across(c(!!!s_vars1), ~ sum(.x))) |>
    left_join(pop_modi, by = c(gvars2, "agegrp")) |>
    group_by(!!!rlang::syms(gvars)) |>
    reframe(
      !!inci := prop(!!fbs, !!popu, mp = 100000, decimal = decimal),
      !!mort := prop(!!sws, !!popu, mp = 100000, decimal = decimal),
      !!popu := sum(!!popu),
      across(c(!!fbs, !!sws), ~ sum(.x)),
      across(c(!!sws), ~ prop(.x, !!fbs, mp = 1, decimal = decimal),
             .names = "mi"),
      across(c(!!!s_vars2), ~ prop(.x, !!fbs, decimal = decimal),
             .names = "{.col}")
    ) |>
    mutate(across(where(is.numeric), ~ replace_na(.x))) |>
    select(!!!gvars, !!popu, !!fbs, !!inci, !!sws, !!mort, !!mi, !!!s_vars2)
  attr(output, "class") <- c("quality", "tbl_df", "tbl", "data.frame")
  
  
  if ("death" %in% names(pop_modi)) {
    death <- pop_modi |>
      group_by(!!!rlang::syms(gvars2)) |>
     reframe(death = sum(!!rlang::sym("death"), na.rm = TRUE) /
               sum(!!rlang::sym("rks"), na.rm = TRUE))
  }

  # Deal with group vars
  output <- post_vars(output) |> post_sex_specific_cancer()
  if (sex_not_pass) {
    output <- filter(output, !!sex == 0L)
  }
  
  if (cancer_not_pass & "death" %in% names(pop_modi)) {
    output <- output |>
      left_join(death, by = join_by(!!!rlang::syms(gvars2)))
  }
  
  dplyr::arrange(output, !!!rlang::syms(gvars))

}
