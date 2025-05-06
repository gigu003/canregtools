#' Create quality indicators
#'
#' @description This function generates a data frame or a list of data frames
#'   containing the quality indicators for population-based cancer registries
#'   (PBCRs). These indicators include various metrics such as:
#' - `fbs`: Number of incident cases.
#' - `fbl`: Incidence rate.
#' - `sws`: Number of death cases.
#' - `swl`: Mortality rate.
#' - `mv`: Percentage of cases with microscopic verification.
#' - `mi`: Mortality-to-incidence ratio.
#' - And other relevant quality metrics for cancer data evaluation.
#'
#' @rdname create_quality
#' @inheritParams data
#' @inheritParams strat_vars
#' @inheritParams cancer_type
#' @param decimal The number of decimal places to include in the resulting
#'   quality indicator values. Defaults to 2.
#' @param collapse Logical value whether output result as quality or qualites.
#'
#' @return The function returns a data frame (if applied to a single registry
#'   object, 'canreg' or 'fbswicd') or a list of data frames (if applied to a
#'   grouped registry object, 'canregs' or 'fbswicds') with a class of either
#'   `'quality'` or `'qualities'`. Each output data frame contains the computed
#'   quality indicators for the registry or subgroup.
#'
#' @export
#'
#' @examples
#' data("canregs")
#' data <- canregs[[1]]
#' qua <- create_quality(data, year, sex, cancer, cancer_type = "big")
#' head(qua)
#' fbsw <- count_canreg(data, cancer_type = "system")
#' qua2 <- create_quality(fbsw, year, sex, cancer)
#' head(qua2)
create_quality <- function(x, ..., decimal = 2, collapse = TRUE) {
  UseMethod("create_quality", x)
}

#' @rdname create_quality
#' @method create_quality canreg
#' @export
create_quality.canreg <- function(x,
                                  ...,
                                  cancer_type = "big"){
  data__ <- count_canreg(x, cancer_type = cancer_type)
  res <- create_quality.fbswicd(data__, ...)
  return(res)
}

#' @method create_quality canregs
#' @rdname create_quality
#' @export
create_quality.canregs <- function(x,
                                   ...,
                                   cancer_type = "big",
                                   collapse = TRUE) {
  data__ <- count_canreg(x, cancer_type = cancer_type)
  res <- create_quality.fbswicds(data__, ..., collapse = collapse)
  return(res)
}

#' @method create_quality fbswicds
#' @rdname create_quality
#' @export
create_quality.fbswicds <- function(x,
                                    ...,
                                    decimal = 2,
                                    collapse = TRUE) {
  res <- purrr::map(x,
                    create_quality.fbswicd,
                    ...,
                    .progress = "Calculating quality indicies #")
  class(res) <- c("qualities", "list")
  if (collapse){res <- cr_merge(res)}
  return(res)
}

#' @method create_quality fbswicd
#' @rdname create_quality
#' @export
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
  if ("sex" %nin% gvars) { gvars <- c(gvars, "sex") }
  gvars2 <- setdiff(gvars, "cancer")
  group_var2 <- rlang::syms(gvars2)
  
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
      across(c(!!fbs, !!sws), ~sum(.x)),
      across(c(!!sws), ~ prop(.x, !!fbs, mp = 1, decimal=decimal), .names = "mi"),
      across(c(!!!s_vars2), ~ prop(.x, !!fbs, decimal=decimal), .names = "{.col}") ) |> 
    mutate(across(where(is.numeric), ~ replace_na(.x))) |> 
    select(!!!gvars, !!popu, !!fbs, !!inci, !!sws, !!mort, !!mi, !!!s_vars2)
  attr(output, "class") <- c("quality", "tbl_df", "tbl", "data.frame")
  
  # Deal with group vars
  output <- post_vars(output) |> post_sex_specific_cancer()
  if (sex_not_pass) { output <- filter(output, !!sex == 0L) }
  output <- dplyr::arrange(output, !!!gvars)
  return(output)
}

prop <- function(x, fmu, mp = 100, decimal = 2) {
  round(sum(x) / sum(fmu) * mp, decimal)
}

