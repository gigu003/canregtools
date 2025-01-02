#' Create quality indicators
#'
#' @description
#' This function generates a data frame or a list of data frames containing the 
#' quality indicators for population-based cancer registries (PBCRs). These indicators 
#' include various metrics such as:
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
#'        quality indicator values. Defaults to 2.
#'
#' @return The function returns a data frame (if applied to a single registry
#'        object, 'canreg' or 'fbswicd') or a list of data frames (if applied
#'        to a grouped registry object, 'canregs' or 'fbswicds') with a class
#'        of either `'quality'` or `'qualities'`. Each output data frame
#'        contains the computed quality indicators for the registry or subgroup.
#'        
#' @export
#' 
#' @examples
#' data <- load_canreg()
#' qua <- create_quality(data, year, sex, cancer, cancer_type = "big")
#' head(qua)
#' fbsw <- count_canreg(data, cancer_type = "system")
#' qua2 <- create_quality(fbsw, year, sex, cancer)
#' head(qua2)
create_quality <- function(x, ..., decimal = 2) {
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
                                   cancer_type = "big") {
  data__ <- count_canreg(x, cancer_type = cancer_type)
  res <- create_quality.fbswicds(data__, ...)
  return(res)
}

#' @method create_quality fbswicds
#' @rdname create_quality
#' @export
create_quality.fbswicds <- function(x,
                                    ...,
                                    decimal = 2) {
  res <- purrr::map(x,
                    create_quality.fbswicd,
                    ...,
                    .progress = "Calculating quality indicies #")
  class(res) <- c("qualities", "list")
  return(res)
}

#' @method create_quality fbswicd
#' @rdname create_quality
#' @export
create_quality.fbswicd <- function(x, ..., decimal = 2) {
  pop_raw <- x$pop
  stats <- c("fbs", "sws", "mv", "dco", "ub", "sub", "m8000")
  s_vars1 <- rlang::syms(stats)
  s_vars2 <- s_vars1[3:length(s_vars1)]
  fbs <- rlang::sym("fbs")
  sws <- rlang::sym("sws")
  fbl <- rlang::sym("fbl")
  swl <- rlang::sym("swl")
  rks <- rlang::sym("rks")
  mi <- rlang::sym("mi")
  
  group_var <- rlang::enquos(...)
  gvars <- purrr::map_chr(group_var, rlang::as_name)
  gvars2 <- setdiff(gvars, "cancer")
  group_var2 <- rlang::syms(gvars2)
  
  # Deal with population data
  pop_modi <- pop_raw |> 
    group_by(!!!group_var2) |> 
    reframe(across(c(!!rks), ~ sum(.x)))  |>
    ungroup()
  
  data <- x$fbswicd
  if ("cancer" %nin% gvars){
    data <- data |> 
      filter(cancer == 60)
  }
  
  output <- data |> 
    group_by(...) |> 
    reframe(across(c(!!!s_vars1), ~ sum(.x))) |> 
    left_join(pop_modi, by = gvars2) |> 
    group_by(...) |> 
    reframe(
      !!fbl := prop(!!fbs, !!rks, mp=100000, decimal=decimal),
      !!swl := prop(!!sws, !!rks, mp=100000, decimal=decimal),
      !!rks := sum(!!rks),
      across(c(!!fbs, !!sws), ~sum(.x)),
      across(c(!!sws), ~ prop(.x, !!fbs, mp = 1, decimal=decimal), .names = "mi"),
      across(c(!!!s_vars2), ~ prop(.x, !!fbs, decimal=decimal), .names = "{.col}") ) |> 
    mutate(across(where(is.numeric), ~ replace_na(.x))) |> 
    select(!!!group_var, !!rks, !!fbs, !!fbl,!!sws,!!swl, !!mi, !!!s_vars2)
  attr(output, "class") <- c("quality", "tbl_df", "tbl", "data.frame")
  
  # Deal with group vars
  output <- output |> 
    post_vars()
  return(output)
}

prop <- function(x, fmu, mp = 100, decimal = 2) {
  round(sum(x) / sum(fmu) * mp, decimal)
}

