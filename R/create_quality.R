#' Create quality index from object of class 'fbswicd'.
#'
#' @rdname create_quality
#' @param x An object of class 'fbswicd'.
#' @param ... Variables used for stratification.
#' @param cancer_type Method used to classify ICD10.
#' @param collapse Whether the output collapse to tibble.
#' @param decimal Decimal digits.
#'
#' @return A data frame of cancer registration quality data.
#' @export
#' 
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
  if (collapse){
    res <- cr_merge(res)
  } else {
    class(res) <- c("qualities", "list")
  }
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

