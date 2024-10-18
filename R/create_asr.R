#' Calculate age-standardized rate.
#'
#' @rdname create_asr
#' @param x data with class of 'fbswicd' or 'canreg'.
#' @param ... Variable names used to stratification.
#' @param event A variable within the input data that corresponds to the cases
#'              counts.
#' @param std Specify the standard population structure in the 'std_pop'
#'            data frame used for calculating standardized rates. When
#'            calculating standardized rates for multiple standard populations, 
#'            specify std = c(segi, china).
#' @param mp A constant to multiply rates by (e.g. mp=1000 for rates per 1000).
#' @param decimal This parameter specifies the number of decimal places to
#'                round the results. The default is 2, which means rates will
#'                be rounded to two decimal places.
#' @param show_pop Logical value whether output population or not.
#' @param show_var Logical value whether output variance or not.
#' @param show_ci Logical value whether output confidence(lower or upper
#'                bound) or not.
#' 
#'
#' @return A data frame or tibble contains the age standard rates and CIs.
#' @export
#'
create_asr <- function(x,
                       ...,
                       event = fbs,
                       std = c("cn2000", "wld85"),
                       mp = 100000,
                       decimal = 2,
                       show_var = FALSE,
                       show_ci = FALSE) {
  UseMethod("create_asr", x)
}

#' @rdname create_asr
#' @method create_asr canregs
#' @param cancer_type Classification type of cancer site, options are "big", "small"
#'              or "system", default is "big". 
#' @export
#'
create_asr.canregs <- function(x, ..., cancer_type = "big"){
  data__ <- count_canreg(x, cancer_type = cancer_type)
  res <- create_asr.fbswicds(data__, ...)
  return(res)
}

#' @rdname create_asr
#' @method create_asr canreg
#' @param cancer_type Classification type of cancer site, options are "big", "small"
#'              or "system", default is "big". 
#'
#' @export
#'
create_asr.canreg <- function(x, ..., cancer_type = "big"){
  data__ <- count_canreg(x, cancer_type = cancer_type)
  res <- create_asr.fbswicd(data__, ...)
  return(res)
}

#' @rdname create_asr
#' @method create_asr fbswicds
#' @export
#'
create_asr.fbswicds <- function(x,
                                ...,
                                event = fbs,
                                std = c("cn2000", "wld85"),
                                mp = 100000,
                                decimal = 6,
                                show_pop = FALSE,
                                show_var = FALSE,
                                show_ci = FALSE) {
  event <- rlang::enquo(event)
  group_var <- rlang::enquos(...)
  res <- purrr::map(x,
                    create_asr.fbswicd,
                    !!!group_var,
                    event = !!event,
                    std = std,
                    mp = mp,
                    decimal = decimal,
                    show_pop = show_pop,
                    show_var = show_var,
                    show_ci = show_ci,
                    .progress = "Calculating asr #")
  class(res) <- c("asrs", "list")
  return(res)
}



#' @rdname create_asr
#' @method create_asr fbswicd
#' @export
#'
create_asr.fbswicd <- function(x,
                               ...,
                               event = fbs,
                               std = c("cn2000", "wld85"),
                               mp = 100000,
                               decimal = 6,
                               show_pop = FALSE,
                               show_var = FALSE,
                               show_ci = FALSE){
  if (!check_group_vars(x, ...,quiet = FALSE)){
    stop("group vars not supported.")
  }
  event <- rlang::enquo(event)
  agegrp <- rlang::sym("agegrp")
  pop <- rlang::sym("rks")
  stdvar1 <- rlang::sym(std[1])
  std <- rlang::enquos(std)


  group_var <- rlang::enquos(...)
  gvars <- purrr::map_chr(group_var, rlang::as_name) |> 
    setdiff("agegrp")
  group_var <- gvars |> rlang::syms()
  gvars2 <- setdiff(gvars, "cancer")
  group_var2 <- rlang::syms(gvars2)
  
  # Deal with population data
  pop_modi <- pre_deal_pop(x, ...)
  # Deal with combine sites.
  fbswicd <- pre_deal_fbswicd(x, ...)
  
  # Adjusting levels for age groups
  levels(std_pop$agegrp) <- levels(fbswicd$agegrp)
  
  # Performing grouping, summarization, and distinct operations for 'fbswicd'
  data_pre <- fbswicd |> 
    group_by(!!!group_var, !!agegrp) |> 
    reframe(across(c(!!event), ~ sum(.x)))  |> 
    left_join(pop_modi, by = c(gvars2, "agegrp"))  |> 
    left_join(std_pop, by = c("agegrp")) |> 
    filter(!is.na(!!agegrp))
  
  # calculate age standardized rate.
  res <- data_pre %>%
    group_by(!!!group_var) %>%
    reframe(across(!!event,
                   list(no_cases = ~ sum(.x),
                        pop = ~sum(!!pop)), .names = "{.fn}"),
            across(!!!std,
                   list(truncr = ~ truncrate(!!event, !!pop, stdpop = .x,
                                             mp = mp, decimal = decimal)),
                   .names = "{.fn}_{.col}"),
            across(!!!std,
                   list(model = ~ list(ageadjust(!!event, !!pop, stdpop = .x,
                                                 mp = mp))), .names = "{.col}"),
            cumur = cumrate(!!event, !!pop, mp = 100)[[1]]) |> 
    rowwise() |>
    mutate(across(!!!std,
                  list(
                    asr = ~ .x[["asr"]],
                    asr_var = ~ .x[["asr_var"]],
                    asr_lower = ~  .x[["asr_lci"]],
                    asr_upper = ~  .x[["asr_uci"]]), .names = "{.fn}_{.col}" ),
           across(!!stdvar1,
                  list(cr = ~ .x[["cr"]],
                       cr_var = ~.x[["cr_var"]],
                       cr_lower = ~.x[["cr_lci"]],
                       cr_upper = ~.x[["cr_uci"]]), .names = "{.fn}")) |> 
    group_by(!!!group_var2) |> 
    mutate(
      across(contains("no_cases"),
             list(prop = ~ .x / max(.x) * 100), .names = "{.fn}"),
      across(
        starts_with(c("asr", "cr", "cumur", "truncr", "prop")) & !contains("var"),
        ~ round(.x, digits = decimal)),
      across(where(is.numeric), ~ replace_na(.x))) |> 
    select(c(gvars, starts_with(c("no_cases","pop","cr","cr_var","cr_lci","cr_uci",
                                  "asr","truncr","cumur","prop"))))
  # Remove columns of "population" if show_pop is set to FALSE
  if (!show_pop) {
    res <- res %>%
      select(-contains(c("pop")))
  }
  
  # Remove columns with "_var" if show_var is set to FALSE
  if (!show_var) {
    res <- res %>%
      select(-contains(c("cr_var", "asr_var")))
  }
  # Remove columns with "lower or upper" if show_ci is set to FALSE
  if (!show_ci) {
    res <- res %>%
      select(-contains(c("_lower", "_upper")))
  }
  
  # Deal with group vars
  res <- res |> 
    post_vars()
  
  # Adding class attributes to the result
  attr(res, "class") <- c("asr", "tbl_df", "tbl", "data.frame")
  
  # Returning the result
  return(res)
}


