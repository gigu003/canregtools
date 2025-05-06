#' Calculate Age-Standardized Rate (ASR)
#'
#' @description `create_asr()` calculates age-standardized rates (ASRs) using
#'   PBCR data. It supports stratification by multiple variables, allows the
#'   specification of different standard population structures, and provides
#'   flexibility in the inclusion of variance, confidence intervals, and
#'   population data.
#'
#' @rdname create_asr
#' @inheritParams data
#' @inheritParams strat_vars
#' @inheritParams event
#' @param std Specify the standard population structure in the 'std_pop' data
#'   frame used for calculating standardized rates. When calculating
#'   standardized rates for multiple standard populations, specify std = c(segi,
#'   china).
#' @inheritParams cancer_type
#' @inheritParams mp_decimal
#' @param show_pop Logical value whether output population or not.
#' @param show_var Logical value whether output variance or not.
#' @param show_ci Logical value whether output confidence(lower or upper bound)
#'   or not.
#' @param collapse Logical value whether output result as asr or asrs. 
#'
#' @return A data frame or tibble contains the age standard rates and CIs.
#' @export
#'
#' @seealso \code{\link{ageadjust}} for age-adjusted rate calculations.
#'   \code{\link{truncrate}} for truncated rate calculations.
#' 
create_asr <- function(x,
                       ...,
                       event = "fbs",
                       std = c("cn2000", "wld85"),
                       cancer_type = "big",
                       mp = 100000,
                       decimal = 2,
                       show_var = FALSE,
                       show_ci = FALSE,
                       collapse = TRUE) {
  UseMethod("create_asr", x)
}

#' @rdname create_asr
#' @method create_asr canregs
#' @export
create_asr.canregs <- function(x, ..., cancer_type = "big", collapse = TRUE){
  data__ <- count_canreg(x, cancer_type = cancer_type)
  res <- create_asr.fbswicds(data__, ..., collapse = collapse)
  return(res)
}

#' @rdname create_asr
#' @method create_asr canreg
#'
#' @export
#' @examples
#' 
#' # calculate ASR based on object with class of `canreg`
#' data("canregs")
#' data <- canregs[[1]]
#' # calculate ASR using default parameter
#' asr <- create_asr(data, year, sex, cancer)
#' head(asr)
#' # calculate ASR using multiple standard population
#' asr_multi_std <- create_asr(data, year, sex, cancer,
#'                             std = c("cn82", "cn2000", "wld85"))
#' head(asr_multi_std)
#' # calculate ASR with confidence interval
#' asr_with_ci <- create_asr(data, year, sex, cancer, show_ci = TRUE)
#' head(asr_with_ci)
#' # calculate ASR with population at risk
#' asr_with_pop <- create_asr(data, year, sex, cancer, show_pop = TRUE)
#' head((asr_with_pop))
#' # calculate ASR with variance
#' asr_with_var <- create_asr(data, year, sex, cancer, show_var = TRUE)
#' head(asr_with_var)
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
                                event = "fbs",
                                std = c("cn2000", "wld85"),
                                mp = 100000,
                                decimal = 2,
                                show_pop = FALSE,
                                show_var = FALSE,
                                show_ci = FALSE,
                                collapse = TRUE) {
  event <- rlang::sym(event)
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
  if (collapse){res <- cr_merge(res)}
  return(res)
}

#' @rdname create_asr
#' @method create_asr fbswicd
#' @export
#' @examples
#' 
#' # calculate ASR based on object with class of `fbswicd`
#' # convert object with class of `canreg` to object with class of `fbswicd`
#' fbsw <- count_canreg(data)
#' asr <- create_asr(fbsw, event= "sws", year, sex, cancer)
#' 
create_asr.fbswicd <- function(x,
                               ...,
                               event = "fbs",
                               std = c("cn2000", "wld85"),
                               mp = 100000,
                               decimal = 2,
                               show_pop = FALSE,
                               show_var = FALSE,
                               show_ci = FALSE){

  if (!check_group_vars(x, ..., quiet = FALSE)){
    stop("group vars not supported.")
  }
  sex <- rlang::sym("sex")
  event <- rlang::sym(event)
  agegrp <- rlang::sym("agegrp")
  pop <- rlang::sym("rks")
  stdvar1 <- rlang::sym(std[1])
  std <- rlang::enquos(std)

  group_var <- rlang::enquos(...)
  gvars <- purrr::map_chr(group_var, rlang::as_name)
  sex_not_pass <- ifelse("sex" %nin% gvars, TRUE, FALSE)
  if ("sex" %nin% gvars) { gvars <- c(gvars, "sex") }
  gvars <- gvars |> setdiff("agegrp")
  group_var <- gvars |> rlang::syms()
  gvars2 <- setdiff(gvars, "cancer")
  group_var2 <- rlang::syms(gvars2)
  
  # Deal with population data
  pop_modi <- pre_deal_pop(x, ...)
  # Deal with combine sites.
  fbswicd <- pre_deal_fbswicd(x, ...)
  
  # Adjusting levels for age groups
  std_pop <- dict_maps[["std_pop"]]
  levels(std_pop$agegrp) <- levels(fbswicd$agegrp)
  
  # Performing grouping, summarization, and distinct operations for 'fbswicd'
  data_pre <- fbswicd |> 
    group_by(!!!group_var, !!agegrp) |> 
    reframe(across(c(!!event), ~ sum(.x)))  |> 
    left_join(pop_modi, by = c(gvars2, "agegrp"))  |> 
    left_join(std_pop, by = c("agegrp")) |> 
    filter(!is.na(!!agegrp))
  
  # calculate age standardized rate.
  res <- data_pre |> 
    group_by(!!!group_var) |> 
    reframe(across(!!event,
                   list(no_cases = ~ sum(.x),
                        pop = ~sum(!!pop)), .names = "{.fn}"),
            across(!!!std, list(truncr = ~ truncrate(!!event,
                                                     !!pop,
                                                     stdpop = .x,
                                                     mp = mp,
                                                     decimal = decimal)),
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
    select(c(gvars, starts_with(c("pop", "no_cases", "cr", "cr_var", "cr_lci",
                                  "cr_uci", "asr", "truncr", "cumur",
                                  "prop"))))

 
  
  # Remove columns of "population" if show_pop is set to FALSE
  if (!show_pop) {
    res <- select(res, -contains(c("pop")))
  }
  
  # Remove columns with "_var" if show_var is set to FALSE
  if (!show_var) {
    res <- select(res, -contains(c("cr_var", "asr_var")))
  }
  # Remove columns with "lower or upper" if show_ci is set to FALSE
  if (!show_ci) {
    res <- select(res, -contains(c("_lower", "_upper")))
  }
  
  # Deal with group vars
  res <- post_vars(res) |> 
    post_sex_specific_cancer() |> 
    arrange(!!!rlang::syms(c("year", "sex", "cancer")))
  # add rank
  if ("cancer" %in% gvars){
    rank <- rlang::sym("rank")
    rank <- filter(res, !!rlang::sym("cancer") %nin% c(60L, 61L)) |> 
      select(!!!group_var, !!rlang::sym("no_cases")) |> 
      group_by(!!!group_var2) |> 
      mutate(!!rank := dense_rank(desc(!!rlang::sym("no_cases")))) |> 
      select(-!!rlang::sym("no_cases"))
    res <- left_join(res, rank, by = gvars)
  }
  
  if(sex_not_pass) { res <- filter(res, !!sex == 0L) }
  
  # Adding class attributes to the result
  attr(res, "class") <- c("asr", "tbl_df", "tbl", "data.frame")
  
  return(res)
}
