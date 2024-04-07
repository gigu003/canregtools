#' Calculate age-standardized rate.
#'
#' @param x data with class of fbswicd.
#' @param ... Stratification factors.
#' @param event A variable within the input data that corresponds to the cases
#'              counts.
#' @param pop A variable within the input data that corresponds to the
#'            population in risk in each stratification.
#' @param agegrp A variable within the input data that corresponds to the age
#'              categories.
#' @param std Specify the standard population structure in the 'std_pop'
#'            data frame used for calculating standardized rates. When
#'            calculating standardized rates for multiple standard populations, 
#'            specify std = c(segi, china).
#' @param mp A constant to multiply rates by (e.g. mp=1000 for rates per 1000).
#' @param decimal This parameter specifies the number of decimal places to
#'                round the results. The default is 2, which means rates will
#'                be rounded to two decimal places.
#' @param show_var Logical value whether output variance or not.
#' @param show_ci Logical value whether output confidence(lower or upper
#'                bound) or not.
#' @param keep_sex_specific Deal with sex specific cases.

#'
#' @return class of asr.
#' @export
#'
create_asr <- function(x,
                       ...,
                       event = fbs,
                       pop = rks,
                       agegrp = agegrp,
                       std = c(china, segi),
                       mp = 100000,
                       decimal = 2,
                       show_var = FALSE,
                       show_ci = FALSE,
                       keep_sex_specific = FALSE) {
  fbs <- "fbs"
  rks <- "rks"
  agegrp <- "agegrp"
  china <- "china"
  segi <- "segi"
  UseMethod("create_asr", x)
}

#' Calculate age-standardized rate.
#'
#' @param x Object with class of canreg.
#' @param ... Parameters.
#' @param type Classification type of cancer site, options are "big", "small"
#'              or "system", default is "big". 
#' @param lang Language of the output result, options are "cn" for chinese, and
#'              "en" for english, default is "cn".
#'
#' @return class of asr.
#' @export
#'
create_asr.canreg <- function(x, ..., type= "big", lang="cn"){
  data__ <- count_canreg(x, type = type, lang = lang)
  res <- create_asr.fbswicd(data__, ...)
  return(res)
}


#' Calculate age-standardized rate.
#'
#' @param x data with class of fbswicd.
#' @param ... Stratification factors.
#' @param event A variable within the input data that corresponds to the cases
#'        counts.
#' @param pop A variable within the input data that corresponds to the
#'        population in risk in each stratification.
#' @param agegrp A variable within the input data that corresponds to the age
#'        categories.
#' @param std Specify the standard population structure in the 'std_pop'
#'        data frame used for calculating standardized rates. When calculating
#'        standardized rates for multiple standard populations, specify
#'        std = c("segi", "china").
#' @param mp A constant to multiply rates by (e.g. mp=1000 for rates per 1000).
#' @param decimal This parameter specifies the number of decimal places to
#'        round the results. The default is 2, which means rates will be
#'        rounded to two decimal places.
#' @param show_var Logical value whether output variance or not.
#' @param show_ci  Logical value whether output confidence(lower or upper
#'                  bound) or not.
#' @param keep_sex_specific deal with sex specific disease.
#'
#' @return A data frame or tibble contains the age standard rates and CIs.
#' @export
#'
create_asr.fbswicd <- function(x,
                       ...,
                       event = fbs,
                       pop = rks,
                       agegrp = agegrp,
                       std = c(china, segi),
                       mp = 100000,
                       decimal = 2,
                       show_var = FALSE,
                       show_ci = FALSE,
                       keep_sex_specific = FALSE
                       ){
  
  # Extracting the fbswicd and pop columns from the input data
  fbswicd <- x$fbswicd
  pop_raw <- x$pop
  fbs <- "fbs"
  rks <- "rks"
  china <- "china"
  segi <- "segi"
 
  sum_by_vars <- c("areacode", "registry", "province",
                   "city", "area_type", "region")
  byvars1 <- c("areacode", "registry", "province",
                "city", "area_type", "region",
               "year", "sex", "icd_cat")
  byvars2 <- byvars1[!byvars1=="icd_cat"]
  
  # Deal with population data
  group_var <- enquos(...)
  group_vars <- purrr::keep(group_var, ~ quo_name(.x) %in% byvars2)
  pop_modi <- pop_raw %>%
    group_by(!!!group_vars, {{agegrp}}) %>%
    reframe(across(c("rks"), ~ sum(.x))) %>%
    ungroup()
  
  # Check if "year" and "sex" columns exist in pop_modi
  by_vars_pop <- byvars2[byvars2 %in% colnames(pop_modi)]
  
  # Adjusting levels for age groups
  levels(std_pop$agegrp) <- levels(fbswicd$agegrp)
  
  # Performing grouping, summarization, and distinct operations for fbswicd
  data_pre <- fbswicd %>%
    group_by(..., {{agegrp}}) %>%
    reframe(across(c({{event}}), ~ sum(.x))) %>%
    left_join(pop_modi, by = c(by_vars_pop, "agegrp")) %>%
    left_join(std_pop, by = c("agegrp"))
  
  # calculate cumulative rate
  cumulative_rate <- data_pre %>%
    filter(as.numeric({{agegrp}}) < 16) %>%
    group_by(across(c(...))) %>%
    reframe(
      across({{event}},
             list(cumur = ~sum(.x * 5 / {{pop}})*100),
             .names = "{.fn}")
      )
  
  # calculate truncated age rate.
  truncate_rate <- data_pre %>%
    filter(as.numeric({{agegrp}})>7 & as.numeric({{agegrp}})<14) %>%
    group_by(across(c(...))) %>%
    reframe(
      across({{std}},
             list(truncr = ~sum({{event}} / {{pop}} * .x) / sum(.x) * mp),
             .names = "{.fn}_{.col}"
             )
    )
  
  # calculate age standardized rate.
  asr <- data_pre %>%
    group_by(across(c(...))) %>%
    reframe(
      across({{event}},
             list(
               no_cases = ~ sum(.x),
               pop = ~sum({{pop}}),
               cr = ~ ageadjust(.x, {{pop}}, mp=mp)$cr,
               cr_var = ~ ageadjust(.x, {{pop}}, mp=mp)$cr_var,
               cr_lower = ~ ageadjust(.x, {{pop}}, mp=mp)$cr_lci,
               cr_upper = ~ ageadjust(.x, {{pop}}, mp=mp)$cr_uci
             ), .names = "{.fn}"),
      across({{std}},
             list(
               asr = ~ ageadjust({{event}}, {{pop}}, stdpop = .x, mp=mp)$asr,
               asr_var = ~ ageadjust({{event}}, {{pop}}, stdpop = .x, mp=mp)$asr_var,
               asr_lower = ~ ageadjust({{event}}, {{pop}}, stdpop = .x, mp=mp)$asr_lci,
               asr_upper = ~ ageadjust({{event}}, {{pop}}, stdpop = .x, mp=mp)$asr_uci
             ), .names = "{.fn}_{.col}")
    )
  
  
  # Judge by vars in the result.
  by_vars_all <- byvars1[byvars1 %in% colnames(asr)]
  
  res <- asr %>%
    left_join(cumulative_rate, by = c(by_vars_all)) %>%
    left_join(truncate_rate, by = c(by_vars_all)) %>%
    group_by(!!!group_vars) %>%
    mutate(
      across(contains("no_cases"),
             list(prop = ~ .x*100/sum(.x)), .names = "{.fn}"),
      across(
        starts_with(c("asr", "cr", "cumur", "truncr", "prop")) & !contains("var"),
        ~ round(.x, digits = decimal)),
      across(where(is.numeric), ~ replace_na(.x)))

  
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
  sex <- "sex"
  icd_cat <- "icd_cat"
  if (keep_sex_specific & all(c("sex", "icd_cat") %in% colnames(res))){
    res <- res %>%
      filter(
        as.numeric(sex) == 2 & as.numeric(icd_cat) %in% c(15:18, 30:37) |
          as.numeric(sex) == 1 & as.numeric(icd_cat) %in% c(19:20, 38:41)
        ) %>%
      select(-sex)
      } else if (keep_sex_specific & c("icd_cat") %in% colnames(res) &
                 !c("sex") %in% colnames(res)) {
        res <- res %>% filter(!as.numeric(icd_cat) %in% c(15:20, 30:41))
  }
  
  # Adding class attributes to the result
  attr(res, "class") <- c("asr", "tbl_df", "tbl", "data.frame")
  
  # Returning the result
  return(res)
}
