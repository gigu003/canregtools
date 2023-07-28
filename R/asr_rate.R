#' Compute age standardized rate based on provided standard population
#' @description
#' This function compute the crude rate and age standard rates and their 95%
#' confidence intervals(CIs).  
#' @param data A data frame or tibble that containing case counts, population
#'        in risk, and stratification variables for calculating standardized
#'        rates.
#' @param std_pop A data frame or tibble that containing the standard
#'        population, the variable name for age group must be 'agegrp'.
#' @param event A variable within the input data that corresponds to the cases
#'        counts.
#' @param pop A variable within the input data that corresponds to the
#'        population in risk in each stratification.
#' @param agegrp A variable within the input data that corresponds to the age
#'        categories.
#' @param ... Variable or variables for stratification when compute
#'        age standard rates.
#' @param std Specify the standard population structure in the 'std_pop'
#'        data frame used for calculating standardized rates. When calculating
#'        standardized rates for multiple standard populations, specify 
#'        std = c("segi", "china").
#' @param mp A constant to multiply rates by (e.g. mp=1000 for rates per 1000).
#' @param sig The desired level of confidence in computing confidence intervals.
#'        The default is 0.95 for 95 percent CIs.
#'
#' @return A data frame or tibble contains the age standard rates and CIs.
#'        Variables included in the output data frame as follows.
#'  \describe{
#'   \item{...}{Variable or variables for stratification.}
#'   \item{no_cases}{Sum of cases in each stratification.}
#'   \item{cr}{Crude rate in each stratification.}
#'   \item{cr_var}{Variance of the crude rate.}
#'   \item{cr_lower}{Lower limit of the crude rate confidence interval.}
#'   \item{cr_upper}{Upper limit of the crude rate confidence interval.}
#'   \item{asr_china}{Age standardized rate using the china standard pop.
#'   The output variable name would be "asr_" followed by the name of the
#'   standard population, if other standard population was used.}
#'   \item{asr_var_china}{Variance of the age standard rate using china
#'   standard population.}
#'   \item{asr_lower_china}{Lower limit of the age standard rate confidence
#'   interval using china standard population.}
#'   \item{asr_upper_china}{Upper limit of the age standard rate confidence
#'   interval using china standard population.}
#'  }
#' 
#' @export
#' 
#' @import dplyr
#' @importFrom Rdpack reprompt
#' @author [Qiong Chen](https://www.chenq.site), 
#'        Email:[chenqiong\@hnccr.org.cn](mailto:chenqiong\@hnccr.org.cn)
#' @references
#' \insertRef{fay1997}{canregtools}
asr_rate <- function(
    data,
    std_pop,
    ...,
    event = "fbs",
    pop = "rks",
    agegrp = "agegrp",
    std = c("segi","china"),
    mp = 100000,
    sig = 0.95) {
  groups <- quos(...)
  output <- data %>%
    group_by(!!!groups, agegrp) %>%
    mutate(across(c({{event}}, {{pop}}), ~ sum(.x))) %>%
    distinct(!!!groups, .keep_all = TRUE) %>%
    left_join(std_pop, by = c("agegrp")) %>%
    group_by(!!!groups) %>%
    summarise(
      across({{event}},
             list(
               no_cases = ~ sum(.x),
               cr = ~ sum(.x) / sum({{pop}}),
               cr_var = ~ sum(.x) / sum({{pop}})^2,
               cr_lower = ~ qgamma((1 - sig) / 2, shape = (sum(.x) / sum({{pop}}))^
                                     2 / (sum(.x) / sum({{pop}})^2)) / ((sum(.x) / sum({{pop}})) / (sum(.x) / sum({{pop}})^
                                                                                              2)),
               cr_upper = ~ qgamma(1 - ((1 - sig) / 2), shape = 1 + (sum(.x) /
                                                                       sum({{pop}}))^2 / (sum(.x) / sum({{pop}})^2)) / ((sum(.x) / sum({{pop}})) / (sum(.x) /
                                                                                                                                          sum({{pop}})^2))
             ),
             .names = "{.fn}"),
      across({{ std }},
             list(
               asr = ~ sum(.x / sum(.x) * ({{event}} / {{pop}})),
               asr_var = ~ sum(as.numeric(((.x / sum(.x)) ^ 2) * ({{event}} / ({{pop}}) ^ 2))),
               asr_lower = ~ qgamma((1 - sig) / 2, shape = sum(.x / sum(.x) * ({{ event }} / {{pop}})) ^ 2 / (sum(as.numeric(((.x / sum(.x)) ^ 2) * ({{event}} / ({{pop}}) ^ 2))))) / (sum(.x / sum(.x) * ({{ event }} / {{pop}})) / sum(as.numeric(((.x / sum(.x)) ^ 2) * ({{event}} / ({{pop}}) ^ 2)))),
               asr_upper = ~ qgamma(1 - ((1 - sig) / 2), shape = 1 + sum(.x / sum(.x) * ({{ event }} / {{pop}})) ^ 2 / (sum(as.numeric(((.x / sum(.x)) ^ 2) * ({{ event }} / ({{pop}}) ^ 2))))) / (sum(.x / sum(.x) * ({{ event }} / {{pop}})) / sum(as.numeric(((.x / sum(.x)) ^ 2) * ({{ event }} / ({{pop}}) ^ 2))))),
             .names = "{.fn}_{.col}"),
      .groups = "keep") %>%
    mutate(across(starts_with(c("asr","cr"))&!contains("var"),
                  ~ round(.x*mp,digits=2)))
  return(output)
}