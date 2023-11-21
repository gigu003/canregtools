#' Compute age standardized rate based on provided standard population
#' @description
#' This function compute the crude rate and age standard rates and their 95%
#' confidence intervals(CIs).
#'
#' @param data A data frame or tibble that containing case counts, population
#'        in risk, and stratification variables for calculating standardized
#'        rates.
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
#' @param method This parameter specifies the method used to calculate
#'        confidence intervals for the rates. The options are "gamma" (default),
#'        "normal", or "lognormal". Different methods may produce slightly
#'        different confidence intervals.
#' @param decimal This parameter specifies the number of decimal places to
#'        round the results. The default is 2, which means rates will be
#'        rounded to two decimal places.
#' @param show_var Logical value whether show variance or not.
#' @param type Character, regrouping type for cancers, options are 'system',
#'        'big', or 'small', default is 'big'.
#' @param lang Character, language for output, options are 'en' or 'cn'.
#'        Default is 'cn'.
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
#' @author [Qiong Chen](https://www.chenq.site)
#' @references
#' \insertRef{fay1997}{canregtools}
asr_rate <- function(
    data,
    ...,
    method = "gamma",
    event = fbs,
    pop = rks,
    agegrp = "agegrp",
    std = c("segi", "china"),
    mp = 100000,
    sig = 0.95,
    decimal = 2,
    show_var = FALSE,
    type = "big",
    lang = "cn") {
  groups <- quos(...)
  fbs <- "fbs"
  rks <- "rks"
  icd_cat <- "icd_cat"
  cr <- function(x, pop) {
    sum(x) / sum(pop)
  }
  crvar <- function(x, pop) {
    sum(x) / sum(pop)^2
  }
  asr <- function(x, event, pop) {
    sum(x / sum(x) * (event / pop))
  }
  asrvar <- function(x, event, pop) {
    sum(as.numeric(((x / sum(x))^2) * (event / (pop)^2)))
  }
  crlower <- function(x, pop) {
    if (method == "gamma") {
      qgamma((1 - sig) / 2, shape = cr(x, pop)^2 / crvar(x, pop)) /
        (cr(x, pop) / crvar(x, pop))
    } else if (method == "normal") {
      cr(x, pop) + qnorm((1 - sig) / 2) * sqrt(crvar(x, pop))
    } else if (method == "lognormal") {
      exp(log(cr(x, pop)) + qnorm((1 - sig) / 2) * sqrt(crvar(x, pop)) /
        cr(x, pop))
    }
  }
  crupper <- function(x, pop) {
    if (method == "gamma") {
      qgamma(1 - ((1 - sig) / 2),
        shape = 1 + cr(x, pop)^2 / crvar(x, pop)
      ) /
        (cr(x, pop) / crvar(x, pop))
    } else if (method == "normal") {
      cr(x, pop) - qnorm((1 - sig) / 2) * sqrt(crvar(x, pop))
    } else if (method == "lognormal") {
      exp(log(cr(x, pop)) - qnorm((1 - sig) / 2) * sqrt(crvar(x, pop)) /
        cr(x, pop))
    }
  }

  asrlower <- function(x, event, pop) {
    if (method == "gamma") {
      qgamma((1 - sig) / 2,
        shape = asr(x, event, pop)^2 / asrvar(x, event, pop)
      ) /
        (asr(x, event, pop) / asrvar(x, event, pop))
    } else if (method == "normal") {
      asr(x, event, pop) + qnorm((1 - sig) / 2) * sqrt(asrvar(x, event, pop))
    } else if (method == "lognormal") {
      exp(log(asr(x, event, pop)) +
        qnorm((1 - sig) / 2) * sqrt(asrvar(x, event, pop)) / asr(x, event, pop))
    }
  }
  asrupper <- function(x, event, pop) {
    if (method == "gamma") {
      qgamma(1 - ((1 - sig) / 2),
        shape = 1 + (asr(x, event, pop)^2 / asrvar(x, event, pop))
      ) /
        (asr(x, event, pop) / asrvar(x, event, pop))
    } else if (method == "normal") {
      asr(x, event, pop) - qnorm((1 - sig) / 2) * sqrt(asrvar(x, event, pop))
    } else if (method == "lognormal") {
      exp(log(asr(x, event, pop)) -
        qnorm((1 - sig) / 2) * sqrt(asrvar(x, event, pop)) / asr(x, event, pop))
    }
  }

  if ("canreg" %in% class(data)) {
    data <- count_canreg(data,
      type = type,
      lang = lang,
      sep_zero = FALSE
    )
    data <- data$fbswicd
  }

  levels(std_pop$agegrp) <- levels(data$agegrp)
  output <- data %>%
    # filter cases was exlcuded by 'classify_icd10'.
    filter(!(icd_cat %in% c("\u6392\u9664", "Excluded"))) %>%
    group_by(!!!groups, agegrp) %>%
    mutate(across(c({{ event }}, {{ pop }}), ~ sum(.x))) %>%
    distinct(!!!groups, .keep_all = TRUE) %>%
    left_join(std_pop, by = c("agegrp")) %>%
    group_by(!!!groups) %>%
    reframe(
      across({{ event }},
        list(
          no_cases = ~ sum(.x),
          cr = ~ cr(.x, {{ pop }}),
          cr_var = ~ crvar(.x, {{ pop }}),
          cr_lower = ~ crlower(.x, {{ pop }}),
          cr_upper = ~ crupper(.x, {{ pop }})
        ),
        .names = "{.fn}"
      ),
      across({{ std }},
        list(
          asr = ~ asr(.x, {{ event }}, {{ pop }}),
          asr_var = ~ asrvar(.x, {{ event }}, {{ pop }}),
          asr_lower = ~ asrlower(.x, {{ event }}, {{ pop }}),
          asr_upper = ~ asrupper(.x, {{ event }}, {{ pop }})
        ),
        .names = "{.fn}_{.col}"
      )
    ) %>%
    mutate(
      across(
        starts_with(c("asr", "cr")) & !contains("var"),
        ~ round(.x * mp, digits = decimal)
      ),
      across(where(is.numeric), ~ replace_na(.x))
    )

  if (!show_var) {
    output <- output %>%
      select(-contains(c("cr_var", "asr_var")))
  }

  attr(output, "class") <- c("asr", "tbl_df", "tbl", "data.frame")
  return(output)
}
