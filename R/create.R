#' Create age specific rate.
#'
#' @param x Object of data with class of 'fbswicd' or 'canreg'.
#' @param ... Variable name used for stratification.
#' @param event Event used for age specific rate.
#' @param rks Risk population.
#' @param agegrp Variable name indicate age groups.
#' @param type Data frame format long or wide.
#' @param mp Correction factor.
#' @param decimal Decimal places for rounding.

#'
#' @return Return a data frame of age specific rate.
#' @export
#'
create_age_rate <- function(x,
                       ...,
                       event = fbs,
                       rks = rks,
                       agegrp = agegrp,
                       type = "long",
                       mp = 100000,
                       decimal = 2) {
  fbs <- "fbs"
  UseMethod("create_age_rate", x)
}


#' Create age specific rate.
#'
#' @param x Data frame of class 'fbswicd'.
#' @param ... Variable name used for stratification.
#' @param event Event used for age specific rate.
#' @param rks Risk population.
#' @param agegrp Variable name indicate age groups.
#' @param type Data frame format long or wide.
#' @param mp Correction factor.
#' @param decimal Decimal places for rounding.
#'
#' @importFrom 'tidyr' 'pivot_wider'
#'
#' @return Return a data frame of age specific rate.
#' @export
#'
create_age_rate.fbswicd <- function(x,
                            ...,
                            event = fbs,
                            rks = rks,
                            agegrp = agegrp,
                            type = "long",
                            mp = 100000,
                            decimal = 2) {
  data_ <- x$fbswicd
  pop_raw <- x$pop
  fbs <- "fbs"

  # Deal with population data
  group_var <- enquos(...)
  group_vars <- purrr::keep(group_var, ~ quo_name(.x) %in% c("year", "sex"))

  pop_modi <- pop_raw %>%
    group_by(!!!group_vars, {{agegrp}}) %>%
    reframe(across(c("rks"), ~ sum(.x))) %>%
    ungroup()
  # Check if "year" and "sex" columns exist in pop_modi
  logi <- c("year", "sex") %in% colnames(pop_modi)
  by_vars <- c("year", "sex")[logi]
  
  
  output <- data_  %>%
    group_by(..., {{ agegrp }})  %>%
    reframe(across(c({{ event }}), ~ sum(.x)))  %>%
    left_join(pop_modi, by = c(by_vars, "agegrp")) %>%
    mutate(
      cases = {{ event }},
      rate = round(mp * {{ event }} / {{ rks }}, decimal)
    ) %>%
    select(-{{event}})
  
  if (type == "long") {
    return(output)
  } else if (type == "wide") {
    output <- output  %>%
      mutate(agegrp2= as.numeric({{agegrp}})) %>%
      select(-{{agegrp}}) %>% 
      rename(f = cases, r = rate, p = rks) %>%
      pivot_wider(
        names_from = agegrp2,
        names_sep = "",
        values_from = c("f", "p", "r"),
        values_fill = 0
      )
    return(output)
  }
}

#' Create quality index from object of class 'fbswicd'.
#'
#' @param data An object of class 'fbswicd'.
#' @param ... Variables used for stratification.
#' @param decimal Decimal digits.
#'
#' @return Data frame of class quality.
#' @export
#' 
#'
#' @examples
#' library(canregtools)
#' file <- system.file("extdata", "411721.xls", package = "canregtools")
#' data <- read_canreg(file)
#' fbsw <- count_canreg(data, cutage_method = "interval")
#' quality <- create_quality(fbsw, year, sex, icd_cat)
create_quality <- function(data, ..., decimal = 2) {
  prop1 <- function(x, fmu) {
    round(sum(x) / sum(fmu), decimal)
  }
  
  prop2 <- function(x, fmu) {
    round(sum(x) / sum(fmu) * 100, decimal)
  }
  data <- data$fbswicd
  output <- data %>%
    group_by(...) %>%
    reframe(
      across(
        starts_with(c("fbs", "sws", "mv", "dco", "ub", "sub","m8000")),
        ~ sum(.x)
      )
    ) %>%
    group_by(...) %>%
    reframe(
      across(starts_with(c("sws")), ~ prop1(.x, fbs), .names = "mi"),
      across(starts_with(c("mv", "ub", "sub", "dco","m8000")),
             ~ prop2(.x, fbs), .names = "{.col}")
      ) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x)))
  attr(output, "class") <- c("quality", "tbl_df", "tbl", "data.frame")
  return(output)
}

replace_na <- function(x) {
  ifelse(is.na(x), 0, x)
}


