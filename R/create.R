#' Create age specific rate.
#'
#' @param data Data frame of class 'fbswicd'.
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
create_age_rate <- function(data,
                            ...,
                            event = fbs,
                            rks = rks,
                            agegrp = agegrp,
                            type = "long",
                            mp = 100000,
                            decimal = 2) {
  stopifnot("fbswicd" %in% class(data))
  fbs <- "fbs"
  output <- data |>
    group_by(..., {{ agegrp }}) |>
    reframe(across(c({{ event }}, {{ rks }}), ~ sum(.x))) |>
    mutate(
      cases = {{ event }},
      rate = round(mp * {{ event }} / {{ rks }}, decimal)
    ) |>
    select(-{{ event }}, -{{ rks }})
  if (type == "long") {
    return(output)
  } else if (type == "wide") {
    output <- output |>
      pivot_wider(
        names_from = {{ agegrp }},
        values_from = c("cases", "rate"),
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
#' @examples
#' library(canregtools)
#' file <- system.file("extdata", "411721.xls", package = "canregtools")
#' data <- read_canreg(file)
#' fbsw <- count_canreg(data, cutage_method = "interval")
#' quality <- create_quality(fbsw, year, sex, icd_cat)
create_quality <- function(data, ..., decimal = 2) {
  output <- data %>%
    group_by(...) %>%
    reframe(
      across(
        starts_with(c("fbs", "sws", "mv", "dco", "ub", "sub")),
        ~ sum(.x)
      )
    ) %>%
    group_by(...) %>%
    reframe(
      across(starts_with(c("sws")), ~ prop1(.x, fbs), .names = "mi"),
      across(starts_with(c("mv", "ub", "sub", "dco")), ~ prop2(.x, fbs),
        .names = "{.col}"
      )
    ) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x)))
  attr(output, "class") <- c("quality", "tbl_df", "tbl", "data.frame")
  return(output)
}

replace_na <- function(x) {
  ifelse(is.na(x), 0, x)
}

prop1 <- function(x, fmu) {
  round(sum(x) / sum(fmu), 2)
}

prop2 <- function(x, fmu) {
  round(sum(x) / sum(fmu) * 100, 2)
}
