#' count canreg data.
#'
#' @param x Object data of class 'canreg' or 'canregs'.
#' @param cutage_method Methods for Specifying Age Groups. Options are
#'        "interval", "distance", or "quantile". Default is "distance".
#' @param smooth_age Methods for expanding age groups from 5 years to 1 year.
#' @param breaks Specicify the break points classify age groups when
#'        cutage_method is 'interval'. Default is 'c(0, 15, 40, 65)'.
#' @param length Specify the length of each age group when cutage_method is
#'        'distance'. Default is 5.
#' @param maxage Specify the max age of age group when cutage_method is
#'        'distance'. Default is 85.
#' @param sep_zero Logical value, TRUE or FALSE, specifying whether to treat age
#'        0 as a separate group. Default is TRUE.
#' @param labels Labels for age groups. Default is NULL.
#' @param label_tail Tail label to be added to the labels. Default is NULL.
#' @param type Method for classify cancer sites.
#' @param lang Character value for specify the language used. options are 'cn'
#'        for Chinese, or 'en' for English.
#'
#' @return data of class fbswicd.
#' @export
#'
count_canreg <- function(x,
                         cutage_method = "distance",
                         smooth_age = "natural",
                         breaks = c(0, 15, 40, 65),
                         length = 5,
                         maxage = 85,
                         sep_zero = TRUE,
                         labels = NULL,
                         label_tail = NULL,
                         type = "big",
                         lang = "cn") {
  UseMethod("count_canreg", x)
}

#' count the canreg data for class canregs.
#'
#' @param x data of class canregs.
#' @param ... Others options.
#'
#' @return An object of class fbswicds.
#' @export
#'
count_canreg.canregs <- function(x, ...) {
  res <- lapply(x, count_canreg.canreg, ...)
  class(res) <- c("fbswicds", "list")
  return(res)
}

#' Count the canreg data.
#'
#' @param x Object data of class 'canreg'.
#' @param cutage_method Methods for Specifying Age Groups. Options are
#'        "interval", "distance", or "quantile". Default is "distance".
#' @param smooth_age Methods for expanding age groups from 5 years to 1 year.
#' @param breaks Specicify the break points classify age groups when
#'        cutage_method is 'interval'. Default is 'c(0, 15, 40, 65)'.
#' @param length Specify the length of each age group when cutage_method is
#'        'distance'. Default is 5.
#' @param maxage Specify the max age of age group when cutage_method is
#'        'distance'. Default is 85.
#' @param sep_zero Logical value, TRUE or FALSE, specifying whether to treat age
#'        0 as a separate group. Default is TRUE.
#' @param labels Labels for age groups. Default is NULL.
#' @param label_tail Tail label to be added to the labels. Default is NULL.
#' @param type Method for classify cancer sites.
#' @param lang Character value for specify the language used. options are 'cn'
#'        for Chinese, or 'en' for English.
#'
#'
#' @return An object of 'fbswicd'.
#' @export
#'
#' @examples
#' library(canregtools)
#' file <- system.file("extdata", "411721.xls", package = "canregtools")
#' data <- read_canreg(file)
#' fbsw <- count_canreg(data, cutage_method = "interval")
#' fbsw
count_canreg.canreg <- function(x,
                         cutage_method = "distance",
                         smooth_age = "natural",
                         breaks = c(0, 15, 40, 65),
                         length = 5,
                         maxage = 85,
                         sep_zero = TRUE,
                         labels = NULL,
                         label_tail = NULL,
                         type = "big",
                         lang = "cn") {
  year <- "year"
  sex <- "sex"
  agegrp <- "agegrp"
  icd_cat <- "icd_cat"
  basi <- "basi"
  icd10 <- "icd10"
  morp <- "morp"

  reg_count <- function(data, varname = "fbs") {
    bind_rows(
      data %>%
        count(year, sex, agegrp, icd_cat, name = varname, .drop = FALSE)
    )
  }
  fb <- clean_canreg(x$FBcases,
    cutage_method = cutage_method,
    breaks = breaks,
    length = length,
    maxage = maxage,
    sep_zero = sep_zero,
    labels = labels,
    label_tail = label_tail,
    type = type,
    lang = lang
  )
  sw <- clean_canreg(x$SWcases,
    cutage_method = cutage_method,
    breaks = breaks,
    length = length,
    maxage = maxage,
    sep_zero = sep_zero,
    labels = labels,
    label_tail = label_tail,
    type = type,
    lang = lang
  )
  pop <- clean_canreg(x$POP,
    cutage_method = cutage_method,
    breaks = breaks,
    length = length,
    maxage = maxage,
    sep_zero = sep_zero,
    labels = labels,
    label_tail = label_tail,
    lang = lang
  )
  ubs <- c("C26", "C39", "C48", "C76", "C77", "C78", "C79", "C80", "C97")
  res <- list(
    reg_count(fb, varname = "fbs"),
    reg_count(sw, varname = "sws"),
    fb %>% filter(basi %in% c(5, 6, 7)) %>% reg_count(varname = "mv"),
    fb %>% filter(substr(icd10, 1, 3) %in% ubs) %>% reg_count(varname = "ub"),
    fb %>% filter(substr(icd10, 5, 5) == "9") %>% reg_count(varname = "sub"),
    fb %>% filter(morp %in% c("8000","8001")) %>% reg_count(varname ="m8000"),
    fb %>% filter(basi == 0) %>% reg_count(varname = "dco")
  ) %>%
    purrr::reduce(left_join, by = c("year", "sex", "agegrp", "icd_cat")) %>%
    left_join(pop, by = c("year", "sex", "agegrp")) %>%
    arrange(year, sex, icd_cat, agegrp) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    filter(!(icd_cat %in% c("\u6392\u9664", "Excluded")))
  
  result <- list(areacode = x$areacode,
                 county = x$county,
                 area_type = x$area_type,
                 location = x$location,
                 year = x$year,
                 fbswicd = res,
                 pop = pop)
  attr(result, "class") <- c("fbswicd", class(result))
  return(result)
}

#' Clean canreg data.
#'
#' @param x Data of class 'canreg'.
#' @param cutage_method Method for grouping ages. Options are 'distance',
#'        'interval', or 'quarter'. Default is 'distance'.
#' @param smooth_age Method for expanding age. Options are 'linear','natural',
#'        or 'periodic'. Default is 'natural'.
#' @param breaks Cut points for age groups when 'cutage_method' is 'interval'.
#'        Default is 'c(0, 15, 40, 65)'.
#' @param length Length of each age group when 'cutage_method' is 'distance'.
#'        Default is 5.
#' @param maxage Start age of the last age groups. Default is 85.
#' @param sep_zero Logical value TRUE or FALSE, if the 0-year age group treated
#'        as a separate group. Default is TRUE.
#' @param labels Labels for the age groups.
#' @param label_tail Tail of the labels.
#' @param type Specify the grouping method for tumors. Options are 'system',
#'        'big', or 'small'. Default is 'big'.
#' @param lang Specify the language for the output. Options are 'en' or 'cn'.
#'        Default is 'cn'.
#'
#' @return Class 'canreg'.
#' @export
#'

clean_canreg <- function(x,
                         cutage_method = "distance",
                         smooth_age = "natural",
                         breaks = c(0, 15, 40, 65),
                         length = 5,
                         maxage = 85,
                         sep_zero = TRUE,
                         labels = NULL,
                         label_tail = NULL,
                         type = "big",
                         lang = "cn") {
  rks <- "rks"
  dclass <- class(x)

  dclass <- dclass[dclass %in% c("FBcases", "SWcases", "population")]
  switch(dclass,
    FBcases = {
      #cat("Processing data: ", length(x$sex), " FBcases.\n")
      year <- as.numeric(format(x$inciden, "%Y"))
      sex <- tidy_sex(x$sex, lang = lang)
      age <- calc_age(x$birthda, x$inciden)
      agegrp <- cutage(age,
        method = cutage_method,
        breaks = breaks,
        length = length,
        maxage = maxage,
        lang = lang,
        label_tail = label_tail,
        sep_zero = sep_zero
      )
      basi <- as.integer(x$basi)
      icd10 <- toupper(x$icd10)
      icd_cat <- classify_icd10(icd10, type = type, lang = lang)
      morp <- x$morp
      res <- tibble(year, sex, agegrp, icd10, icd_cat, basi, morp)
      return(res)
    },
    SWcases = {
      #cat("Processing data: ", length(x$sex), " SWcases.\n")
      year <- as.numeric(format(x$deathda, "%Y"))
      sex <- tidy_sex(x$sex, lang = lang)
      age <- calc_age(x$birthda, x$deathda)
      agegrp <- cutage(age,
        method = cutage_method,
        breaks = breaks,
        length = length,
        maxage = maxage,
        lang = lang,
        label_tail = label_tail,
        sep_zero = sep_zero
      )
      icd10 <- toupper(x$icd10)
      icd_cat <- classify_icd10(icd10, type = type, lang = lang)
      res <- tibble(year, sex, agegrp, icd_cat)
      return(res)
    },
    population = {
      #cat("Processing POP data of year: ", unique(x$year),"\n")
      res <- x %>%
        mutate(sex = tidy_sex(sex, lang = lang)) %>%
        group_by(year, sex) %>%
        reframe(
          agegrp = cutage(seq(0, 92, 1),
            method = cutage_method,
            breaks = breaks,
            length = length,
            maxage = maxage,
            lang = lang,
            label_tail = label_tail,
            sep_zero = sep_zero
          ),
          rks = as.integer(expand_age_pop(rks, method = smooth_age)$y)
        ) %>%
        group_by(year, sex, agegrp) %>%
        reframe(rks = sum(rks))
      return(res)
    },
    default = {
      stop(paste("Unsupported data class:", dclass))
    }
  )
}
