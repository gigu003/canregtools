#' Count canreg data.
#'
#' @rdname count_canreg
#' @param x Object data of class 'canreg' or 'canregs'.
#' @param cutage_method Methods for Specifying Age Groups. Options are
#'        "interval", "distance", or "quantile". Default is "distance".
#' @param breaks Specify the break points classify age groups when
#'        cutage_method is 'interval'. Default is 'c(0, 15, 40, 65)'.
#' @param length Specify the length of each age group when cutage_method is
#'        'distance'. Default is 5.
#' @param maxage Specify the max age of age group when cutage_method is
#'        'distance'. Default is 85.
#' @param sep_zero Logical value, TRUE or FALSE, specifying whether to treat age
#'        0 as a separate group. Default is TRUE.
#' @param labels Labels for age groups. Default is NULL.
#' @param label_tail Tail label to be added to the labels. Default is NULL.
#' @inheritParams cancer_type
#'
#' @return data of class fbswicd.
#' @export
count_canreg <- function(x,
                         cutage_method = "distance",
                         breaks = c(0, 15, 40, 65),
                         length = 5,
                         maxage = 85,
                         sep_zero = TRUE,
                         labels = NULL,
                         label_tail = NULL,
                         cancer_type = "big") {
  UseMethod("count_canreg", x)
}

#' @rdname count_canreg
#' @method count_canreg canregs
#' @param ... Parameters.
#' @export
count_canreg.canregs <- function(x, ...) {
  res <- purrr::map(x,
                    count_canreg.canreg,
                    ...,
                    .progress = "Counting canregs #")
  class(res) <- c("fbswicds", "list")
  return(res)
}

#' @rdname count_canreg
#' @method count_canreg canreg
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
                         breaks = c(0, 15, 40, 65),
                         length = 5,
                         maxage = 85,
                         sep_zero = TRUE,
                         labels = NULL,
                         label_tail = NULL,
                         cancer_type = "big") {
  fb <- clean_canreg(x$FBcases,
    cutage_method = cutage_method,
    breaks = breaks,
    length = length,
    maxage = maxage,
    sep_zero = sep_zero,
    labels = labels,
    label_tail = label_tail,
    cancer_type = cancer_type)
  
  sw <- clean_canreg(x$SWcases,
    cutage_method = cutage_method,
    breaks = breaks,
    length = length,
    maxage = maxage,
    sep_zero = sep_zero,
    labels = labels,
    label_tail = label_tail,
    cancer_type = cancer_type)
  
  pop <- clean_canreg(x$POP,
    cutage_method = cutage_method,
    breaks = breaks,
    length = length,
    maxage = maxage,
    sep_zero = sep_zero,
    labels = labels,
    label_tail = label_tail)

  ubs <- c("C26", "C39", "C48", "C76", "C77", "C78", "C79", "C80", "C97")
  basi <- rlang::sym("basi")
  icd10 <- rlang::sym("icd10")
  morp <- rlang::sym("morp")
  # prepare the data for counting

  fb <- as.data.table(fb)
  fb <- na.omit(fb, cols = c("year", "sex"))
  # 使用data.table进行过滤
  fb_basi_5_7 <- fb[basi %in% c(5, 6, 7), ]
  fb_ubs <- fb[substr(icd10, 1, 3) %in% ubs, ]
  fb_icd10_9 <- fb[substr(icd10, 5, 5) == "9", ]
  fb_morp_8000_8001 <- fb[morp %in% c("8000", "8001"), ]
  fb_basi_0 <- fb[basi == 0, ]
  # count data
  res <- list(
    reg_count(fb, varname = "fbs"),
    reg_count(sw, varname = "sws"),
    reg_count(fb_basi_5_7, varname = "mv"),
    reg_count(fb_ubs, varname = "ub"),
    reg_count(fb_icd10_9, varname = "sub"),
    reg_count(fb_morp_8000_8001, varname = "m8000"),
    reg_count(fb_basi_0, varname = "dco")
    )
  res <- Reduce(function(dt1, dt2) {
    merge(dt1, dt2, by = c("year", "sex", "agegrp", "cancer"), all = TRUE)
  }, res)
  
  # expand to full data frame of combinations of each variable values.
  cate <- CJ(year = unique(res$year),
             sex = unique(res$sex),
             cancer = unique(res$cancer),
             agegrp = factor(levels(res$agegrp), levels = levels(res$agegrp)),
             sorted = TRUE)
  res <- merge(cate, res, by = c("year", "sex", "agegrp", "cancer"),
               all.x = TRUE)
  
  num_cols <- names(res)[sapply(res, is.numeric)]
  res[, (num_cols) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = num_cols]
  res[, (num_cols) := lapply(.SD, as.integer), .SDcols = num_cols]

  sitemorp <- fb[, .(
    site = list(count_tp(icd10)),
    morp = list(count_tp(morp))
  ), by = .(year, sex, cancer)]
    
  result <- list(areacode = as.character(x$areacode),
                 fbswicd = res,
                 sitemorp = sitemorp,
                 pop = pop)
  attr(result, "class") <- c("fbswicd", class(result))
  return(result)
}



reg_count <- function(data, varname = "fbs") {
  data <- as.data.table(data)
  varname <- as.name(varname)
  gvars1 <- c("year", "sex", "cancer", "agegrp")
  gvars2 <- c("year", "sex", "agegrp")
  mcount1 <- data[, .N, by = gvars1, drop = FALSE]
  mcount2 <- data[, .N, by = gvars2, drop = FALSE]
  set(mcount2, j = "cancer", value = 60)
  mcount3 <- data[!grepl("^C44", icd10), .N, by = gvars2, drop = FALSE]
  set(mcount3, j = "cancer", value = 61)
  result <- rbindlist(list(mcount1, mcount2, mcount3), fill = TRUE)
  setnames(result, "N", as.character(varname))
  return(result)
}

# function count sub-sites of icd10 or morphology code in each year, sex, cancer
count_tp <- function(x) {
  var_name <- rlang::ensym(x)
  res <- as.data.frame(table(x), stringsAsFactors = FALSE)
  names(res) <- c(rlang::as_string(var_name), "count")
  return(res)
}
