#' Count canreg data.
#'
#' @rdname count_canreg
#' @param x Object data of class 'canreg' or 'canregs'.
#' @param age_breaks Specify the break points classify age groups. Default is
#'        `c(0, 1, seq(0, 85, 5))`.
#' @param label_tail Tail label to be added to the labels. Default is NULL.
#' @inheritParams cancer_type
#' @importFrom data.table data.table setorder
#' @return data of class fbswicd.
#' @export
count_canreg <- function(x,
                         age_breaks = c(0, 1, seq(5, 85, 5)),
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
                    count_canreg,
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
#' file <- system.file("extdata", "411721.xlsx", package = "canregtools")
#' data <- read_canreg(file)
#' fbsw <- count_canreg(data)
#' fbsw
count_canreg.canreg <- function(x,
                                age_breaks = c(0, 1, seq(5, 85, 5)),
                                label_tail = NULL,
                                cancer_type = "big") {
  format <- cr_clean(x,
                     age_breaks = age_breaks,
                     label_tail = label_tail,
                     cancer_type = cancer_type
                     )
  fb <- format$FBcases
  sw <- format$SWcases

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
  res <- merge(cate, res, by = c("year", "sex", "agegrp", "cancer"), all.x = TRUE)
  
  num_cols <- names(res)[sapply(res, is.numeric)]
  res[, (num_cols) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = num_cols]
  res[, (num_cols) := lapply(.SD, as.integer), .SDcols = num_cols]

  sitemorp <- fb[, .(site = list(count_tp(icd10)),
                     morp = list(count_tp(morp))),
                 by = .(year, sex, cancer)]
  sitemorp <- setorder(sitemorp, year, sex, cancer)
    
  result <- list(areacode = as.character(x$areacode),
                 fbswicd = res,
                 sitemorp = sitemorp,
                 pop = format$POP)
  attr(result, "class") <- c("fbswicd", class(result))
  return(result)
}

reg_count <- function(data, varname = "fbs") {
  data <- as.data.table(data)
  varname <- as.name(varname)
  gvars1 <- c("year", "sex", "cancer", "agegrp")
  gvars2 <- setdiff(gvars1, "cancer")
  mcount1 <- data[, .N, by = gvars1, drop = FALSE]
  mcount2 <- data[, .N, by = gvars2, drop = FALSE]
  mcount2[, cancer := 60L]
  mcount3 <- data[!grepl("^C44", icd10), .N, by = gvars2, drop = FALSE]
  mcount3[, cancer := 61L]
  res <- rbindlist(list(mcount1, mcount2, mcount3), fill = TRUE)
  
  gvars3 <- setdiff(gvars1, "sex")
  N <- "N"
  sex_total <- res[, .(sex = 0L, N = sum(N, na.rm = TRUE)), by = gvars3]
  res <- rbind(res, sex_total, fill = TRUE)
  
  setnames(res, "N", as.character(varname))
  return(res)
}

# function count sub-sites of icd10 or morphology code in each year, sex, cancer
count_tp <- function(x) {
  var_name <- rlang::ensym(x)
  var_str  <- rlang::as_string(var_name)
  vec <- eval.parent(var_name)
  dt <- data.table(value = as.character(vec))
  res <- dt[, .N, by = value][order(value)]
  setnames(res, c(var_str, "count"))
  return(res)
}
