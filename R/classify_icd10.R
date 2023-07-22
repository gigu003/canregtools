#' Convert ICD10 Codes to Cancer Categories.
#' @description
#' Convert ICD10 codes to Cancer Categories according to the specified
#' category type and language.
#'    
#'
#' @param icd10 The ICD10 codes of cancer part (C00-C98 and D00-D48) collected 
#'        by the Population-Based Cancer Registration (PBCR).
#' @param type Character, type of cancer category ("system","big","small"), by
#'        default, the "big" category was choosed which will categorize the
#'        icd10 code 25 cancer categories.
#' @param lang Character, two options ("cn","en"), by default, lang="cn" was
#'        choosed, which will output the categories in Chinese, otherwise, in
#'        English.
#'
#' @return a vector that contain the cancer categories.
#' @export
#'
#' @author [Qiong Chen](https://www.chenq.site), Email: chenqiong@hnccr.org.cn
#' @references National Cancer Center. Guidelines for Cancer Registration in
#'        China (2016) \[M\]. Beijing: People's Medical Publishing House, 2016.  
#' @examples
#' library(canregtools)
#' icd10 <- c("C16.1", "C15.1", "C34.1", "C34.2", "C15.0")
#' category <- classify_icd10(icd10, type = "system", lang = "en")
#'
classify_icd10 <- function(icd10, type = "big", lang = "cn") {
  icd_number <- as.numeric(gsub("[^0-9\\.]", "", icd10))
  if (type == "system") {
    breaks <- c(-Inf, 0, 15, 27, 30, 40, 45, 50, 51, 59, 60, 64, 69, 76,
                81, 97, 98, Inf)
    labels <- c(0, 1, 2, 11, 3, 4, 11, 5, 6, 11, 7, 8, 9, 11, 10, 11, 0)
    icdd <- cut(icd_number, breaks = breaks, labels = labels, right = FALSE)
    icdd <- as.numeric(as.character(icdd))
    if (lang == "cn") {
      icdd <- factor(icdd, levels = c(0:11), labels = label[[1]]$cn)
    } else {
      icdd <- factor(icdd, levels = c(0:11), labels = label[[1]]$en)
    }
  } else if (type == "big") {
    breaks <-
      c(
        -Inf, 0, 11, 12, 15, 16, 17, 18, 22, 23, 25, 26, 32, 33,
        35, 37, 39, 40, 42, 43, 44, 50, 51, 53, 54, 56, 57,
        61, 62, 63, 64, 67, 68, 69, 70, 73, 74, 81, 86, 88,
        89, 90, 91, 96, 97, 98, Inf
      )
    labels <-
      c(
        0, 1, 2, 1, 3, 4, 26, 5, 6, 7, 8, 26, 9,
        10, 26, 11, 26, 12, 26, 13, 26, 14, 26, 15, 16,
        17, 26, 18, 19, 26, 20, 21, 20, 26, 22, 23, 26,
        24, 26, 24, 26, 24, 25, 24, 26, 0
      )
    icdd <- cut(icd_number, breaks = breaks, labels = labels, right = FALSE)
    icdd <- as.numeric(as.character(icdd))
    loc <- which(toupper(substring(icd10, 1, 1)) == "D" &
      ((as.numeric(gsub("[^0-9\\.]", "", icd10)) >= 32 &
        as.numeric(gsub("[^0-9\\.]", "", icd10)) < 34) |
        (as.numeric(gsub("[^0-9\\.]", "", icd10)) >= 42 &
          as.numeric(gsub("[^0-9\\.]", "", icd10)) < 44)))
    icdd[loc] <- 22

    loc <- which(toupper(substring(icd10, 1, 1)) == "D" &
      (as.numeric(gsub("[^0-9\\.]", "", icd10)) >= 45 &
        as.numeric(gsub("[^0-9\\.]", "", icd10)) < 48))
    icdd[loc] <- 25
    if (lang == "cn") {
      icdd <- factor(icdd, levels = c(0:26), labels = label[[2]]$cn)
    } else {
      icdd <- factor(icdd, levels = c(0:26), labels = label[[2]]$en)
    }
  } else if (type == "small") {
    breaks <-
      c(
        -Inf, 0, 1, 3, 7, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 21,
        22, 23, 25, 26, 30, 32, 33, 35, 37, 39, 40, 42, 43, 44, 45,
        46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
        61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 73, 74, 75, 76, 81,
        82, 86, 88, 89, 90, 91, 92, 95, 96, 97, 98, Inf
      )
    labels <-
      c(
        0, 1:18, 59, 19:21, 59, 22, 59, 23, 59, 24:28, 59, 28:37, 59, 38:51,
        59, 52:53, 59, 54, 59, 55:58, 53, 59, 0
      )

    icdd <- cut(icd_number, breaks = breaks, labels = labels, right = FALSE)
    icdd <- as.numeric(as.character(icdd))
    loc <- which(toupper(substring(icd10, 1, 1)) == "D" &
      ((as.numeric(gsub("[^0-9\\.]", "", icd10)) >= 32 &
        as.numeric(gsub("[^0-9\\.]", "", icd10)) < 34) |
        (as.numeric(gsub("[^0-9\\.]", "", icd10)) >= 42 &
          as.numeric(gsub("[^0-9\\.]", "", icd10)) < 44)))
    icdd[loc] <- 48

    loc <- which(toupper(substring(icd10, 1, 1)) == "D" &
      (as.numeric(gsub("[^0-9\\.]", "", icd10)) >= 45 &
        as.numeric(gsub("[^0-9\\.]", "", icd10)) < 48))
    icdd[loc] <- 57
    if (lang == "cn") {
      icdd <- factor(icdd, levels = c(0:59), labels = label[[3]]$cn)
    } else {
      icdd <- factor(icdd, levels = c(0:59), labels = label[[3]]$en)
    }
  }
  return(icdd)
}
