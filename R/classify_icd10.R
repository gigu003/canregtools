#' Classify ICD10 Codes to Cancer Categories.
#' 
#' @description
#' Classify ICD10 codes into Cancer Categories according to the specified
#' category type and language.
#' 
#' 
#' @param x The ICD10 codes of cancer part (C00-C98 and D00-D48) collected
#'        by the Population-Based Cancer Registration (PBCR).
#' @inheritParams cancer_type
#' @param as_factor logical indicate that the output value as factor.
#'
#' @return Cancer code
#' @export
#'
#' @examples
#' icd10 <- c("C15.2", "C33.4", "C80.9", "C26.2", "C16.3")
#' classify_icd10(icd10, cancer_type = "big")
classify_icd10 <- function(x, cancer_type = "big", as_factor = FALSE) {
  cancer_type <- toupper(cancer_type)
  x <- toupper(substr(x, 1, 3))
  if (cancer_type %in% c("BIG", "SMALL")) {
    res <- case_match(
      x,
      c("C00") ~ 1,
      c("C01", "C02") ~ 2,
      c(paste0("C0", seq(3, 6))) ~ 3,
      c("C07", "C08") ~ 4,
      c("C09") ~ 5,
      c("C10") ~ 6,
      c("C11") ~ 7,
      c("C12", "C13") ~ 8,
      c("C14") ~ 9,
      c("C15") ~ 10,
      c("C16") ~ 11,
      c("C17") ~ 12,
      c("C18") ~ 13,
      c("C19", "C20") ~ 14,
      c("C21") ~ 15,
      c("C22") ~ 16,
      c("C23", "C24") ~ 17,
      c("C25") ~ 18,
      c("C30", "C31") ~ 19,
      c("C32") ~ 20,
      c("C33", "C34") ~ 21,
      c("C37", "C38") ~ 22,
      c("C40", "C41") ~ 23,
      c("C43") ~ 24,
      c("C44") ~ 25,
      c("C45") ~ 26,
      c("C46") ~ 27,
      c("C47", "C49") ~ 28,
      c("C50") ~ 29,
      c("C51") ~ 30,
      c("C52") ~ 31,
      c("C53") ~ 32,
      c("C54") ~ 33,
      c("C55") ~ 34,
      c("C56") ~ 35,
      c("C57") ~ 36,
      c("C58") ~ 37,
      c("C60") ~ 38,
      c("C61") ~ 39,
      c("C62") ~ 40,
      c("C63") ~ 41,
      c("C64") ~ 42,
      c("C65") ~ 43,
      c("C66") ~ 44,
      c("C67") ~ 45,
      c("C68") ~ 46,
      c("C69") ~ 47,
      c(paste0("C", seq(70, 72)), "D32", "D33", "D42", "D43") ~ 48,
      c("C73") ~ 49,
      c("C74") ~ 50,
      c("C75") ~ 51,
      c("C81") ~ 52,
      c(paste0("C", c(seq(82, 85), 96))) ~ 53,
      c("C88") ~ 54,
      c("C90") ~ 55,
      c("C91") ~ 56,
      c(paste0("C", seq(92, 94)), paste0("D", seq(45, 47))) ~ 57,
      c("C95") ~ 58,
      c(paste0(
        "C", c(seq(26, 29), 35, 36, 39, 48, 59, seq(76, 80), 86, 87, 89)
      )) ~ 59,
      NA ~ NA
    )
    res <- as.integer(res)
    if (cancer_type == "BIG") {
      res <- case_match(
        res,
        c(1:6, 8, 9) ~ 101,
        7 ~ 102,
        10 ~ 103,
        11 ~ 104,
        c(13:15) ~ 105,
        16 ~ 106,
        17 ~ 107,
        18 ~ 108, 
        20 ~ 109,
        21 ~ 110,
        22 ~ 111,
        23 ~ 112,
        24 ~ 113,
        29 ~ 114,
        32 ~ 115,
        c(33, 34) ~ 116,
        35 ~ 117,
        39 ~ 118,
        40 ~ 119,
        c(42:44, 46) ~ 120,
        45 ~ 121,
        48 ~ 122,
        49 ~ 123,
        c(52:55) ~ 124,
        c(56:58) ~ 125,
        c(12, 19, 25:28, 30, 31, 36:38, 41, 47, 50, 51, 59) ~ 126,
        .default = NA
      )
      res <- as.integer(res)
      if (as_factor){
        res <- factor(res, levels = c(101:126), labels = c(101:126))
      }
    }
  } else if (cancer_type == "SYSTEM"){
    res <- case_match(
      x,
      c(paste0("C0", 1:9), paste0("C", 10:14)) ~ 1,
      c(paste0("C", 15:26)) ~ 2,
      c(paste0("C", 30:39)) ~ 3,
      c(paste0("C", 40:44)) ~ 4,
      c("C50") ~ 5,
      c(paste0("C", 51:58)) ~ 6,
      c(paste0("C", 60:63)) ~ 7,
      c(paste0("C", 64:68)) ~ 8,
      c(paste0("C", 69:75)) ~ 9,
      c(paste0("C", 81:96)) ~ 10,
      c(paste0("C", c(27:29, 45:49, 59, 76:80, 97))) ~ 11,
      .default = NA
    )
    res <- res + 200
    res <- as.integer(res)
    if (as_factor){
      res <- factor(res, levels = c(201:211), labels = c(201:211))
    }
  } else if (cancer_type == "GCO"){
    res <- case_match(
      x,
      c(paste0("C0", 1:6)) ~ 301,
      c(paste0("C0", 7:8)) ~ 302,
      c(paste0("C", c("09","10"))) ~ 303,
      c("C11") ~ 304,
      c(paste0("C", 12:13)) ~ 305,
      c("C15") ~ 306,
      c("C16") ~ 307,
      c("C18") ~ 308,
      c(paste0("C", 19:20)) ~ 309,
      c("C21") ~ 310,
      c("C22") ~ 311,
      c("C23") ~ 312,
      c("C25") ~ 313,
      c("C32") ~ 314,
      c(paste0("C", 33:34)) ~ 315,
      c("C43") ~ 316,
      c("C44") ~ 317,
      c("C45") ~ 318,
      c("C46") ~ 319,
      c("C50") ~ 320,
      c("C51") ~ 321,
      c("C52") ~ 322,
      c("C53") ~ 323,
      c("C54") ~ 324,
      c("C56") ~ 325,
      c("C60") ~ 326,
      c("C61") ~ 327,
      c("C62") ~ 328,
      c("C64") ~ 329,
      c("C67") ~ 330,
      c(paste0("C", 70:72)) ~ 331,
      c("C73") ~ 332,
      c("C81") ~ 333,
      c(paste0("C", c(82:86, 88))) ~ 334,
      c("C90") ~ 335,
      c(paste0("C", 91:95)) ~ 336,
      c(paste0("C", c(76:80, 96:97))) ~ 337,
      NA ~ NA,
      .default = 338
    )
    res <- as.integer(res)
    if (as_factor){
      res <- factor(res, levels = c(301:338), labels = c(301:338))
    }
  }
  
  if (as_factor&cancer_type=="SMALL"){
    res <- factor(res, levels = c(1:59), labels = c(1:59))
  }
  
  return(res)
}
