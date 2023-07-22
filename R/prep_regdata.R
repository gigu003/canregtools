#' Prepare the population based cancer registration data
#' @description
#' Organize data (FB, SW, and POP) for further analysis using fbswicd function.
#'
#' @param data Data frame.
#' @param icd_type Specify the classification method for ICD-10.
#'
#' @return Data frame includes newly generated variables such as year of
#'         incidence or death, age groups, cancer classifications, etc.
#' @export
#'
#' @import dplyr
#' @import readxl
#' 
#' @author [Qiong Chen](https://www.chenq.site), Email: chenqiong@hnccr.org.cn
#'
#' @examples
#' library(canregtools)
#' library(readxl)
#' file <- system.file("extdata", "411721.xls", package = "canregtools")
#' FB <- read_excel(file, sheet="FB")
#' fb <- prep_regdata(FB, icd_type = "big")
#'
prep_regdata <- function(data, icd_type = "big") {
  # 把所有变量名改为小写
  if (deparse(substitute(data)) %in% c("FB", "SW")) {
    if (deparse(substitute(data)) == "FB") {
      year <- as.numeric(format(data$inciden, "%Y"))
      basi <- as.integer(data$basi)
    } else if (deparse(substitute(data)) == "SW") {
      year <- as.numeric(format(data$deathda, "%Y"))
    }
    # 把性别编码改为整数格式
    sex <- as.integer(data$sex)
    # 把年龄改为整数格式
    age <- as.integer(data$age)
    # 把年龄分为19个年龄分组,并生成新变量agegrp
    agegrp <- cut(age,
                  c(0, 1, seq(5, 85, 5), Inf),
                  labels = seq(1, 19, 1),
                  right = FALSE)
    icd10 <- toupper(data$icd10)
    icd_cat <- classify_icd10(icd10, type = icd_type)
    # 生成数据框
    if (deparse(substitute(data)) == "FB") {
      output <- tibble(year, sex, age, agegrp, icd10, icd_cat, basi)
    } else if (deparse(substitute(data)) == "SW") {
      output <- tibble(year, sex, age, agegrp, icd10, icd_cat)
    }
  } else {
    year <- as.integer(data$year)
    sex <- as.integer(data$sex)
    agegrp <- factor(data$agegrp, labels = seq(1, 19, 1))
    rks <- as.integer(data$rks)
    output <- tibble(year, sex, agegrp, rks)
  }
  return(output)
}
