#' Add variable labels for data set.
#'
#' This function labels variables such as `sex` and `cancer` in the dataset
#' based on the specified language and label type.
#'
#' @param x A data frame or tibble to be labeled.
#' @param label_type Type of label for the cancer variable. Options are:
#'   \itemize{
#'     \item "full" - Full descriptive labels.
#'     \item "abbr" - Shortened labels.
#'     \item "icd10" - ICD-10 codes only.
#'   }
#'   The default is "full".
#' @param lang Language of the labels. Options are:
#'   \itemize{
#'     \item "zh" - Chinese.
#'     \item "en" - English.
#'   }
#'   The default is "zh".
#'
#' @return A data frame or tibble with labeled variables.
#' @export
#'
add_labels <- function(x,
                       label_type = "full",
                       lang = "zh"){
  col_names <- colnames(x)
  res <- x
  if ("sex" %in% col_names) {
    res$sex <- tidy_sex(res$sex, lang = lang, as_factor = T) }
  if ("cancer" %in% col_names) {
    res$site <- tidy_cancer(res$cancer, label_type = label_type,
                            lang = lang, as_factor = T)
    res$icd10 <- tidy_cancer(res$cancer, label_type = "icd10")
  }
  
  areacode <- rlang::sym("areacode")
  if ("areacode" %in% col_names) {
    areacodes <- res |> pull(!!areacode)
    addr <- "~/.canregtools/label_areacode.dcf"
    if (file.exists(addr)) {
      label_area <- read.dcf(addr)
      label_area <- as.data.frame(label_area)}
    label_sel <- label_area |> filter(!!areacode %in% areacodes)
    areacode2 <- label_sel |> pull(!!areacode)
    label_cn <- label_sel |> pull(label_cn)
    label_en <- label_sel |> pull(label_en)
    res$name <- factor(res$areacode, levels = areacode2, labels = label_cn)
  }
  
  res <- res |> 
    select(starts_with(c("areacode", "name", "year", "sex", "cancer", "site",
                         "icd10")),
           everything())
  return(res)
}


post_sex_specific_cancer <- function(x){
  female_cancer <- c(29:37, 114:117, 206, 320:325)
  male_cancer <- c(38:41, 118:119, 207, 326:328)
  sex_specific_cancer <- x |> 
    filter(
      as.numeric(sex) == 2 & cancer %in% female_cancer |
        as.numeric(sex) == 1 & cancer %in% male_cancer ) |> 
    mutate(sex = 0)
  res <- x |> 
    filter(!(sex == 0 & cancer %in% c(male_cancer, female_cancer))) |> 
    bind_rows(sex_specific_cancer) |> 
    arrange(year, sex, cancer)
  return(res)
}

post_vars <- function(data){
  all_vars <- colnames(data)
  if ("year" %nin% all_vars){
    data <- data |> 
      mutate(year = 9000)
  }
  if ("sex" %nin% all_vars){
    data <- data |> 
      mutate(sex = 0)
  }
  if ("cancer" %nin% all_vars){
    data <- data |> 
      mutate(cancer = 60)
  }
  
  data <- data |> 
    select(starts_with(c("year", "sex", "cancer")), everything())
  return(data)
}

#' Drop the total cancer
#'
#' @param x A data frame.
#'
#' @return A data frame.
#' @export
#'
drop_total <- function(x){
  res <- x |> 
    filter(cancer %nin% c(60, 61))
  return(res)
}


#' Drop other cancer in cancer categories.
#'
#' @param x A data frame.
#'
#' @return A data frame.
#' @export
#'
drop_others <- function(x){
  res <- x |> 
    filter(cancer %nin% c(0, 111, 126, 211))
  return(res)
}
