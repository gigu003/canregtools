#' Add variable labels for data set.
#'
#' This function labels variables such as `sex` and `cancer` in the dataset
#' based on the specified language and label type.
#'
#' @param x A data frame or tibble to be labeled.
#' @inheritParams label_type
#' @inheritParams lang
#' @inheritParams as_factor
#' @return A data frame or tibble with labeled variables.
#' @export
#'
add_labels <- function(x,
                       label_type = "full",
                       lang = "zh",
                       as_factor = TRUE){
  col_names <- colnames(x)
  res <- x
  if ("sex" %in% col_names) {
    res$sex <- tidy_var(res$sex, var_name = "sex", label_type = label_type,
                        lang = lang, as_factor = as_factor)
    }
  if ("cancer" %in% col_names) {
    res$site <- tidy_var(res$cancer, var_name = "cancer",
                         label_type = label_type, lang = lang,
                         as_factor = as_factor)
    res$icd10 <- tidy_var(res$cancer, var_name = "cancer", lang = "icd10")
  }
  
  areacode <- rlang::sym("areacode")
  if ("areacode" %in% col_names) {
    res$name <- tidy_var(res$areacode, var_name = "areacode",
                         lang = lang, label_type = label_type,
                         as_factor = as_factor)
  }
  
  res <- res |> 
    select(starts_with(c("areacode", "name", "year", "sex", "cancer", "site",
                         "icd10")),
           everything())
  return(res)
}


#' post
#'
#' @param x asr 
#'
#' @returns a
#'
post_sex_specific_cancer <- function(x){
  sex <- rlang::sym("sex")
  cancer <- rlang::sym("cancer")
  female_cancer <- c(29:37, 114:117, 206, 320:325)
  male_cancer <- c(38:41, 118:119, 207, 326:328)
  sex_specific_cancer <- x |> 
    filter(
      as.numeric(!!sex) == 2 & !!cancer %in% female_cancer |
        as.numeric(!!sex) == 1 & !!cancer %in% male_cancer ) |> 
    mutate(!!sex := 0)
  
  res <- x |> 
    filter(!(!!sex == 0 & !!cancer %in% c(male_cancer, female_cancer))) |> 
    bind_rows(sex_specific_cancer) |> 
    filter(
      (!!sex == 1 & !!cancer %nin% female_cancer) |
        (!!sex == 2 & !!cancer %nin% male_cancer) |
        !!sex == 0)
  
  sex_not_cancer <- x |> 
    filter((!!sex == 1 & !!cancer %in% female_cancer)|
             (!!sex == 2 & !!cancer %in% male_cancer)) |> 
    mutate(across(!starts_with(c("areacode", "name", "year", "sex", "cancer",
                                 "pop", "site", "type", "icd10", "rks","agegrp")), ~0)
           )
  res <- bind_rows(res, sex_not_cancer) 
  return(res)
}

post_vars <- function(data){
  all_vars <- colnames(data)
  year <- rlang::sym("year")
  sex <- rlang::sym("sex")
  cancer <- rlang::sym("cancer")
  if ("year" %nin% all_vars){
    data <- mutate(data, !!year := 9000L)
  }
  if ("sex" %nin% all_vars){
    data <- mutate(data, !!sex := 0L)
  }
  if ("cancer" %nin% all_vars){
    data <- mutate(data, !!cancer := "60")
  }
  
  data <- select(data, starts_with(c("year", "sex", "cancer")), everything())
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
  cancer <- rlang::sym("cancer")
  res <- filter(x, !!cancer %nin% c("60", "61"))
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
  cancer <- rlang::sym("cancer")
  res <- filter(x, !!cancer %nin% c("0", "111", "126", "211"))
  return(res)
}
