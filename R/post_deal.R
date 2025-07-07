#' Recode sex-specific cancers
#'
#' Adjusts cancer records to account for sex-specific cancers
#' * If a male case has a female-specific cancer, it is recoded
#' * If a female case has a male-specific cancer, it is recoded
#' * Sex-specific cancers are reassigned to sex = 0 to denote shared category
#'
#' @param x A data frame (usually from ASR output) with `sex` and `cancer`
#'    columns
#'
#' @return A modified data frame with recoded sex-specific cancer cases
#' @noRd
post_sex_specific_cancer <- function(x) {
  sex <- rlang::sym("sex")
  cancer <- rlang::sym("cancer")
  female_cancer <- c(29:37, 114:117, 206, 320:325)
  male_cancer <- c(38:41, 118:119, 207, 326:328)
  sex_specific_cancer <- x |>
    filter(
      as.numeric(!!sex) == 2 & !!cancer %in% female_cancer |
        as.numeric(!!sex) == 1 & !!cancer %in% male_cancer
    ) |>
    mutate(!!sex := 0)

  res <- x |>
    filter(!(!!sex == 0 & !!cancer %in% c(male_cancer, female_cancer))) |>
    bind_rows(sex_specific_cancer) |>
    filter(
      (!!sex == 1 & !!cancer %nin% female_cancer) |
        (!!sex == 2 & !!cancer %nin% male_cancer) |
        !!sex == 0
    )

  sex_not_cancer <- x |>
    filter((!!sex == 1 & !!cancer %in% female_cancer) |
             (!!sex == 2 & !!cancer %in% male_cancer)) |>
    mutate(across(!starts_with(c("areacode", "name", "year", "sex", "cancer",
                                 "pop", "site", "type", "icd10", "rks",
                                 "agegrp")), ~ 0))
  bind_rows(res, sex_not_cancer)
}

#' Complete stratify variables
#'
#' @noRd
post_vars <- function(data) {
  stopifnot(is.data.frame(data))
  
  defaults <- list(
    year = 9000L,
    sex = 0L,
    cancer = "60"
  )
  
  for (var in names(defaults)) {
    if (var %nin% names(data)) {
      data[[var]] <- defaults[[var]]
    }
  }
  dplyr::relocate(data, !!!rlang::syms(c("year", "sex", "cancer")))
}

#' Drop the total cancer
#'
#' Filter out cases that summarized as `total` when using [create_asr()],
#' [create_quality()], or [create_age_rate()].
#'
#' @param x A data frame containing a `cancer` variable created by
#'   [classify_icd10()].
#' @return A filtered data frame excluding `cancer` in `c("60", "61")`.
#' @export
#' @examples
#' data("canregs")
#' qua <- create_quality(canregs, year, sex, cancer)
#' qua2 <- qua |> drop_total()
#' 
drop_total <- function(x) {
  cancer <- rlang::sym("cancer")
  filter(x, !!cancer %nin% c("60", "61"))
}


#' Drop “other” cancer codes
#'
#' Filters out cases classified as "other" cancer types based on specific codes.
#'
#' @param x A data frame containing a `cancer` variable created by
#'   [classify_icd10()].
#'
#' @return A filtered data frame excluding non-specific cancer codes.
#' @export
#' @examples
#' data("canregs")
#' asr <- create_quality(canregs, year, sex, cancer)
#' asr2 <- asr |> drop_others()
#' 
drop_others <- function(x) {
  cancer <- rlang::sym("cancer")
  filter(x, !!cancer %nin% c("0", "111", "126", "211"))
}
