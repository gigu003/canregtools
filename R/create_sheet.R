#' Create sheet of cancer statistics
#'
#' @rdname create_sheet
#' @param x data with class of 'fbswicd' or 'canreg'.
#' @param sheet_type Character value indicate type of sheet, options are
#'        'incidence', 'mortality', 'quality', 'population'.
#' @param cancer_type Type of classification method of ICD10, options are 'big',
#'        'small', 'system', or 'gco', default is "big". 
#'
#' @return A data frame or tibble contains the age standard rates and CIs.
#' @export
#'
create_sheet <- function(x,
                         sheet_type = "incidence",
                         cancer_type = "big") {
  UseMethod("create_sheet", x)
}

#' @rdname create_sheet
#' @method create_sheet canreg
#' @export
#' 
create_sheet.canreg <- function(x,
                                sheet_type = "incidence",
                                cancer_type = "big") {
  fbsw_ <- count_canreg(x, cancer_type = cancer_type)
  res <- create_sheet.fbswicd(fbsw_, sheet_type = sheet_type)
  return(res)
}

#' @rdname create_sheet
#' @method create_sheet fbswicds
#' @export
#'
create_sheet.fbswicds <- function(x,
                                  sheet_type = "incidence",
                                  cancer_type = "big") {
  res <- purrr::map(x,
                    create_sheet.fbswicd,
                    sheet_type = sheet_type, cancer_type = cancer_type,
                    .progress = "Creating sheets #")
  res <- cr_merge(res)
  return(res)
}

#' @rdname create_sheet
#' @method create_sheet fbswicd
#' @export
#'
create_sheet.fbswicd <- function(x,
                                 sheet_type = "incidence",
                                 cancer_type = "big") {
  if (sheet_type == "quality") {
    qua1 <- create_quality(x, year, sex, cancer)
    qua2 <- create_quality(x, year, cancer)
    qua <- bind_rows(qua1, qua2)
    qua <- post_sex_specific_cancer(qua)
    res <- qua
  } else if (sheet_type == "population") {
    pop <- x[["pop"]]
    res <- pop |> 
      group_by(year, agegrp) |>
      reframe(rks = sum(rks)) |> 
      mutate(sex = 0) |> 
      bind_rows(pop) |> 
      mutate(agegrp = as.numeric(agegrp)) |> 
      pivot_wider(names_from = agegrp,
                  names_prefix = "p",
                  names_sep = "",
                  values_from = c("rks"),
                  values_fill = 0)
  } else if (sheet_type %in% c("incidence", "mortality")) {
    # Generate ASR (age-standardized rates) for the data set
    if (sheet_type == "incidence") {
      asr1 <- create_asr(x, year, sex, cancer)
      asr2 <- create_asr(x, year, cancer)
      age_rate1 <- create_age_rate(x, format = "wide", year, sex, cancer)
      age_rate2 <- create_age_rate(x, format = "wide", year, cancer)
    } else if (sheet_type == "mortality") {
      asr1 <- create_asr(x, event = sws, year, sex, cancer)
      asr2 <- create_asr(x, event = sws, year, cancer)
      age_rate1 <- create_age_rate(x, event = sws, format = "wide", year, sex, cancer)
      age_rate2 <- create_age_rate(x, event = sws, format = "wide", year, cancer)
    } else {
      stop("Unsupported sheet_type. Choose from 'incidence' or 'mortality'.")
    }
    asr <- bind_rows(asr1, asr2)
    asr <- post_sex_specific_cancer(asr)
    age_rate <- bind_rows(age_rate1, age_rate2)
    age_rate <- post_sex_specific_cancer(age_rate)
    
    # Merge ASR and age-specific rates into final result
    res <- age_rate |> 
      left_join(asr, by = c("year", "sex", "cancer"))
  }
  return(res)
}

