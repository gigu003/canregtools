#' Clean canreg data.
#'
#' @rdname cr_clean
#' @param x Data of class 'FBcases', 'SWcases' or 'population'.
#' @param age_breaks Cut points for age groups. Default is
#'        `c(0, 1, seq(5, 85, 5))`.
#' @param label_tail Tail of the labels.
#' @inheritParams cancer_type
#'
#' @return Class 'canreg'.
#' @export
#'
cr_clean <- function(x,
                     age_breaks = c(0, 1, seq(5, 85, 5)),
                     label_tail = NULL,
                     cancer_type = "big") {
  UseMethod("cr_clean", x)
}

#' @rdname cr_clean
#' @method cr_clean canregs
#' @export
cr_clean.canregs <- function(x,
                             age_breaks = c(0, 1, seq(5, 85, 5)),
                             label_tail = NULL,
                             cancer_type = "big"
                             ) {
  res <- purrr::map(x,
                    cr_clean.canreg,
                    age_breaks = age_breaks,
                    label_tail = label_tail,
                    cancer_type = cancer_type)
  class(res) <- c("canregs", "list")
  return(res)
}


#' @rdname cr_clean
#' @method cr_clean canreg
#' @export
cr_clean.canreg <- function(x,
                            age_breaks = c(0, 1, seq(5, 85, 5)),
                            label_tail = NULL,
                            cancer_type = "big"
                            ) {
  
  res <- list(
    areacode = purrr::pluck(x, "areacode"),
    FBcases = cr_clean.FBcases(purrr::pluck(x, "FBcases"),
                               age_breaks = age_breaks,
                               label_tail = label_tail,
                               cancer_type = cancer_type),
    SWcases = cr_clean.SWcases(purrr::pluck(x, "SWcases"),
                               age_breaks = age_breaks,
                               label_tail = label_tail,
                               cancer_type = cancer_type),
    POP = cr_clean.population(purrr::pluck(x, "POP"),
                              age_breaks = age_breaks,
                              label_tail = label_tail,
                              cancer_type = cancer_type))
  class(res) <- c("canreg", "list")
  return(res)
}

#' @rdname cr_clean
#' @method cr_clean FBcases
#' @export
cr_clean.FBcases <- function(x,
                             age_breaks = c(0, 1, seq(5, 85, 5)),
                             label_tail = NULL,
                             cancer_type = "big"){
  female_cancer <- c(29:37, 114:117, 206, 320:325)
  male_cancer   <- c(38:41, 118:119, 207, 326:328)
  
  # Convert the input data.frame to a data.table if it's not already one
  x <- as.data.table(x)
  
  # Step 1: Select only the required columns first (this can improve efficiency)
  x <- x[, .(inciden, sex, birthda, basi, icd10, morp)]
  
  # Step 2: Perform the necessary transformations
  x[, year := as.integer(format(inciden, "%Y"))]
  x[, sex := tidy_sex(sex)]
  x <- x[sex != 0]
  x[, age := calc_age(birthda, inciden)]
  x[, basi := as.integer(basi)]
  x[, icd10 := toupper(icd10)]
  x[, cancer := classify_icd10(icd10, cancer_type = cancer_type)]

  
  # Step 3: Age group transformation
  x[, agegrp := cutage(age, method = "interval", breaks = age_breaks,
                       label_tail = label_tail)]
  x <- x[!is.na(cancer)]
  x <- x[!( (sex == 1 & cancer %in% female_cancer) | 
              (sex == 2 & cancer %in% male_cancer) |
              sex == 0)]
  # Step 4: Return only the required columns
  res <- x[, .(year, sex, agegrp, basi, icd10, cancer, morp)]
  class(res) <- c("FBcases", class(res))
  return(res)
}

#' @rdname cr_clean
#' @method cr_clean SWcases
#' @export
cr_clean.SWcases <- function(x,
                             age_breaks = c(0, 1, seq(5, 85, 5)),
                             label_tail = NULL,
                             cancer_type = "big"){
  female_cancer <- c(29:37, 114:117, 206, 320:325)
  male_cancer   <- c(38:41, 118:119, 207, 326:328)
  x <- as.data.table(x)
  x <- x[, .(deathda, sex, birthda, icd10)]
  x[, year := as.integer(format(deathda, "%Y"))]
  x[, sex := tidy_sex(sex)]
  x <- x[sex != 0]
  x[, age := calc_age(birthda, deathda)]
  x[, icd10 := toupper(icd10)]
  x[, cancer := classify_icd10(icd10, cancer_type = cancer_type)]
  x[, agegrp := cutage(age, method = "interval", breaks = age_breaks,
                       label_tail = label_tail)]
  x <- x[!is.na(cancer)]
  x <- x[!( (sex == 1 & cancer %in% female_cancer) | 
                (sex == 2 & cancer %in% male_cancer) )]
  res <- x[, .(year, sex, agegrp, icd10, cancer)]
  class(res) <- c("SWcases", class(res))
  return(res)
}

#' @rdname cr_clean
#' @method cr_clean population
#' @export
cr_clean.population <- function(x,
                                age_breaks = c(0, 1, seq(5, 85, 5)),
                                label_tail = NULL,
                                cancer_type = "big"){
  x <- as.data.table(x)
  x[, sex := tidy_sex(sex)]
  
  # re-scale the age groups
  x <- x[, .(agegrp = cutage(seq(0, 92), method = "interval", breaks = age_breaks,
                                 label_tail = label_tail),
             rks = as.integer(expand_age_pop(rks, method = "natural")$y),
             death = as.integer(expand_age_pop(death, method = "natural")$y)),
             by = .(year, sex)]
  x <- x[, .(rks = sum(rks), death = sum(death)), by = .(year, sex, agegrp)]
  # complete structure of the population data-set
  year_value <- unique(x$year)
  eagegrp <- unique(x$agegrp)
  full_pop <- CJ(year = year_value, sex = c(1, 2), agegrp = eagegrp)
  res <- merge(full_pop, x, by = c("year", "sex", "agegrp"), all.x = TRUE)
  res[is.na(rks), rks := 0]
  res_miss_pop <- res[rks == 0]
  year_miss <- unique(res_miss_pop$year)
  res <- res[!year %in% year_miss]
  class(res) <- c("population", class(res))
  return(res)
}
