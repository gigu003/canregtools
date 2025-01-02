#' Clean canreg data.
#'
#' @rdname clean_canreg
#' @param x Data of class 'FBcases', 'SWcases' or 'population'.
#' @param cutage_method Method for grouping ages. Options are 'distance',
#'        'interval', or 'quarter'. Default is 'distance'.
#' @param smooth_age Method for expanding age. Options are 'linear','natural',
#'        or 'periodic'. Default is 'natural'.
#' @param breaks Cut points for age groups when 'cutage_method' is 'interval'.
#'        Default is 'c(0, 15, 40, 65)'.
#' @param length Length of each age group when 'cutage_method' is 'distance'.
#'        Default is 5.
#' @param maxage Start age of the last age groups. Default is 85.
#' @param sep_zero Logical value TRUE or FALSE, if the 0-year age group treated
#'        as a separate group. Default is TRUE.
#' @param labels Labels for the age groups.
#' @param label_tail Tail of the labels.
#' @inheritParams cancer_type
#'
#' @return Class 'canreg'.
#' @export
#'
clean_canreg <- function(x,
                         cutage_method = "distance",
                         smooth_age = "natural",
                         breaks = c(0, 15, 40, 65),
                         length = 5,
                         maxage = 85,
                         sep_zero = TRUE,
                         labels = NULL,
                         label_tail = NULL,
                         cancer_type = "big"
                         ) {
  UseMethod("clean_canreg", x)
}


#' @rdname clean_canreg
#' @method clean_canreg FBcases
#' @export
clean_canreg.FBcases <- function(x,
                                 cutage_method = "distance",
                                 smooth_age = "natural",
                                 breaks = c(0, 15, 40, 65),
                                 length = 5,
                                 maxage = 85,
                                 sep_zero = TRUE,
                                 labels = NULL,
                                 label_tail = NULL,
                                 cancer_type = "big"){
  
  # Convert the input data.frame to a data.table if it's not already one
  x <- as.data.table(x)
  
  # Step 1: Select only the required columns first (this can improve efficiency)
  x <- x[, .(inciden, sex, birthda, basi, icd10, morp)]
  
  # Step 2: Perform the necessary transformations
  x[, year := as.integer(format(inciden, "%Y"))]
  x[, sex := tidy_sex(sex)]
  x[, age := calc_age(birthda, inciden)]
  x[, basi := as.integer(basi)]
  x[, icd10 := toupper(icd10)]
  x[, cancer := classify_icd10(icd10, cancer_type = cancer_type)]
  
  # Step 3: Age group transformation
  x[, agegrp := cutage(age, method = cutage_method, breaks = breaks,
                       length = length, maxage = maxage,
                       label_tail = label_tail, sep_zero = sep_zero)]
  x <- x[!is.na(cancer)]
  # Step 4: Return only the required columns
  res <- x[, .(year, sex, agegrp, basi, icd10, cancer, morp)]
  
  return(res)
}

#' @rdname clean_canreg
#' @method clean_canreg SWcases
#' @export
clean_canreg.SWcases <- function(x,
                                 cutage_method = "distance",
                                 smooth_age = "natural",
                                 breaks = c(0, 15, 40, 65),
                                 length = 5,
                                 maxage = 85,
                                 sep_zero = TRUE,
                                 labels = NULL,
                                 label_tail = NULL,
                                 cancer_type = "big"){
  x <- as.data.table(x)
  x <- x[, .(deathda, sex, birthda, icd10)]
  x[, year := as.integer(format(deathda, "%Y"))]
  x[, sex := tidy_sex(sex)]
  x[, age := calc_age(birthda, deathda)]
  x[, icd10 := toupper(icd10)]
  x[, cancer := classify_icd10(icd10, cancer_type = cancer_type)]
  x[, agegrp := cutage(age, method = cutage_method, breaks = breaks,
                       length = length, maxage = maxage,
                       label_tail = label_tail, sep_zero = sep_zero)]
  x <- x[!is.na(cancer)]
  res <- x[, .(year, sex, agegrp, icd10, cancer)]
  return(res)
}

#' @rdname clean_canreg
#' @method clean_canreg population
#' @export
clean_canreg.population <- function(x,
                                    cutage_method = "distance",
                                    smooth_age = "natural",
                                    breaks = c(0, 15, 40, 65),
                                    length = 5,
                                    maxage = 85,
                                    sep_zero = TRUE,
                                    labels = NULL,
                                    label_tail = NULL,
                                    cancer_type = "big"){
  x <- as.data.table(x)
  x[, sex := tidy_sex(sex)]
  x <- x[, .(agegrp = cutage(seq(0, 92, 1), 
                             method = cutage_method, 
                             breaks = breaks,
                             length = length, 
                             maxage = maxage, 
                             label_tail = label_tail, 
                             sep_zero = sep_zero),
             rks = as.integer(expand_age_pop(rks, method = smooth_age)$y)),
         by = .(year, sex)]
  res <- x[, .(rks = sum(rks)), by = .(year, sex, agegrp)]
  return(res)
}
