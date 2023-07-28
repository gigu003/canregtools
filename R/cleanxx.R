cleanxx <- function(x,
                    type = "big",
                    length = 5,
                    maxage = 85,
                    sep_zero = T)
  UseMethod("cleanxx") 


cleanxx.FBcases <- function(x,
                              type = "big",
                              length = 5,
                              maxage = 85,
                              sep_zero = T){
  if (!inherits(x, "FBcases")) {
    stop("Input must be a valid incidence object.")
  }
  year <- as.numeric(format(x$inciden, "%Y"))
  sex <- as.integer(x$sex)
  age <- calc_age(x$birthda, x$inciden)
  agegrp <- cutage(age,
                   method = "distance",
                   length = length,
                   maxage = maxage,
                   sep_zero = sep_zero)
  basi <- as.integer(x$basi)
  icd10 <- toupper(x$icd10)
  icd_cat <- classify_icd10(icd10, type = type)
  res <- tibble(year, sex, agegrp, icd_cat, basi)
  return(res)
}

cleanxx.SWcases <- function(x,
                              type = "big",
                              length = 5,
                              maxage = 85,
                              sep_zero = T){
  if (!inherits(x, "SWcases")) {
    stop("Input must be a valid mortality object.")
  }
  year <- as.numeric(format(x$deathda, "%Y"))
  sex <- as.integer(x$sex)
  age <- calc_age(x$birthda, x$deathda)
  agegrp <- cutage(age, method = "distance",
                   length = length,
                   maxage = maxage,
                   sep_zero = sep_zero)
  icd10 <- toupper(x$icd10)
  icd_cat <- classify_icd10(icd10, type = type)
  res <- tibble(year, sex, agegrp, icd_cat)
  return(res)
}

cleanxx.population <- function(x){
  if (!inherits(x, "population")) {
    stop("Input must be a valid population object.")
  }
  year <- as.integer(x$year)
  sex <- as.integer(x$sex)
  agegrp <- factor(x$agegrp, labels = seq(1, 19, 1))
  rks <- as.integer(x$rks)
  res <- tibble(year, sex, agegrp, rks)
  return(res)
}