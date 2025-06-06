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
                    cr_clean,
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
                            cancer_type = "big") {
  areacode <- purrr::pluck(x, "areacode")
  res <- purrr::map(x[2:4],
                    cr_clean,
                    age_breaks = age_breaks,
                    label_tail = label_tail,
                    cancer_type = cancer_type)
  res["areacode"] <- areacode
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
  
  year <- rlang::sym("year")
  sex <- rlang::sym("sex")
  age <- rlang::sym("age")
  birthda <- rlang::sym("birthda")
  inciden <- rlang::sym("inciden")
  basi <- rlang::sym("basi")
  icd10 <- rlang::sym("icd10")
  cancer <- rlang::sym("cancer")
  agegrp <- rlang::sym("agegrp")
  
  res <- x |>
    select(!!!rlang::syms(c("sex", "birthda", "inciden", "basi", "icd10", "morp"))) |>
    mutate(!!year := as.integer(format(!!inciden, "%Y")),
           !!sex := tidy_sex(!!sex),
           !!age := calc_age(!!birthda, !!inciden),
           !!basi := as.integer(!!basi),
           !!icd10 := toupper(!!icd10),
           !!cancer := classify_icd10(!!icd10, cancer_type = cancer_type),
           !!agegrp := cutage(!!age, method = "interval", breaks = age_breaks,
                            label_tail = label_tail)
           ) |>
    filter(!is.na(cancer), !(!!sex == 0),
           !( (!!sex == 1 & cancer %in% female_cancer) | 
                (!!sex == 2 & cancer %in% male_cancer) |
                !!sex == 0)) |>
    select(!!!rlang::syms(c("year", "sex", "agegrp", "basi", "icd10",
                            "cancer", "morp")))

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
  year <- rlang::sym("year")
  age <- rlang::sym("age")
  sex <- rlang::sym("sex")
  birthda <- rlang::sym("birthda")
  deathda <- rlang::sym("deathda")
  icd10 <- rlang::sym("icd10")
  cancer <- rlang::sym("cancer")
  agegrp <- rlang::sym("agegrp")
  res <- x |>
    select(!!sex, !!birthda, !!deathda, !!icd10) |>
    mutate(!!year := as.integer(format(!!deathda, "%Y")),
           !!sex := tidy_sex(!!sex),
           !!age := calc_age(!!birthda, !!deathda),
           !!icd10 := toupper(!!icd10),
           !!cancer := classify_icd10(!!icd10, cancer_type = cancer_type),
           !!agegrp := cutage(age, method = "interval", breaks = age_breaks,
                            label_tail = label_tail)
           ) |>
    filter(!is.na(!!cancer), !(!!sex == 0),
           !( (!!sex == 1 & !!cancer %in% female_cancer) | 
                (!!sex == 2 & !!cancer %in% male_cancer) |
                !!sex == 0)) |>
    select(year, !!sex, !!cancer, !!agegrp, !!icd10)
  
  class(res) <- c("SWcases", class(res))
  return(res)
}

#' @rdname cr_clean
#' @method cr_clean POP
#' @export
cr_clean.POP <- function(x,
                         age_breaks = c(0, 1, seq(5, 85, 5)),
                         label_tail = NULL,
                         cancer_type = "big"){
  year <-  rlang::sym("year")
  sex <- rlang::sym("sex")
  agegrp <- rlang::sym("agegrp")
  
  if ("death" %in% names(x)){
    sum_vars <- rlang::syms(c("rks", "death"))  
  } else {
    sum_vars <- rlang::syms(c("rks"))
  }
  reframe_agegrp <- cutage(seq(0, 92), method = "interval",
                           breaks = c(0, 1, seq(5 ,85, 5)),
                           label_tail = label_tail)
  if (nrow(x) == 0){
    res <- expand_grid(year = c(2021L), sex = c(1L, 2L),
                       agegrp = unique(reframe_agegrp)) |>
      mutate(rks = 0L, death = 0L)
    } else {
      res <- x |> 
      mutate(!!sex := tidy_sex(!!sex),
             !!year := as.integer(!!year)) |>
      group_by(!!year, !!sex) |>
      reframe(!!agegrp := reframe_agegrp,
              across(c(!!!sum_vars), ~ as.integer(
                expand_age_pop(.x,method = "natural")$y))
              ) |> 
      group_by(!!year, !!sex, !!agegrp) |> 
      reframe(across(c(!!!sum_vars), sum))
      uniyear <- as.integer(unique(pull(x, !!year)))
      uniagegrp <- unique(pull(res, !!agegrp))
      res <- expand_grid(year = uniyear,
                         sex = c(1L, 2L),
                         agegrp = uniagegrp) |>
        left_join(res, by = c("year", "sex", "agegrp")) |>
        mutate(across(c(!!!sum_vars), ~ dplyr::coalesce(.x, 0)))
  }

  class(res) <- c("POP", class(res))
  return(res)
}
