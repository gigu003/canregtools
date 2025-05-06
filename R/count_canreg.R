#' Count canreg data.
#'
#' @rdname count_canreg
#' @param x Object data of class 'canreg' or 'canregs'.
#' @param age_breaks Specify the break points classify age groups. Default is
#'        `c(0, 1, seq(0, 85, 5))`.
#' @param label_tail Tail label to be added to the labels. Default is NULL.
#' @inheritParams cancer_type
#' @return data of class fbswicd.
#' @export
count_canreg <- function(x,
                         age_breaks = c(0, 1, seq(5, 85, 5)),
                         label_tail = NULL,
                         cancer_type = "big") {
  UseMethod("count_canreg", x)
}

#' @rdname count_canreg
#' @method count_canreg canregs
#' @param ... Parameters.
#' @export
count_canreg.canregs <- function(x, ...) {
  res <- purrr::map(x,
                    count_canreg,
                    ...,
                    .progress = "Counting canregs #")
  #res <- lapply(x, count_canreg, ...)
  class(res) <- c("fbswicds", "list")
  return(res)
}

#' @rdname count_canreg
#' @method count_canreg canreg
#' @export
#'
#' @examples
#' library(canregtools)
#' data("canregs")
#' fbsw <- count_canreg(canregs)
#' fbsw
count_canreg.canreg <- function(x,
                                age_breaks = c(0, 1, seq(5, 85, 5)),
                                label_tail = NULL,
                                cancer_type = "big") {
  format <- cr_clean(x, age_breaks = age_breaks, label_tail = label_tail,
                     cancer_type = cancer_type)
  fb <- format$FBcases
  sw <- format$SWcases
  
  
  year <- rlang::sym("year")
  sex <- rlang::sym("sex")
  cancer <- rlang::sym("cancer")
  sumvars <- c("fbs", "sws", "mv", "dco", "m8000", "ub", "sub")
  
  ubs <- c("C26", "C39", "C48", "C76", "C77", "C78", "C79", "C80", "C97")
  basi <- rlang::sym("basi")
  icd10 <- rlang::sym("icd10")
  morp <- rlang::sym("morp")
  byvars <- c("year", "sex", "cancer", "agegrp")
  
  
  c44 <- rlang::sym("c44")
  fbs <- rlang::sym("fbs")
  mv <- rlang::sym("mv")
  ub <- rlang::sym("ub")
  sub <- rlang::sym("sub")
  m8000 <- rlang::sym("m8000")
  dco <- rlang::sym("dco")
  
  fb <- fb |>
    filter(!is.na(!!year), !is.na(!!sex), !is.na(!!cancer))
  res_sw <- sw |>
    filter(!is.na(!!year), !is.na(!!sex), !is.na(!!cancer)) |>
    mutate(!!c44 := as.integer(substr(!!icd10, 1, 3) == "C44")) |>
    count(!!!rlang::syms(byvars), !!c44, name = "sws")
  res <- fb |>
    mutate(!!fbs := 1L,
           !!c44 := as.integer(substr(!!icd10, 1, 3) == "C44"),
           !!mv := as.integer(!!basi %in% 5:7),
           !!dco := as.integer(!!basi == 0),
           !!m8000 := as.integer(!!morp %in% c("8000", "80001")),
           !!ub := as.integer(substr(!!icd10, 1, 3) %in% ubs),
           !!sub := as.integer(substr(!!icd10, 5, 5) == "9")) |> 
    group_by(!!!rlang::syms(byvars), !!c44) |>
    reframe(across(all_of(setdiff(sumvars, "sws")), sum)) |>
    left_join(res_sw, by = c(byvars, "c44"))
  
  # Construct the `fbswicd`
  fbswicd <- bind_rows(
    group_by(res, !!!rlang::syms(byvars)) |>
      reframe(across(all_of(sumvars), sum)),
    group_by(res, !!!rlang::syms(setdiff(byvars, "cancer"))) |>
      reframe(across(all_of(sumvars), sum)) |>
      mutate(!!cancer := "60"),
    filter(res, !!c44 == 0) |>
      group_by(!!!rlang::syms(setdiff(byvars, "cancer"))) |>
      reframe(across(all_of(sumvars), sum)) |>
      mutate(!!cancer := "61"))
  fbswicd <- expand_grid(year = unique(fbswicd$year),
                         sex = unique(fbswicd$sex),
                         cancer = unique(fbswicd$cancer),
                         agegrp = factor(levels(res$agegrp),
                                         levels = levels(res$agegrp))) |>
    left_join(fbswicd, by = byvars) |>
    mutate(across(all_of(sumvars), ~ coalesce(.x, 0)))
  #
  fbswicd <- fbswicd |>
    group_by(!!!rlang::syms(setdiff(byvars, "sex"))) |>
    reframe(across(all_of(sumvars), sum)) |>
    mutate(sex = 0L) |>
    bind_rows(fbswicd) |>
    select(!!!rlang::syms(byvars), !!!rlang::syms(sumvars)) |>
    arrange(!!!rlang::syms(byvars))


  # count the sub-sites and morphology codes
  sitemorp <- fb |>
    group_by(!!!rlang::syms(c("year", "sex", "cancer"))) |>
    reframe(site = list(ctp(!!icd10)),
            !!morp := list(ctp(!!morp)))
  res <- list(areacode = purrr::pluck(format, "areacode"),
              fbswicd = fbswicd,
              sitemorp = sitemorp,
              pop = purrr::pluck(format, "POP"))
  
  attr(res, "class") <- c("fbswicd", class(res))
  
  return(res)
}

ctp <- function(x) {
  res <- table(x)
  res_vec <- as.vector(res)
  names(res_vec) <- names(res)
  return(res_vec)
}
