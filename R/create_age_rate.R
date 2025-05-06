#' Create age specific rate
#'
#' @description Creates a data frame of age-specific rates based on the class of
#'   data (`fbswicd` or `canreg`).
#'
#' @inheritParams data
#' @inheritParams strat_vars
#' @inheritParams event
#' @inheritParams cancer_type
#' @param format Format of the output data frame, either "long" or "wide".
#' @inheritParams mp_decimal
#' @param show_pop Logical value whether output population or not.
#' @param collapse Logical value whether output result as age_rate or age_rates. 
#' @return A data frame of age-specific rates.
#' @export
create_age_rate <- function(x,
                            ...,
                            event = "fbs",
                            cancer_type = "big",
                            format = "long",
                            mp = 100000,
                            decimal = 6,
                            show_pop = FALSE,
                            collapse = TRUE) {
  UseMethod("create_age_rate", x)
}


#' @rdname create_age_rate
#' @method create_age_rate canreg
#' @export
create_age_rate.canreg <- function(x,
                                   ...,
                                   cancer_type = "big"){
  data__ <- count_canreg(x, cancer_type = cancer_type)
  res <- create_age_rate.fbswicd(data__, ...)
  class(res) <- c("age_rate", "tbl_df", "tbl", "data.frame")
  return(res)
}

#' @rdname create_age_rate
#' @method create_age_rate canregs
#' @export
create_age_rate.canregs <- function(x,
                                   ...,
                                   cancer_type = "big",
                                   collapse = TRUE){
  data__ <- count_canreg.canregs(x, cancer_type = cancer_type)
  res <- create_age_rate.fbswicds(data__, ..., collapse = collapse)
  class(res) <- c("age_rates", "list")
  return(res)
}

#' @method create_age_rate fbswicds
#' @rdname create_age_rate
#' @export
create_age_rate.fbswicds <- function(x,
                                     ...,
                                     event = "fbs",
                                     format = "long",
                                     mp = 100000,
                                     decimal = 6,
                                     show_pop = FALSE,
                                     collapse = TRUE
                                     ) {
  event <- rlang::sym(event)
  res <- purrr::map(x,
                    create_age_rate.fbswicd,
                    ...,
                    event = !!event,
                    format = format,
                    mp = mp,
                    decimal =decimal,
                    show_pop = show_pop,
                    .progress = "Calculating age rate #")
  class(res) <- c("age_rates", "list")
  if (collapse){res <- cr_merge(res)}
  return(res)
}

#' @method create_age_rate fbswicd
#' @rdname create_age_rate
#' @export
create_age_rate.fbswicd <- function(x,
                                    ...,
                                    event = "fbs",
                                    format = "long",
                                    mp = 100000,
                                    decimal = 6,
                                    show_pop = FALSE) {

  rks <- rlang::sym("rks")
  agegrp <- rlang::sym("agegrp")
  agegrp2 <- rlang::sym("agegrp2")
  sex <- rlang::sym("sex")
  cases <- rlang::sym("cases")
  rate <- rlang::sym("rate")

  # Deal with population data
  gvars <- rlang::enquos(...) |>
    purrr::map_chr(rlang::as_name)
  sex_not_pass <- ifelse("sex" %nin% gvars, TRUE, FALSE)
  if ("sex" %nin% gvars) { gvars <- c(gvars, "sex") }
  gvars2 <- setdiff(gvars, c("cancer", "agegrp"))

  fbsw <- pre_deal_fbswicd(x, ...)
  pop_modi <- pre_deal_pop(x, ...)
  
  output <- fbsw |>
    group_by(!!!rlang::syms(gvars), !!agegrp) |>
    reframe(across(c({{event}}), ~ sum(.x))) |>
    left_join(pop_modi, by = c(gvars2, "agegrp")) |>
    mutate(!!cases := {{event}},
           !!rate := round(mp * {{event}} / !!rks, decimal)) |>
    select(-{{ event }})
  
  if (format == "long") {
    if (!show_pop) {
      output <- output |> 
        select(-starts_with("rks")) }
    } else if (format == "wide") {
    output <- output  %>%
      mutate(!!agegrp2 := as.numeric(!!agegrp)) %>%
      select(-!!agegrp) %>% 
      rename(f = cases, r = rate, p = rks) %>%
      pivot_wider(
        names_from = !!agegrp2,
        names_sep = "",
        values_from = c("f", "p", "r"),
        values_fill = 0)
    
    output <- output |> 
      rowwise() |>  
      mutate(!!rlang::sym("f0") := sum(c_across(starts_with("f"))),
             !!rlang::sym("p0") := sum(c_across(starts_with("p"))),
             !!rlang::sym("r0") := round(!!rlang::sym("f0") / !!rlang::sym("p0") * mp,
                                        decimal)) |> 
      ungroup() |> 
      select(starts_with(c("year", "sex", "cancer")),
             starts_with("f0"), starts_with("f"),
             starts_with("p0"), starts_with("p"),
             starts_with("r0"), starts_with("r"))
    
    if (!show_pop) {output <- select(output, -starts_with("p")) }
    }
  
  # Deal with group vars
  output <- output |> 
    post_vars() |> 
    post_sex_specific_cancer()
  if(sex_not_pass) { output <- filter(output, !!sex == 0L) }
  attr(output, "class") <- c("age_rate", "tbl_df", "tbl", "data.frame")
  
  return(output)
}
