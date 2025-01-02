#' Create age specific rate
#'
#' @description
#' Creates a data frame of age-specific rates based on the class of data
#' (`fbswicd` or `canreg`).
#' 
#' @inheritParams data
#' @inheritParams strat_vars
#' @inheritParams event
#' @inheritParams cancer_type
#' @param format Format of the output data frame, either "long" or "wide".
#' @inheritParams mp_decimal
#' @param show_pop Logical value whether output population or not.
#' @return A data frame of age-specific rates.
#' @export
create_age_rate <- function(x,
                            ...,
                            event = fbs,
                            cancer_type = "big",
                            format = "long",
                            mp = 100000,
                            decimal = 6,
                            show_pop = FALSE) {
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
  return(res)
}


#' @method create_age_rate fbswicds
#' @rdname create_age_rate
#' @export
create_age_rate.fbswicds <- function(x,
                                     ...,
                                     event = fbs,
                                     format = "long",
                                     mp = 100000,
                                     decimal = 6,
                                     show_pop = FALSE
                                     ) {
  event <- rlang::enquo(event)
  res <- purrr::map(x,
                    create_age_rate.fbswicd,
                    ...,
                    event = !!event,
                    format = format,
                    mp = mp,
                    decimal =decimal,
                    show_pop = show_pop,
                    .progress = "Calculating age rate #")
  return(res)
}



#' @method create_age_rate fbswicd
#' @rdname create_age_rate
#' @export
create_age_rate.fbswicd <- function(x,
                                    ...,
                                    event = fbs,
                                    format = "long",
                                    mp = 100000,
                                    decimal = 6,
                                    show_pop = FALSE) {
  
  data_ <- x$fbswicd
  pop_raw <- x$pop
  rks <- rlang::sym("rks")
  agegrp <- rlang::sym("agegrp")


  # Deal with population data
  f_cancer <- rlang::sym("cancer")
  group_var <- rlang::enquos(...)
  pop_group_var <- purrr::keep(group_var, ~ rlang::as_name(.x) %in% c("year", "sex"))
  gvars <- purrr::map_chr(group_var, rlang::as_name)
  pop_gvars <- purrr::map_chr(pop_group_var, rlang::as_name)
  pop_modi <- pop_raw %>%
    group_by(!!!pop_group_var, !!agegrp) %>%
    reframe(across(c("rks"), ~ sum(.x))) %>%
    ungroup()

  # Check if "year" and "sex" columns exist in pop_modi
  #logi <- c("year", "sex") %in% colnames(pop_modi)
  #by_vars <- c("year", "sex")[logi]
  
  # Deal with combine sites.
  if ("cancer" %nin% gvars){
    data_ <- data_ |> 
      filter(!!f_cancer %nin% c(60, 61))
  }
  
  output <- data_  %>%
    group_by(..., !!agegrp)  %>%
    reframe(across(c({{event}}), ~ sum(.x)))  %>%
    left_join(pop_modi, by = c(pop_gvars, "agegrp")) %>%
    mutate(
      cases = {{ event }},
      rate = round(mp * {{ event }} / !!rks, decimal)) %>%
    select(-{{ event }})
  
  if (format == "long") {
    if (!show_pop) {
      output <- output |> 
        select(-starts_with("rks")) }

  } else if (format == "wide") {
    output <- output  %>%
      mutate(agegrp2 = as.numeric(!!agegrp)) %>%
      select(-!!agegrp) %>% 
      rename(f = cases, r = rate, p = rks) %>%
      pivot_wider(
        names_from = agegrp2,
        names_sep = "",
        values_from = c("f", "p", "r"),
        values_fill = 0)
    
    output <- output |> 
      rowwise() |>  
      mutate(f0 = sum(c_across(starts_with("f"))),
             p0 = sum(c_across(starts_with("p"))),
             r0 = round(f0 / p0 * mp, decimal)) |> 
      ungroup() |> 
      select(starts_with(c("year", "sex", "cancer")),
             starts_with("f0"), starts_with("f"),
             starts_with("p0"), starts_with("p"),
             starts_with("r0"), starts_with("r"))
    
    if (!show_pop) {
      output <- output |> 
        select(-starts_with("p")) }
    }
  
  # Deal with group vars
  output <- output |> 
    post_vars()
  
  return(output)
}
