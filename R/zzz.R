check_group_vars <- function(x, ..., quiet = TRUE) {
  accept_vars <- names(tidy_areacode(purrr::pluck(x, "areacode")))
  gvars <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  error_vars <- setdiff(gvars, c(accept_vars, "year", "sex", "cancer"))
  res <- length(error_vars) > 0
  if (res & !quiet) {
    message(paste0(paste0(error_vars, collapse = ", "),
                   " was not supported. \nAvailable options are ",
                   paste0(c(accept_vars, "year", "sex", "cancer"),
                          collapse = ", ")))
  }
  return(!res)
}


pre_deal_pop <- function(x, ...) {
  # Retrieve the accepted area code variables from the areacode object
  accept_vars <- names(tidy_areacode(purrr::pluck(x, "areacode")))
  
  # Extract the population data from the input list
  pop <- purrr::pluck(x, "pop")
  
  # Capture the grouping variables passed in '...'
  # Convert them to names and remove the "cancer" variable from grouping
  gvars <- rlang::enquos(...) |> 
    purrr::map_chr(rlang::as_name) |>
    setdiff("cancer") |>
    rlang::syms()
  
  # Determine variables that need to be summed by excluding 
  # the accepted area codes and demographic variables like year, sex, agegrp
  sum_vars <- setdiff(names(pop), c(accept_vars, "year", "sex", "agegrp")) |> 
    rlang::syms()
  
  # Group the population data by the selected grouping variables and age group
  # Then sum the relevant variables and return the modified data frame
  pop_modi <- pop |>
    group_by(!!!gvars, !!rlang::sym("agegrp")) |>
    reframe(across(c(!!!sum_vars), sum)) |>
    ungroup()
  
  return(pop_modi)
}

pre_deal_fbswicd <- function(x, ...) {
  gvars <- rlang::enquos(...) |> 
    purrr::map_chr(rlang::as_name)
  # Deal with combine sites.
  if ("cancer" %nin% gvars){
    res <- purrr::pluck(x, "fbswicd") |> 
      filter(!!rlang::sym("cancer") %nin% c(60, 61))
  } else {
    res <- purrr::pluck(x, "fbswicd")
  }
  return(res)
}

#' list all standard population names available
#'
#' @return Character vector contain names of standard population.
#' @export
#'
ls_std_vars <- function(){
  purrr::keep(names(std_pop), ~ .x %nin% c("agegrp"))
}
