check_group_vars <- function(x, ..., quiet = TRUE) {
  accept_vars <- names(tidy_areacode("410302"))
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
  accept_vars <- names(tidy_areacode("410302"))
  
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
  gvars <- rlang::enquos(...) |> purrr::map_chr(rlang::as_name)
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
#' @examples
#' ls_std_vars()
ls_std_vars <- function(){
  name <- purrr::keep(names(std_pop), ~ .x %nin% c("agegrp"))
  des <- c("Standard population in Chinese in 1964",
           "Standard population in Chinese in 1982",
           "Standard population in Chinese in 2000",
           "Segi's world standard population",
           "World standard population in 2000")
  vars <- tibble(Vars = name, Description = des)
  return(vars)
}

#' List names of variables produced by the summary function.
#'
#' @description
#' This function returns a predefined list of variable names that are 
#' typically produced by the summary function in data analysis workflows.
#'
#' @returns A `tibble` with two columns:
#'   - `Vars`: The name of the variable (e.g., "fbs").
#'   - `Description`: A detailed description of the variable.
#' @export
#'
#' @examples
#' ls_summary_vars()
ls_summary_vars <- function() {
  name <- c("fbs", "inci", "sws", "mort", "mi", "rks", "rks_year", "inci_vars",
            "miss_r_vars_inci", "mort_vars", "miss_r_vars_mort")
  des <- c("Cancer Incidence Cases", "Cancer Incidence Rate(1 per 100000)",
             "Cancer Mortality Cases", "Cancer Mortality Rate(1 per 100000)",
             "Mortality to Incidence Ratio(M:I)", 
             "Population Size Corresponding to the cancer cases",
             "Years Corresponding to the Population",
             "Variables in the FB Sheets",
             "Missed variables of necessary variable of FB sheet",
             "Variables in the SW Sheets",
             "Missed variables of necessary variable of SW sheet")
  vars <- tibble(Vars = name, Description = des)
  return(vars)
}


#' List names of variables could be used cr_reframe function.
#'
#' @returns A `tibble` with two columns:
#'   - `Vars`: The name of the variable (e.g., "fbs").
#'   - `Description`: A detailed description of the variable.
#' @export
#'
#' @examples
#' ls_reframe_vars()
ls_reframe_vars <- function() {
  name <- c("areacode", "registry", "province", "city", "area_type", "region")
  des <- c("China administrative division code",
           "Cancer Registry codes expressed as administrative division code",
           "Province expressed as administrative division code",
           "City expressed as administrative division code", 
           "Area type expressed as 6 digits code 910000 for city, 920000 for rural",
           "Region in China express as digits code")
  vars <- tibble(Vars = name, Description = des)
  return(vars)
}