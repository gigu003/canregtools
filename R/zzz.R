#' Check the legality of group vars passed by dot dot dot
#'
#' @param x x
#' @param ... Dot dot dot parameters
#' @param quiet Run the function quietly or not.
#'
#' @returns Logical value
#'
check_group_vars <- function(x, ..., quiet = TRUE) {
  accept_vars <- names(classify_areacode("410302"))
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


#' Reframe the population data according to vars passed by dot dot dot. 
#'
#' @param x Population data.
#' @param ... Dot dot dot.
#'
#' @returns Reframed population data.
#'
pre_deal_pop <- function(x, ...) {
  # Retrieve the accepted area code variables from the areacode object
  accept_vars <- names(classify_areacode("410302"))
  
  # Extract the population data from the input list
  pop <- purrr::pluck(x, "pop")
  
  # Capture the grouping variables passed in '...'
  # Convert them to names and remove the "cancer" variable from grouping
  gvars <- rlang::enquos(...) |> 
    purrr::map_chr(rlang::as_name) |>
    setdiff("cancer")
  if ("sex" %nin% gvars) gvars <- c(gvars, "sex")
  gvars1 <- gvars |> rlang::syms()
  
  # Determine variables that need to be summed by excluding 
  # the accepted area codes and demographic variables like year, sex, agegrp
  sum_vars <- setdiff(names(pop), c(accept_vars, "year", "sex", "agegrp")) |> 
    rlang::syms()
  
  # Group the population data by the selected grouping variables and age group
  # Then sum the relevant variables and return the modified data frame
  pop_modi <- pop |>
    group_by(!!!gvars1, !!rlang::sym("agegrp")) |>
    reframe(across(c(!!!sum_vars), sum)) |>
    ungroup()

  # Deal with combine sex.
  if ("sex" %in% gvars){
    gvars2 <- setdiff(gvars, "sex") |> rlang::syms()
    pop_modi <- pop_modi |> 
      group_by(!!!gvars2, !!rlang::sym("agegrp")) |> 
      reframe(across(c(!!!sum_vars), sum)) |>
      ungroup() |> 
      mutate(!!rlang::sym("sex") := 0L) |> 
      bind_rows(pop_modi)
  }
  
  return(pop_modi)
}

#' Prepare the fbswicd data before other calculation
#'
#' @param x Object with class of fbswicd
#' @param ... Dot dot dot
#'
#' @returns fbswicd data removed combined stats.
#'
pre_deal_fbswicd <- function(x, ...) {
  gvars <- rlang::enquos(...) |>
    purrr::map_chr(rlang::as_name)
  # Deal with combine sites.
  if ("cancer" %nin% gvars) {
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
  name <- purrr::keep(names(dict_maps[["std_pop"]]), ~ .x %nin% c("agegrp"))
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
