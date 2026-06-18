#' Check the legality of group vars passed by dot dot dot
#'
#' @param x x
#' @param ... Dot dot dot parameters
#' @param quiet Run the function quietly or not.
#'
#' @returns Logical value
#' @noRd
check_group_vars <- function(x, ..., quiet = TRUE) {
  accept_vars <- names(classify_areacode("410302"))
  gvars <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  error_vars <- dplyr::setdiff(gvars, c(accept_vars, "year", "sex", "cancer"))
  res <- length(error_vars) > 0
  if (res && !quiet) {
    message(paste0(
      paste0(error_vars, collapse = ", "),
      " was not supported. \nAvailable options are ",
      paste0(c(accept_vars, "year", "sex", "cancer"),
        collapse = ", "
      )
    ))
  }
  return(!res)
}


#' Reframe the population data according to vars passed by dot dot dot.
#'
#' @param x Population data.
#' @param ... Dot dot dot.
#'
#' @returns Reframed population data.
#' @noRd
pre_deal_pop <- function(x, ...) {
  # Extract the population data from the input list
  pop <- purrr::pluck(x, "pop")
  
  # Retrieve the accepted area code variables from the areacode object
  accept_vars <- names(classify_areacode("410302"))

  # Capture the grouping variables passed in '...'
  # Convert them to names and remove the "cancer" variable from grouping
  gvars <- rlang::enquos(...) |>
    purrr::map_chr(rlang::as_name) |>
    dplyr::setdiff("cancer")
  if ("sex" %nin% gvars) gvars <- c(gvars, "sex")
  gvars1 <- gvars |> rlang::syms()

  # Determine variables that need to be summed by excluding
  # the accepted area codes and demographic variables like year, sex, agegrp
  sum_vars <- dplyr::setdiff(names(pop), c(accept_vars, "year", "sex", "agegrp")) |>
    rlang::syms()

  # Group the population data by the selected grouping variables and age group
  # Then sum the relevant variables and return the modified data frame
  pop_modi <- pop |>
    group_by(!!!gvars1, !!rlang::sym("agegrp")) |>
    reframe(across(c(!!!sum_vars), \(x) sum(x, na.rm = TRUE))) |>
    ungroup()

  # Deal with combine sex.
  if ("sex" %in% gvars) {
    gvars2 <- dplyr::setdiff(gvars, "sex") |> rlang::syms()
    pop_modi <- pop_modi |>
      group_by(!!!gvars2, !!rlang::sym("agegrp")) |>
      reframe(across(c(!!!sum_vars), \(x) sum(x, na.rm = TRUE))) |>
      ungroup() |>
      mutate(!!rlang::sym("sex") := 0L) |>
      bind_rows(pop_modi)
  }

  pop_modi
}

#' Prepare the fbswicd data before other calculation
#'
#' @param x Object with class of fbswicd
#' @param ... Dot dot dot
#'
#' @returns fbswicd data removed combined stats.
#' @noRd
pre_deal_fbswicd <- function(x, ...) {
  accept_vars <- names(classify_areacode("410302"))
  gvars <- rlang::enquos(...) |>
    purrr::map_chr(rlang::as_name)
  gvars <- c(gvars, "agegrp")
  if ("sex" %nin% gvars) {
    gvars_modi <- c(gvars, "sex") 
  } else {
    gvars_modi <- gvars
  }
  if ("cancer" %nin% gvars) {
    gvars_modi <- c(gvars_modi, "cancer") 
  } else {
    gvars_modi <- gvars_modi
  }
  fbswicd <- purrr::pluck(x, "fbswicd")
  sum_vars <- dplyr::setdiff(names(fbswicd),
                      c(accept_vars, "year", "sex", "cancer", "agegrp"))
  res <- fbswicd |>
    group_by(!!!rlang::syms(dplyr::setdiff(gvars_modi, "sex"))) |>
    reframe(across(all_of(sum_vars), \(x) sum(x, na.rm = TRUE))) |>
    mutate(sex = 0L) |>
    bind_rows(fbswicd) |>
    select(!!!rlang::syms(gvars_modi), !!!rlang::syms(sum_vars)) |>
    arrange(!!!rlang::syms(gvars_modi))
  
  # Deal with combine sites.
  if ("cancer" %nin% gvars) {
    res |>
      filter(!!rlang::sym("cancer") %nin% c(60, 61))
  } else {
    res
  }
}
