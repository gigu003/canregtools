#' Check the class of 'canreg'
#'
#' @rdname check_canreg
#' @param data Object of classs 'canreg'
#' @param pop_type Population data structure of class 'canreg'
#'
#' @return Logical value
#' @export
#'
check_canreg <- function(data, pop_type = "single"){
  UseMethod("check_canreg", data)
}


#' @rdname check_canreg
#' @method check_canreg canregs
#' @export
#'
check_canreg.canregs <- function(data, pop_type = "single") {
  res <- purrr::map(data, check_canreg.canreg, pop_type = pop_type)
  class(res) <- c("checks", class(res))
  return(res)
}

#' @rdname check_canreg
#' @method check_canreg canreg
#' @export
#'
check_canreg.canreg <- function(data, pop_type = "single") {
    check <-  list(
      SWcases = check_SWcases(data[["SWcases"]]),
      FBcases = check_FBcases(data[["FBcases"]]),
      POP = check_pop(data[["POP"]]))
  class(check) <- c("check", "list")
  #res <- list(check = check)
  return(check)
}


check_SWcases <- function(x){
  z_rows <- nrow(x) == 0
  r_vars <- c("sex", "birthda", "deathda", "icd10", "morp", "basi")
  m_vars <- setdiff(r_vars, colnames(x))
  m_vars <- length(m_vars) > 0
  res <- list(
    null_data = z_rows,
    required = m_vars
  )
  return(res)
}


check_FBcases <- function(x){
  z_rows <- nrow(x) == 0
  r_vars <- c("sex", "birthda", "inciden", "icd10", "morp", "basi")
  m_vars <- setdiff(r_vars, colnames(x))
  m_vars <- length(m_vars) > 0
  res <- list(
    null_data = z_rows,
    required = m_vars
  )
  return(res)
}
  
check_pop <- function(x, pop_type = "single") {
  z_rows <- nrow(x) == 0
  m_rows <- nrow(x) %% 38 != 0
  r_vars <- c("year", "sex", "agegrp", "rks", "death")
  if (pop_type == "single") {
    r_vars <- r_vars[-5]
  }
  m_vars <- setdiff(r_vars, colnames(x))
  m_vars <- length(m_vars) > 0
  
  sex_values <- !all(c(1, 2) %in% x[["sex"]])
  r_age_groups <- c(0, 1, seq(5, 85, 5))
  age_values <- !all(r_age_groups %in% x[["agegrp"]])
  rks_values <- any(x[["rks"]] < 0)
  if (pop_type == "single"){
    death_values <- FALSE
  } else {
    death_values <-  any(x[["death"]] < 0)
  }
  
  res <- list(
    null_data = z_rows,
    required = m_vars,
    sex = sex_values,
    age = age_values,
    rks =rks_values,
    death = death_values
  )
  
  return(res)
}
