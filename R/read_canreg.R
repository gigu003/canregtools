#' Read cancer registration data in the format reported to the National Cancer
#' Center.
#'
#' @param x Address of cancer registration data, it could be a specific excel
#'        file, or a folder contains a series of Excel files.
#' @param pop_type Format of population sheet, options area 'long' or 'wide',
#'        default is 'long'.
#'
#' @return An object of canreg class or a list of objects of canreg class.
#' @export
#'
read_canreg <- function(x, pop_type = "long") {
  if (unique(file.info(x)$isdir)) {
    files <- list.files(x)
    isxls <- tolower(tools::file_ext(files)) %in% c("xls", "xlsx")
    files <- files[isxls]
    names <- tools::file_path_sans_ext(basename(files))
    if (length(files) == 0) {
      stop("No Excel files found in the specified directory.")
    }
    files <- paste0(x, "/", files)
    res <- purrr::map(files, read_file, pop_type = pop_type,
                      .progress = "Reading canreg data #")
    names(res) <- names
    class(res) <- c("canregs", "list")
    return(res)
  } else if (length(x)==1){
    read_file(x, pop_type = pop_type)
  } else if (length(x) > 1){
    isxls <- tolower(tools::file_ext(x)) %in% c("xls", "xlsx")
    files <- x[isxls]
    names <- tools::file_path_sans_ext(basename(files))
    res <- purrr::map(files, read_file, pop_type = pop_type)
    names(res) <- names
    class(res) <- c("canregs", "list")
    return(res)
  } else {
    stop("Input data type not supported. Please use the following:\n
          1. 1 file full address,\n
          2. file addresses,\n
          3. folder.\n")
  }
}


get_year <- function(x, date_var = "inciden") {
  date_var <- rlang::sym(date_var)
  res <- x |> pull(!!date_var) |> format("%Y") |> unique() |> as.integer() |> 
    na.omit()
  return(res)
}



read_file <- function(x, pop_type = "long", pop_var="popu", death_var="death") {
  tryCatch({
    fb <- read_sheet(x, sheet = "FB")
    sw <- read_sheet(x, sheet = "SW")
    # Extract year information
    fyear <- get_year(fb, date_var = "inciden")
    syear <- get_year(sw, date_var = "deathda")
    year <- unique(c(fyear, syear))
    if (pop_type == "wide") { pop <- read_single_pop(x, year = fyear) }
    else if (pop_type == "long") { pop <- read_long_pop(x) }
    attr(fb, "class") <- c("FBcases", class(fb))
    attr(sw, "class") <- c("SWcases", class(sw))
    attr(pop, "class") <- c("POP", class(pop))
    bsname <- tools::file_path_sans_ext(basename(x))
    areacode <- gsub("\\D", "", bsname)
    res <- list(areacode = areacode, FBcases = fb, SWcases = sw, POP = pop)
    class(res) <- c("canreg", "list")
    return(res)},
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL) }
  )
}

read_sheet <- function(x, sheet= "FB") {
  date_vars <- rlang::syms(c("inciden", "deathda", "birthda"))
  res <- read_excel(x, sheet = sheet) |>
    rename_all(tolower) |>
    mutate(across(c(!!!date_vars), as.Date))
  return(res)
}

read_single_pop <- function(x, year) {
  pop <- read_excel(x, sheet = "POP", range = "B2:C20",
                    col_names = c("male", "female"))
  pop <- round(pop)
  pop <- tibble(
    year = as.integer(c(rep(year, 38))),
    sex = as.integer(c(rep(1L, 19), rep(2L, 19))),
    agegrp = factor(c(rep(1:19, 2))),
    rks = as.integer(c(pop[[1]], pop[[2]]))
    )
  return(pop)
}

read_long_pop <- function(x, pop_var = "popu", age_var= "agegroup") {
  strat_vars <- rlang::syms(c("year", "sex"))
  pop_var <- rlang::sym(pop_var)
  age_var <- rlang::sym(age_var)
  rks <- rlang::sym("rks")
  agegrp <- rlang::sym("agegrp")
  pop <- read_excel(x, sheet = "POP") |> 
    mutate(across(c(!!!strat_vars), as.integer),
           !!rks := as.integer(round(!!pop_var)),
           !!agegrp := factor(!!age_var)) |> 
    select(c(!!!strat_vars), !!agegrp, !!rks)
  return(pop)
}

