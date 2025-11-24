#' Read cancer registration data
#'
#' Reads cancer registry data from one or more Excel files or from
#' a directory containing Excel files. It extracts case and population data and
#' returns objects of class `"canreg"` or a list of such objects (`"canregs"`).
#'
#' @param x A path to an Excel file, a character vector of file paths, or
#'    a directory containing Excel files.
#' @param pop_type A character string specifying the format of the population
#'    sheet. Must be either `"long"` (default) or `"wide"`.
#' @param age_var Name of the age group column (used only when
#'    `pop_type = "long"`). Default is `"agegroup"`.
#' @param pop_var Name of the population count column (used only when
#'    `pop_type = "long"`). Default is `"popu"`.
#' @param death_var Name of the death count column (used only when
#'    `pop_type = "long"`). If not present in the sheet, a column with 0s will
#'    be added. Default is `"death"`.
#'
#' @return A `canreg` object (a list with components: `areacode`, `FBcases`,
#'   `SWcases`, `POP`) if a single file is read. A named list of such objects
#'   with class `"canregs"` if multiple files or a directory is provided.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file_address <- "410302.xlsx"
#' canreg <- read_canreg(file_address, pop_type = "long")
#' }
#'
read_canreg <- function(x,
                        pop_type = "long",
                        age_var = "agegroup",
                        pop_var = "popu",
                        death_var = "death"
                        ) {
  if (length(x) == 1 && dir.exists(x)) {
    files <- list.files(x, pattern = "\\.xlsx?$", full.names = TRUE)
    if (length(files) == 0) {
      stop("No Excel files found in the specified directory.")
    }
  } else {
    isxls <- tolower(tools::file_ext(x)) %in% c("xls", "xlsx")
    files <- x[isxls]
    if (length(files) == 0) {
      stop("No valid Excel files provided.")
    }
  }
  
  
  # Get the areacode in the file names
  names <- gsub("\\D", "", tools::file_path_sans_ext(basename(files)))
  
  # Read files
  res <- purrr::map(files,
                    read_file,
                    pop_type = pop_type,
                    age_var = age_var,
                    pop_var = pop_var,
                    death_var = death_var,
                    .progress = "Reading canreg data #"
                    )
  names(res) <- names
  res <- purrr::compact(res)
  
  if (length(res) == 1 && inherits(res[[1]], "canreg")) {
    return(res[[1]])
  }
  
  structure(res, class = c("canregs", "list"))
}

#' Read and process a cancer registry Excel file
#'
#' This function reads and standardizes data from a cancer registry Excel file.
#' It extracts FB and SW case data, population data (in long or wide format),
#' and derives the year and area code from file contents and name.
#'
#' @param x A character string giving the path to an Excel file.
#' @param pop_type Character; format of population data.
#'    Either `"long"` (default) or `"wide"`.
#' @param age_var Name of the age group column (used only when
#'    `pop_type = "long"`). Default is `"agegroup"`.
#' @param pop_var Name of the population count column (used only when
#'    `pop_type = "long"`). Default is `"popu"`.
#' @param death_var Name of the death count column (used only when
#'    `pop_type = "long"`). If not present in the sheet, a column with 0s
#'    will be added. Default is `"death"`.
#'
#' @return A list of class `"canreg"` containing:
#'   \describe{
#'     \item{areacode}{Character string extracted from the file name (digits
#'      only).}
#'     \item{FBcases}{A data frame of FB cases, with class `"FBcases"`.}
#'     \item{SWcases}{A data frame of SW cases, with class `"SWcases"`.}
#'     \item{POP}{A population data frame of class `"POP"` depending on the
#'      format.}
#'   }
#' @keywords internal
#' @noRd
read_file <- function(x,
                      pop_type = "long",
                      age_var = "agegroup",
                      pop_var = "popu",
                      death_var = "death") {
  tryCatch(
    {
      fb <- read_sheet(x, sheet = "FB")
      sw <- read_sheet(x, sheet = "SW")
      fyear <- get_year(fb, date_var = "inciden")
      
      pop <- switch(
        pop_type,
        "wide" = read_single_pop(x, year = fyear),
        "long" = read_long_pop(x,
                               pop_var = pop_var,
                               age_var = age_var,
                               death_var = death_var),
        stop("`pop_type` only can be 'long' or 'wide'")
      )
      
      bsname <- tools::file_path_sans_ext(basename(x))
      areacode <- gsub("\\D", "", bsname)
      
      structure(
        list(
          areacode = areacode,
          FBcases = fb,
          SWcases = sw,
          POP = pop
        ),
        class = c("canreg", "list")
      )
    },
    error = function(e) {
      message("Reading'", x, "' error occured:", conditionMessage(e))
      NULL
    }
  )
}

#' Read cancer registry sheet and standardize date columns
#'
#' Internal helper function to read either "FB" or "SW" sheet from an Excel
#' file. Automatically converts common date variables to Date type and
#' assigns class "FBcases" or "SWcases" as appropriate.
#'
#' @param x A file path to an Excel file.
#' @param sheet Sheet name to read ("FB" or "SW").
#' @param date_vars Character vector of column names to be parsed as Date.
#'
#' @return A data frame with standardized column names and date types, 
#'   and class "FBcases" or "SWcases" (if applicable).
#'
#' @keywords internal
#' @noRd
read_sheet <- function(x,
                       sheet = "FB",
                       date_vars = c("inciden", "deathda", "birthda")) {
  # Assign appropriate class
  sheet_class <- switch(
    sheet,
    "FB" = "FBcases",
    "SW" = "SWcases",
    NULL
  )
  
  # Read sheet and standardize column names
  res <- readxl::read_excel(x, sheet = sheet) |>
    dplyr::rename_all(tolower)
  
  # Only convert columns that exist in the data
  date_vars_present <- intersect(tolower(date_vars), names(res))
  if (length(date_vars_present) > 0) {
    res <- dplyr::mutate(res,
                         dplyr::across(dplyr::any_of(date_vars_present),
                                       as.Date))
  }
  
  if (!is.null(sheet_class)) {
    res <- structure(res, class = c(sheet_class, class(res)))
  }
  res
}

#' Read population data in wide format from POP sheet
#'
#' Internal helper function to extract population counts from a "POP" sheet
#' where population counts for males and females are stored in two columns.
#' The function assumes a fixed range `B2:C20` with 19 age groups per sex.
#'
#' @param x A file path to an Excel file containing the "POP" sheet.
#' @param year An integer or vector indicating the year associated with the
#'    population data.
#'
#' @return A tibble with columns: year, sex (1 = male, 2 = female),
#'    agegrp (factor), rks (population count).
#'
#' @keywords internal
#' @noRd
read_single_pop <- function(x, year) {
  # Validate year input
  if (!is.numeric(year) || length(year) != 1) {
    stop("`year` must be a single numeric value.")
  }
  
  # Try reading the population sheet
  pop <- tryCatch(
    readxl::read_excel(
      x,
      sheet = "POP",
      range = "B2:C20",
      col_names = c("male", "female")
    ),
    error = function(e) {
      stop("Failed to read 'POP' sheet from file: ",
           x, "\n",
           conditionMessage(e))
    }
  )
  
  # Ensure correct number of rows
  if (nrow(pop) != 19) {
    stop("Expected 19 age groups in population sheet (B2:C20).")
  }
  
  # Round and validate values
  pop <- round(pop)
  if (anyNA(pop)) {
    message(paste0("Missing (NA) values found in population data.",
                   "They will be treated as zeros.")
            )
    pop[is.na(pop)] <- 0L
  }
  
  # Construct tibble with age groups and sex
  res <- dplyr::tibble(
    year   = rep(as.integer(year), 38),
    sex    = rep(c(1L, 2L), each = 19),
    agegrp = factor(rep(1:19, 2)),
    rks    = as.integer(c(pop$male, pop$female))
  )
  structure(res, class = c("POP", class(res)))
}


#' Read population data in long format from POP sheet
#'
#' Internal helper to read and clean population data from the "POP" sheet of a
#' cancer registry Excel file. It ensures required variables exist, rounds numeric
#' population and death counts, and extracts clean age group identifiers.
#'
#' If the specified death variable column is not present, a column with value 0
#' will be added automatically.
#'
#' @param x A file path to an Excel file containing the "POP" sheet.
#' @param pop_var Name of the population count column. Default is `"popu"`.
#' @param age_var Name of the age group column. Default is `"agegroup"`.
#' @param death_var Name of the death count column. If missing, a zero-filled
#'        column is created. Default is `"death"`.
#'
#' @return A data frame with columns: year, sex, agegrp (factor),
#'    rks (population), death.
#' 
#' @keywords internal
#' @noRd
read_long_pop <- function(x,
                          pop_var = "popu",
                          age_var = "agegroup",
                          death_var = "death") {
  # Deal with basic variables
  strat_vars <- rlang::syms(c("year", "sex"))
  pop_sym <- rlang::sym(pop_var)
  age_sym <- rlang::sym(age_var)
  death_sym <- rlang::sym(death_var)
  rks <- rlang::sym("rks")
  death <- rlang::sym("death")
  agegrp <- rlang::sym("agegrp")
  
  # Read pop data from excel file
  pop <- readxl::read_excel(x, sheet = "POP")
  
  # Create column death_var if it does not exist
  if (!(death_var %in% names(pop))) {
    pop[[death_var]] <- 0L
  }
  
  # Check existence of variables
  required_cols <- c("year", "sex", pop_var, age_var)
  missing_cols <- dplyr::setdiff(required_cols, names(pop))
  if (length(missing_cols) > 0) {
    stop("Missing columns in 'POP' sheet: ", paste(missing_cols, collapse = ", "))
  }
  
  # Return the cleaned result
  res <- pop |>
    dplyr::mutate(
      dplyr::across(c(!!!strat_vars), as.integer),
      !!rks := as.integer(round(!!pop_sym)),
      !!death := as.integer(round(!!death_sym)),
      !!agegrp := factor(as.numeric(gsub("\\D", "", !!age_sym)))
    ) |>
    dplyr::select(!!!strat_vars, !!agegrp, !!rks, !!death)
  structure(res, class = c("POP", class(res)))
}
