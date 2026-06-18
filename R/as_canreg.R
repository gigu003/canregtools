#' Construct a canreg object
#'
#' This function constructs a `canreg` object, which contains cancer registry data,
#' including incidence, mortality, and population information.
#' Users can provide either a raw `regi_data` data frame containing `inciden` and `deathda` columns,
#' or supply `incidence` and `mortality` data frames directly.
#'
#' @param regi_data A data frame containing raw registry data with at least the columns:
#'   - `inciden`: date of diagnosis
#'   - `deathda`: date of death  
#'   If provided, `incidence` and `mortality` will be automatically derived.
#' @param incidence A data frame containing incidence (case) data. If not provided,
#'   it can be derived from `regi_data`.
#' @param mortality A data frame containing mortality (death) data. If not provided,
#'   it can be derived from `regi_data`.
#' @param population A data frame containing population data. This is required.
#' @param year Optional numeric vector. If specified, only cases or deaths from these years
#'   will be included.
#' @param areacode Character. The administrative area code for the data. Default is `NULL`.
#'
#' @return A `canreg` object (S3 class `canreg`) which is a list containing:
#'   - `areacode`: area code
#'   - `FBcases`: incidence data (S3 class `FBcases`)
#'   - `SWcases`: mortality data (S3 class `SWcases`)
#'   - `POP`: population data (S3 class `POP`)
#'
#' @examples
#' \dontrun{
#' # Using raw registry data
#' canreg_obj <- as_canreg(
#'   regi_data = my_registry_df,
#'   population = my_population_df,
#'   year = 2023,
#'   areacode = "410100"
#' )
#'
#' # Using separate incidence and mortality data frames
#' canreg_obj <- as_canreg(
#'   incidence = my_incidence_df,
#'   mortality = my_mortality_df,
#'   population = my_population_df
#' )
#' }
#'
#' @export
as_canreg <- function(
    regi_data = NULL,
    incidence = NULL,
    mortality = NULL,
    population = NULL,
    year = NULL,
    areacode = NULL
) {
  check_required_vars <- function(data, vars, data_name) {
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0) {
      stop(
        "Missing required variables in `", data_name, "`: ",
        paste(missing_vars, collapse = ", "),
        call. = FALSE
      )
    }
    invisible(TRUE)
  }
  
  standardize_dates <- function(data, vars) {
    date_vars <- intersect(vars, names(data))
    for (var in date_vars) {
      data[[var]] <- as.Date(data[[var]])
    }
    data
  }
  
  # Derive incidence/mortality from regi_data
  if (!is.null(regi_data)) {
    required_vars <- c("sex", "birthda", "inciden", "deathda", "basi", "icd10", "morp", "topo")
    check_required_vars(regi_data, required_vars, "regi_data")
    
    regi_data <- standardize_dates(regi_data, c("birthda", "inciden", "deathda"))
    regi_data$f_year <- as.integer(format(regi_data$inciden, "%Y"))
    regi_data$s_year <- as.integer(format(regi_data$deathda, "%Y"))
    
    if (!is.null(year)) {
      incidence <- regi_data[regi_data$f_year %in% year, ]
      mortality <- regi_data[regi_data$s_year %in% year & !is.na(regi_data$deathda), ]
    } else {
      incidence <- regi_data
      mortality <- regi_data[!is.na(regi_data$deathda), ]
    }
    incidence$f_year <- NULL
    incidence$s_year <- NULL
    mortality$f_year <- NULL
    mortality$s_year <- NULL
  }
  
  # using incidence/mortality if they were passed by the responding parameter
  if (!is.null(incidence)) {
    check_required_vars(
      incidence,
      c("sex", "birthda", "inciden", "basi", "icd10", "morp", "topo"),
      "incidence"
    )
    incidence <- standardize_dates(incidence, c("birthda", "inciden", "deathda"))
    incidence <- structure(incidence, class = unique(c("FBcases", class(incidence))))
  } else {
    stop("Incidence data must be provided either via 'regi_data' or 'incidence'.")
  }
  
  if (!is.null(mortality)) {
    check_required_vars(
      mortality,
      c("sex", "birthda", "deathda", "icd10"),
      "mortality"
    )
    mortality <- standardize_dates(mortality, c("birthda", "inciden", "deathda"))
    mortality <- structure(mortality, class = unique(c("SWcases", class(mortality))))
  } else {
    stop("Mortality data must be provided either via 'regi_data' or 'mortality'.")
  }
  
  if (!is.null(population)) {
    check_required_vars(population, c("year", "sex", "agegrp", "rks"), "population")
    population <- structure(population, class = unique(c("POP", class(population))))
  } else {
    stop("Population data must be provided.")
  }
  
  # Construct 'canreg' object
  res <- structure(
    list(
      areacode = areacode,
      FBcases = incidence,
      SWcases = mortality,
      POP = population
    ),
    class = c("canreg", "list")
  )
  
  return(res)
}
