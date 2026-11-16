#' Construct a canreg object
#'
#' This function constructs a `canreg` object, which contains cancer registry data,
#' including incidence, mortality, and population information.
#' Users can provide either a raw `regi_data` data frame containing `inciden` and `deathda` columns,
#' or supply `inci` and `mort` data frames directly.
#'
#' @param regi_data A data frame containing raw registry data with at least the columns:
#'   - `inciden`: date of diagnosis
#'   - `deathda`: date of death
#'   If provided, `inci` and `mort` will be automatically derived.
#' @param inci A data frame containing incidence (case) data. If not provided,
#'   it can be derived from `regi_data`.
#' @param mort A data frame containing mortality (death) data. If not provided,
#'   it can be derived from `regi_data`.
#' @param pop A data frame containing population data. This is required.
#' @param year Optional vector of four-digit calendar years or inclusive
#'   `"YYYY-YYYY"` periods. If specified, only cases or deaths from those
#'   calendar years will be included, for both raw and separate input tables.
#'   Incidence uses `inciden` and mortality uses `deathda`; missing event dates
#'   are excluded when filtering. Population rows are not filtered.
#' @param regicode Character. A registry or area identifier for the data.
#'   It is stored as a character vector and is not limited to administrative
#'   division codes. Default is `NULL`.
#' @param regicode_var Optional character string naming the column that
#'   identifies registries in all supplied data frames. When provided, the
#'   data are split by this column and one `canreg` object is created per
#'   registry. A single registry still returns a `canreg`; multiple registries
#'   return a named `canregs` object.
#'
#' @return A `canreg` object (S3 class `canreg`) or, when multiple registry
#'   codes are found with `regicode_var`, a named `canregs` object. Each
#'   `canreg` contains `regicode`, `FBcases`, `SWcases`, and `POP`.
#'
#' @examples
#' \dontrun{
#' # Using raw registry data
#' canreg_obj <- as_canreg(
#'   regi_data = my_registry_df,
#'   pop = my_population_df,
#'   year = 2023,
#'   regicode = "410100"
#' )
#'
#' # Build one canreg per registry using a grouping column
#' canregs_obj <- as_canreg(
#'   inci = incidence_df,
#'   mort = mortality_df,
#'   pop = population_df,
#'   regicode_var = "registry_id"
#' )
#'
#' # Using separate incidence and mortality data frames
#' canreg_obj <- as_canreg(
#'   inci = my_incidence_df,
#'   mort = my_mortality_df,
#'   pop = my_population_df
#' )
#' }
#'
#' @export
as_canreg <- function(
  regi_data = NULL,
  inci = NULL,
  mort = NULL,
  pop = NULL,
  year = NULL,
  regicode = NULL,
  regicode_var = NULL
) {
  if (is.null(regicode_var)) {
    return(as_canreg_single(
      regi_data = regi_data,
      incidence = inci,
      mortality = mort,
      population = pop,
      year = year,
      regicode = regicode
    ))
  }

  validate_regicode_var(regicode_var)
  if (!is.null(regicode)) {
    stop(
      "`regicode` must be NULL when `regicode_var` is provided.",
      call. = FALSE
    )
  }

  if (!is.null(regi_data)) {
    validate_regicode_group_data(regi_data, "regi_data", regicode_var)
    validate_regicode_group_data(pop, "pop", regicode_var)

    codes <- regicode_codes(regi_data, regicode_var)
    check_regicode_code_sets(
      list(regi_data = regi_data, pop = pop),
      regicode_var,
      codes
    )

    regi_groups <- split_regicode_data(regi_data, regicode_var, codes)
    population_groups <- split_regicode_data(
      pop,
      regicode_var,
      codes
    )
    result <- Map(
      function(registry, registry_data, registry_population) {
        as_canreg_single(
          regi_data = registry_data,
          population = registry_population,
          year = year,
          regicode = registry
        )
      },
      codes,
      regi_groups,
      population_groups
    )
  } else {
    validate_regicode_group_data(inci, "inci", regicode_var)
    validate_regicode_group_data(mort, "mort", regicode_var)
    validate_regicode_group_data(pop, "pop", regicode_var)

    codes <- regicode_codes(inci, regicode_var)
    check_regicode_code_sets(
      list(
        inci = inci,
        mort = mort,
        pop = pop
      ),
      regicode_var,
      codes
    )

    incidence_groups <- split_regicode_data(inci, regicode_var, codes)
    mortality_groups <- split_regicode_data(mort, regicode_var, codes)
    population_groups <- split_regicode_data(
      pop,
      regicode_var,
      codes
    )
    result <- Map(
      function(
        registry,
        registry_incidence,
        registry_mortality,
        registry_population
      ) {
        as_canreg_single(
          incidence = registry_incidence,
          mortality = registry_mortality,
          population = registry_population,
          year = year,
          regicode = registry
        )
      },
      codes,
      incidence_groups,
      mortality_groups,
      population_groups
    )
  }

  names(result) <- codes
  if (length(result) == 1L) {
    return(result[[1L]])
  }

  structure(result, class = c("canregs", "list"))
}

as_canreg_single <- function(
  regi_data = NULL,
  incidence = NULL,
  mortality = NULL,
  population = NULL,
  year = NULL,
  regicode = NULL
) {
  check_required_vars <- function(data, vars, data_name) {
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0) {
      stop(
        "Missing required variables in `",
        data_name,
        "`: ",
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
    required_vars <- c(
      "sex",
      "birthda",
      "inciden",
      "deathda",
      "basi",
      "icd10",
      "morp",
      "topo"
    )
    check_required_vars(regi_data, required_vars, "regi_data")

    regi_data <- standardize_dates(
      regi_data,
      c("birthda", "inciden", "deathda")
    )
    incidence <- regi_data
    mortality <- regi_data[!is.na(regi_data$deathda), ]
  }

  # using incidence/mortality if they were passed by the responding parameter
  if (!is.null(incidence)) {
    check_required_vars(
      incidence,
      c("sex", "birthda", "inciden", "basi", "icd10", "morp", "topo"),
      "incidence"
    )
    incidence <- standardize_dates(
      incidence,
      c("birthda", "inciden", "deathda")
    )
    incidence <- standardize_year_sex(incidence)
    incidence <- structure(
      incidence,
      class = unique(c("FBcases", class(incidence)))
    )
  } else {
    stop(
      "Incidence data must be provided either via 'regi_data' or 'incidence'."
    )
  }

  if (!is.null(mortality)) {
    check_required_vars(
      mortality,
      c("sex", "birthda", "deathda", "icd10"),
      "mortality"
    )
    mortality <- standardize_dates(
      mortality,
      c("birthda", "inciden", "deathda")
    )
    mortality <- standardize_year_sex(mortality)
    mortality <- structure(
      mortality,
      class = unique(c("SWcases", class(mortality)))
    )
  } else {
    stop(
      "Mortality data must be provided either via 'regi_data' or 'mortality'."
    )
  }

  if (!is.null(population)) {
    check_required_vars(
      population,
      c("year", "sex", "agegrp", "rks"),
      "population"
    )
    population <- standardize_year_sex(population)
    population <- structure(
      population,
      class = unique(c("POP", class(population)))
    )
  } else {
    stop("Population data must be provided.")
  }
  regicode <- as.character(regicode)

  if (!is.null(year)) {
    selected_year <- as_canreg_year_filter(year)
    incidence <- incidence[
      as.integer(format(incidence$inciden, "%Y")) %in% selected_year,
    ]
    mortality <- mortality[
      as.integer(format(mortality$deathda, "%Y")) %in% selected_year,
    ]
  }

  # Construct 'canreg' object
  res <- structure(
    list(
      regicode = regicode,
      FBcases = incidence,
      SWcases = mortality,
      POP = population
    ),
    class = c("canreg", "list")
  )

  return(res)
}

validate_regicode_var <- function(regicode_var) {
  if (
    !is.character(regicode_var) ||
      length(regicode_var) != 1L ||
      is.na(regicode_var) ||
      !nzchar(regicode_var)
  ) {
    stop(
      "`regicode_var` must be a non-empty character string of length 1.",
      call. = FALSE
    )
  }
}

validate_regicode_group_data <- function(data, data_name, regicode_var) {
  if (!is.data.frame(data)) {
    stop(
      "`",
      data_name,
      "` must be a data frame when `regicode_var` is provided.",
      call. = FALSE
    )
  }
  if (!regicode_var %in% names(data)) {
    stop(
      "`",
      data_name,
      "` must contain the `",
      regicode_var,
      "` column.",
      call. = FALSE
    )
  }

  codes <- as.character(data[[regicode_var]])
  if (anyNA(codes) || any(!nzchar(trimws(codes)))) {
    stop(
      "`",
      regicode_var,
      "` in `",
      data_name,
      "` must not contain missing or empty values.",
      call. = FALSE
    )
  }
  invisible(data)
}

regicode_codes <- function(data, regicode_var) {
  unique(as.character(data[[regicode_var]]))
}

check_regicode_code_sets <- function(data, regicode_var, codes) {
  code_sets <- lapply(data, regicode_codes, regicode_var = regicode_var)
  matches <- vapply(code_sets, setequal, logical(1), y = codes)
  if (!all(matches)) {
    stop(
      "All data frames must contain the same set of `",
      regicode_var,
      "` values.",
      call. = FALSE
    )
  }
  invisible(codes)
}

split_regicode_data <- function(data, regicode_var, codes) {
  values <- as.character(data[[regicode_var]])
  result <- lapply(codes, function(code) {
    part <- data[values == code, , drop = FALSE]
    part[[regicode_var]] <- NULL
    part
  })
  names(result) <- codes
  result
}

as_canreg_year_filter <- function(year) {
  labels <- trimws(as.character(year))
  if (length(labels) == 0L || any(!is_valid_year_label(labels))) {
    stop(
      "`year` must contain four-digit calendar years or YYYY-YYYY periods.",
      call. = FALSE
    )
  }
  starts <- as.integer(substr(labels, 1L, 4L))
  ends <- starts
  has_end <- nchar(labels) == 9L
  ends[has_end] <- as.integer(substr(labels[has_end], 6L, 9L))
  unlist(Map(seq.int, starts, ends), use.names = FALSE)
}
