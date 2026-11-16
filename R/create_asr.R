#' Calculate age-standardized rate (ASR)
#'
#' `create_asr()` calculate age-standardized rates (ASRs) from object with
#' class of `canreg`, `canregs`, `fbswicd`, or `fbswicds`. It supports
#' stratification by multiple variables, allows the specification of different
#' standard population structures, and provides flexibility in the inclusion
#' of variance, standard errors, confidence intervals, and population data.
#'
#' Standard populations use 19 age groups by default: age 0, 1--4, 5--9,
#' ..., and 85+. When the input uses 18 five-year age groups beginning with
#' 0--4, `create_asr()` automatically combines the age-0 and age-1--4
#' standard-population weights before calculating the ASR.
#'
#' Supply `age_breaks` to use another compatible grouping. By default, groups
#' must only combine the original standard-population groups; set
#' `std_method = "interpolate"` when a requested boundary splits one.
#'
#' @rdname create_asr
#' @template data
#' @template strat_vars
#' @template event
#' @param std Specify the standard population structure in the 'std_pop' data
#'   frame used for calculating standardized rates. When calculating
#'   standardized rates for multiple standard populations, specify std = c(segi,
#'   china).
#' @param age_breaks Numeric vector of lower bounds for input age groups. It
#'   must begin at 0 and end no later than 85. The final group is open-ended.
#'   If omitted, the bounds are inferred from `agegrp`.
#' @param std_method Method used to align standard-population weights with
#'   `age_breaks`: `"exact"` combines original groups, while
#'   `"interpolate"` estimates single-year weights before regrouping.
#' @param interpolation_method Interpolation method used when
#'   `std_method = "interpolate"`.
#' @template cancer_type
#' @template mp_decimal
#' @param show_pop Logical value whether output population or not.
#' @param show_se Logical value indicating whether to output standard errors.
#'   Standard errors use the same scale as the reported rates.
#' @param show_var Logical value indicating whether to output variances.
#'   Variances use the squared scale of the reported rates, so their square
#'   roots equal the corresponding standard errors before rounding.
#' @param show_ci Logical value whether output confidence(lower or upper bound)
#'   or not.
#' @param collapse Logical value whether output result as asr or asrs.
#'
#' @return A data frame or tibble contains the age standard rates and CIs.
#' @details For summary input, existing `sex = "0"` rows are retained once.
#'   Missing overall rows are computed from available male/female rows at each
#'   original stratum before grouping. If an overall row and both sex-specific
#'   rows are supplied, inconsistent totals cause an error. Overall-only input
#'   is supported; sex-specific rows are not inferred from overall values.
#'   Within each observed year/sex/cancer stratum, absent summary age rows
#'   are interpreted as zero counts and completed from matching population
#'   age groups. Entirely absent strata are not inferred. Supplied missing,
#'   non-finite, or negative counts cause an error; population rows are never
#'   invented or filled with zero.
#' @export
#'
#' @seealso \code{\link{ageadjust}} for age-adjusted rate calculations.
#'   \code{\link{truncrate}} for truncated rate calculations.
#'
#' @examples
#' data("canregs")
#' asr_inci <- create_asr(canregs, event = "fbs", year, sex, cancer)
#' asr_mort <- create_asr(canregs, event = "sws", year, sex, cancer)
#'
create_asr <- function(
  x,
  ...,
  event = "fbs",
  std = c("cn2000", "wld85"),
  age_breaks = NULL,
  std_method = c("exact", "interpolate"),
  interpolation_method = "linear",
  cancer_type = "big",
  mp = 100000,
  decimal = 2,
  show_pop = FALSE,
  show_var = FALSE,
  show_se = FALSE,
  show_ci = FALSE,
  collapse = TRUE
) {
  UseMethod("create_asr", x)
}

#' @rdname create_asr
#' @method create_asr canregs
#' @export
#' @examples
#' # calculate ASR based on object with class of `canregs`
#' asr <- create_asr(canregs, event = "sws", year, sex, cancer)
#'
create_asr.canregs <- function(
  x,
  ...,
  event = "fbs",
  std = c("cn2000", "wld85"),
  age_breaks = NULL,
  std_method = c("exact", "interpolate"),
  interpolation_method = "linear",
  cancer_type = "big",
  mp = 100000,
  decimal = 2,
  show_pop = FALSE,
  show_var = FALSE,
  show_se = FALSE,
  show_ci = FALSE,
  collapse = TRUE
) {
  data__ <- if (is.null(age_breaks)) {
    count_canreg(x, cancer_type = cancer_type)
  } else {
    count_canreg(x, age_breaks = age_breaks, cancer_type = cancer_type)
  }
  res <- create_asr.fbswicds(
    data__,
    ...,
    event = event,
    std = std,
    age_breaks = age_breaks,
    std_method = std_method,
    interpolation_method = interpolation_method,
    mp = mp,
    decimal = decimal,
    show_pop = show_pop,
    show_var = show_var,
    show_se = show_se,
    show_ci = show_ci,
    collapse = collapse
  )
  return(res)
}

asr_age_breaks <- function(agegrp, age_breaks = NULL) {
  age_levels <- levels(agegrp)
  if (is.null(age_levels)) {
    age_levels <- unique(as.character(agegrp))
  }

  if (!is.null(age_breaks)) {
    age_breaks <- validate_std_age_breaks(age_breaks)
    if (length(age_breaks) != length(age_levels)) {
      stop("`age_breaks` must have one lower bound for each input age group.")
    }
    return(age_breaks)
  }

  labels <- suppressWarnings(
    as.numeric(sub("^[^0-9]*([0-9]+).*$", "\\1", age_levels))
  )
  if (identical(labels, seq_len(19L))) {
    return(std_age_breaks())
  }
  if (identical(labels, seq_len(18L))) {
    return(c(0, seq(5, 85, 5)))
  }
  validate_std_age_breaks(labels)
}

asr_has_five_year_groups <- function(age_breaks) {
  identical(age_breaks, std_age_breaks()) ||
    identical(age_breaks, c(0, seq(5, 85, 5)))
}

#' @rdname create_asr
#' @method create_asr canreg
#'
#' @export
#' @examples
#' # calculate ASR based on object with class of `canreg`
#' data <- canregs[[1]]
#' # calculate ASR using default parameter
#' asr <- create_asr(data, year, sex, cancer)
#' head(asr)
#' # calculate ASR using multiple standard population
#' asr_multi_std <- create_asr(data, year, sex, cancer,
#'   std = c("cn82", "cn2000", "wld85")
#' )
#' head(asr_multi_std)
#' # calculate ASR with confidence interval
#' asr_with_ci <- create_asr(data, year, sex, cancer, show_ci = TRUE)
#' head(asr_with_ci)
#' # calculate ASR with population at risk
#' asr_with_pop <- create_asr(data, year, sex, cancer, show_pop = TRUE)
#' head((asr_with_pop))
#' # calculate ASR with standard errors
#' asr_with_se <- create_asr(data, year, sex, cancer, show_se = TRUE)
#' head(asr_with_se)
#'
create_asr.canreg <- function(
  x,
  ...,
  event = "fbs",
  std = c("cn2000", "wld85"),
  age_breaks = NULL,
  std_method = c("exact", "interpolate"),
  interpolation_method = "linear",
  cancer_type = "big",
  mp = 100000,
  decimal = 2,
  show_pop = FALSE,
  show_var = FALSE,
  show_se = FALSE,
  show_ci = FALSE
) {
  data__ <- if (is.null(age_breaks)) {
    count_canreg(x, cancer_type = cancer_type)
  } else {
    count_canreg(x, age_breaks = age_breaks, cancer_type = cancer_type)
  }
  res <- create_asr.fbswicd(
    data__,
    ...,
    event = event,
    std = std,
    age_breaks = age_breaks,
    std_method = std_method,
    interpolation_method = interpolation_method,
    mp = mp,
    decimal = decimal,
    show_pop = show_pop,
    show_var = show_var,
    show_se = show_se,
    show_ci = show_ci
  )
  return(res)
}

#' @rdname create_asr
#' @method create_asr fbswicds
#' @export
#' @examples
#' # calculate ASR based on object with class of `fbswicds`
#' # convert object with class of `canregs` to object with class of `fbswicds`
#' fbsws <- count_canreg(canregs)
#' asrs <- create_asr(fbsws, event = "sws", year, sex, cancer)
#'
create_asr.fbswicds <- function(
  x,
  ...,
  event = "fbs",
  std = c("cn2000", "wld85"),
  age_breaks = NULL,
  std_method = c("exact", "interpolate"),
  interpolation_method = "linear",
  mp = 100000,
  decimal = 2,
  show_pop = FALSE,
  show_var = FALSE,
  show_se = FALSE,
  show_ci = FALSE,
  collapse = TRUE
) {
  group_var <- rlang::enquos(...)
  registry_ids <- names(x)
  if (is.null(registry_ids)) {
    registry_ids <- purrr::map_chr(x, ~ as.character(.x$regicode))
  }
  skipped <- character()
  res <- purrr::map2(
    x,
    registry_ids,
    function(data, registry_id) {
      tryCatch(
        create_asr.fbswicd(
          data,
          !!!group_var,
          event = {{ event }},
          std = std,
          age_breaks = age_breaks,
          std_method = std_method,
          interpolation_method = interpolation_method,
          mp = mp,
          decimal = decimal,
          show_pop = show_pop,
          show_var = show_var,
          show_se = show_se,
          show_ci = show_ci
        ),
        canregtools_asr_population_error = function(error) {
          skipped <<- c(skipped, registry_id)
          NULL
        }
      )
    },
    .progress = "Calculating asr #"
  )
  if (length(skipped) > 0L) {
    warning(
      "Skipping registries without positive ASR population denominators: ",
      paste(skipped, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  res <- purrr::compact(res)
  if (length(res) == 0L) {
    stop(
      "No registry has valid positive population denominators for ASR.",
      call. = FALSE
    )
  }
  class(res) <- c("asrs", "list")
  if (collapse) {
    res <- cr_merge(res)
  }
  return(res)
}

#' @rdname create_asr
#' @method create_asr fbswicd
#' @export
#' @examples
#' # calculate ASR based on object with class of `fbswicd`
#' # convert object with class of `canreg` to object with class of `fbswicd`
#' fbsw <- count_canreg(canregs[[1]])
#' asr <- create_asr(fbsw, event = "sws", year, sex, cancer)
#'
create_asr.fbswicd <- function(
  x,
  ...,
  event = "fbs",
  std = c("cn2000", "wld85"),
  age_breaks = NULL,
  std_method = c("exact", "interpolate"),
  interpolation_method = "linear",
  mp = 100000,
  decimal = 2,
  show_pop = FALSE,
  show_var = FALSE,
  show_se = FALSE,
  show_ci = FALSE
) {
  if (!check_group_vars(x, ..., quiet = FALSE)) {
    stop("group vars not supported.")
  }
  sex <- rlang::sym("sex")
  event <- rlang::sym(event)
  agegrp <- rlang::sym("agegrp")
  pop <- rlang::sym("rks")
  std_names <- std
  stdvar1 <- rlang::sym(std_names[1])
  std_method <- match.arg(std_method)

  group_var <- rlang::enquos(...)
  gvars <- purrr::map_chr(group_var, rlang::as_name)
  sex_not_pass <- ifelse("sex" %nin% gvars, TRUE, FALSE)
  if ("sex" %nin% gvars) {
    gvars <- c(gvars, "sex")
  }
  gvars <- gvars |> dplyr::setdiff("agegrp")
  group_var <- gvars |> rlang::syms()
  gvars2 <- dplyr::setdiff(gvars, "cancer")
  group_var2 <- rlang::syms(gvars2)

  # Deal with population data
  pop_modi <- pre_deal_pop(x, ...)
  # Deal with combine sites.
  fbswicd <- pre_deal_fbswicd(x, ...)

  # Align standard-population weights with the input age groups.
  age_levels <- levels(fbswicd$agegrp)
  if (is.null(age_levels)) {
    age_levels <- unique(as.character(fbswicd$agegrp))
  }
  age_breaks <- asr_age_breaks(fbswicd$agegrp, age_breaks)
  five_year_groups <- asr_has_five_year_groups(age_breaks)
  if (!five_year_groups) {
    warning(
      "Truncated and cumulative rates are returned as NA for age groups other ",
      "than 0, 1-4, 5-9, ..., 85+ or 0-4, 5-9, ..., 85+."
    )
  }
  std_pop <- data.frame(agegrp = factor(age_levels, levels = age_levels))
  for (std_name in std_names) {
    std_pop[[std_name]] <- get_stdpop(
      std_name,
      age_breaks = age_breaks,
      method = std_method,
      interpolation_method = interpolation_method
    )
  }

  # Performing grouping, summarization, and distinct operations for 'fbswicd'
  data_pre <- fbswicd |>
    group_by(!!!group_var, !!agegrp) |>
    reframe(across(c(!!event), ~ sum(.x, na.rm = TRUE))) |>
    left_join(pop_modi, by = c(gvars2, "agegrp")) |>
    left_join(std_pop, by = c("agegrp")) |>
    filter(!is.na(!!agegrp))

  if (nrow(data_pre) > 0L) {
    pop_values <- data_pre[[rlang::as_name(pop)]]
    invalid_pop <- if (!is.numeric(pop_values)) {
      rep(TRUE, nrow(data_pre))
    } else {
      !is.finite(pop_values) | pop_values <= 0
    }
    if (any(invalid_pop)) {
      key_columns <- intersect(c(gvars, "agegrp"), names(data_pre))
      bad_keys <- data_pre[invalid_pop, key_columns, drop = FALSE]
      bad_detail <- paste(
        vapply(
          seq_len(nrow(bad_keys)),
          function(index) {
            paste(
              paste(
                names(bad_keys),
                as.character(bad_keys[index, ]),
                sep = "="
              ),
              collapse = ", "
            )
          },
          character(1)
        ),
        collapse = "; "
      )
      rlang::abort(
        paste0(
          "Registry ",
          as.character(x$regicode),
          " has missing or non-positive population denominators for ASR: ",
          bad_detail,
          "."
        ),
        class = "canregtools_asr_population_error"
      )
    }
  }

  coverage <- data_pre |>
    group_by(!!!group_var) |>
    dplyr::summarise(.age_count = dplyr::n_distinct(!!agegrp), .groups = "drop")
  if (any(coverage$.age_count != length(age_levels))) {
    rlang::abort(
      paste0(
        "Registry ",
        x$regicode,
        " does not cover every declared age group; missing population age-group rows cannot be treated as zero."
      ),
      class = "canregtools_asr_population_error"
    )
  }

  # calculate age standardized rate.
  res <- data_pre |>
    group_by(!!!group_var) |>
    reframe(
      across(
        !!event,
        list(no_cases = ~ sum(.x, na.rm = TRUE), pop = ~ sum(!!pop)),
        .names = "{.fn}"
      ),
      across(
        all_of(std_names),
        list(
          truncr = ~ if (five_year_groups) {
            truncrate(
              !!event,
              !!pop,
              stdpop = .x,
              sep_zero = length(age_breaks) == 19L,
              mp = mp,
              decimal = decimal
            )
          } else {
            NA_real_
          }
        ),
        .names = "{.fn}_{.col}"
      ),
      across(
        all_of(std_names),
        list(model = ~ list(ageadjust(!!event, !!pop, stdpop = .x, mp = mp))),
        .names = "{.col}"
      ),
      cumur = if (five_year_groups) {
        cumrate(
          !!event,
          !!pop,
          sep_zero = length(age_breaks) == 19L,
          mp = 100
        )[[1]]
      } else {
        NA_real_
      }
    ) |>
    as_tibble()
  for (std_name in std_names) {
    model <- res[[std_name]]
    res[[paste0("asr_", std_name)]] <- purrr::map_dbl(model, "asr")
    if (show_var || show_se) {
      variance <- purrr::map_dbl(model, "asr_var")
      if (show_var) {
        res[[paste0("asr_var_", std_name)]] <- variance * mp^2
      }
      if (show_se) {
        res[[paste0("asr_se_", std_name)]] <- sqrt(variance) * mp
      }
    }
    if (show_ci) {
      res[[paste0("asr_lower_", std_name)]] <- purrr::map_dbl(model, "asr_lci")
      res[[paste0("asr_upper_", std_name)]] <- purrr::map_dbl(model, "asr_uci")
    }
  }
  cr_model <- res[[std_names[1]]]
  res$cr <- purrr::map_dbl(cr_model, "cr")
  if (show_var || show_se) {
    variance <- purrr::map_dbl(cr_model, "cr_var")
    if (show_var) {
      res$cr_var <- variance * mp^2
    }
    if (show_se) {
      res$cr_se <- sqrt(variance) * mp
    }
  }
  if (show_ci) {
    res$cr_lower <- purrr::map_dbl(cr_model, "cr_lci")
    res$cr_upper <- purrr::map_dbl(cr_model, "cr_uci")
  }

  res <- res |>
    group_by(!!!group_var2) |>
    mutate(
      #across(contains("no_cases"),
      #  list(prop = ~ .x / max(.x) * 100),
      #  .names = "{.fn}"
      #),
      across(
        starts_with(c("asr", "cr", "cumur", "truncr")) &
          !contains("var"),
        ~ round(.x, digits = decimal)
      ),
      across(
        where(is.numeric) & !starts_with(c("cumur", "truncr")),
        ~ cr_replace_na(.x)
      )
    ) |>
    select(c(
      all_of(gvars),
      starts_with(c(
        "pop",
        "no_cases",
        "cr",
        "cr_var",
        "cr_se",
        "cr_lci",
        "cr_uci",
        "asr",
        "truncr",
        "cumur"
      ))
    ))

  # Remove columns of "population" if show_pop is set to FALSE
  if (!show_pop) {
    res <- select(res, -contains(c("pop")))
  }

  # Remove variance columns unless requested.
  if (!show_var) {
    res <- select(res, -contains(c("cr_var", "asr_var")))
  }

  # Remove standard-error columns unless requested.
  if (!show_se) {
    res <- select(res, -contains(c("cr_se", "asr_se")))
  }
  # Remove columns with "lower or upper" if show_ci is set to FALSE
  if (!show_ci) {
    res <- select(res, -contains(c("_lower", "_upper")))
  }

  # Deal with group vars
  res <- post_vars(res) |>
    post_sex_specific_cancer() |>
    arrange(!!!rlang::syms(c("year", "sex", "cancer")))
  # add rank and proportion
  if ("cancer" %in% gvars) {
    rank <- rlang::sym("rank")

    rank <- filter(res, !!rlang::sym("cancer") %nin% c(60L, 61L)) |>
      select(!!!group_var, !!rlang::sym("cr")) |>
      group_by(!!!group_var2) |>
      mutate(!!rank := dense_rank(desc(!!rlang::sym("cr")))) |>
      select(-!!rlang::sym("cr"))
    res <- res |>
      group_by(!!!group_var2) |>
      mutate(
        prop = round(
          !!rlang::sym("no_cases") / max(!!rlang::sym("no_cases")) * 100,
          2
        )
      ) |>
      left_join(rank, by = gvars)
  }

  if (sex_not_pass) {
    res <- filter(res, !!sex == "0")
  }

  # Adding class attributes to the result
  attr(res, "class") <- c("asr", "tbl_df", "tbl", "data.frame")

  return(res)
}
