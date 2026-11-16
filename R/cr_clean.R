#' Clean canreg data.
#'
#' @rdname cr_clean
#' @param x Data of class 'FBcases', 'SWcases' or 'population'.
#' @param age_breaks Cut points for age groups. Default is
#'        `c(0, 1, seq(5, 85, 5))`.
#' @param label_tail Tail of the labels.
#' @template cancer_type
#' @importFrom rlang .data
#'
#' @return Class 'canreg'.
#' @details Population input must contain one complete standard 18- or
#'   19-group schedule per year and sex, with numeric lower age bounds in
#'   `agegrp`. Aligned groups are summed directly; finer groups use natural
#'   interpolation with source-group totals preserved. The final open group
#'   cannot be split. Empty population input remains empty.
#' @export
#' @examples
#' data("canregs")
#' data <- cr_clean(canregs)
#'
cr_clean <- function(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
) {
  UseMethod("cr_clean", x)
}

#' @rdname cr_clean
#' @method cr_clean canregs
#' @export
#' @examples
#' data <- cr_clean(canregs, cancer_type = "small")
#'
cr_clean.canregs <- function(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
) {
  res <- purrr::map(
    x,
    cr_clean,
    age_breaks = age_breaks,
    label_tail = label_tail,
    cancer_type = cancer_type
  )
  class(res) <- c("canregs", "list")
  return(res)
}


#' @rdname cr_clean
#' @method cr_clean canreg
#' @export
#' @examples
#' data <- cr_clean(canregs[[1]], cancer_type = "big")
#'
cr_clean.canreg <- function(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
) {
  regicode <- purrr::pluck(x, "regicode")
  res <- purrr::map(
    x[c("FBcases", "SWcases", "POP")],
    cr_clean,
    age_breaks = age_breaks,
    label_tail = label_tail,
    cancer_type = cancer_type
  )
  res["regicode"] <- regicode
  class(res) <- c("canreg", "list")
  return(res)
}

#' @rdname cr_clean
#' @method cr_clean FBcases
#' @export
#' @examples
#' fbcases <- purrr::pluck(canregs[[1]], "FBcases")
#' fbcases <- cr_clean(fbcases)
#'
cr_clean.FBcases <- function(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
) {
  female_cancer <- c(29:37, 114:117, 206, 320:325)
  male_cancer <- c(38:41, 118:119, 207, 326:328)

  year <- rlang::sym("year")
  sex <- rlang::sym("sex")
  age <- rlang::sym("age")
  birthda <- rlang::sym("birthda")
  inciden <- rlang::sym("inciden")
  basi <- rlang::sym("basi")
  icd10 <- rlang::sym("icd10")
  topo <- rlang::sym("topo")
  cancer <- rlang::sym("cancer")
  agegrp <- rlang::sym("agegrp")
  regi_num <- rlang::sym("regi_num")

  sel_vars <- c("sex", "birthda", "inciden", "basi", "icd10", "morp", "topo")
  stat_vars <- c(
    "regi_num",
    "year",
    "sex",
    "agegrp",
    "basi",
    "icd10",
    "cancer",
    "morp",
    "topo"
  )
  if (".canreg_index" %in% names(x)) {
    sel_vars <- c(sel_vars, ".canreg_index")
    stat_vars <- c(stat_vars, ".canreg_index")
  }
  if ("id" %in% names(x)) {
    sel_vars <- c(sel_vars, "id")
    stat_vars <- c(stat_vars, "id")
  }
  if ("mult" %in% names(x)) {
    sel_vars <- c(sel_vars, "mult")
    stat_vars <- c(stat_vars, "mult")
  }

  res <- x |>
    select(!!!rlang::syms(sel_vars)) |>
    mutate(
      !!regi_num := row_number(),
      !!year := as.character(format(!!inciden, "%Y")),
      !!sex := tidy_sex(!!sex),
      !!age := calc_age(!!birthda, !!inciden),
      !!basi := as.integer(!!basi),
      !!icd10 := toupper(!!icd10),
      !!cancer := classify_icd10(!!icd10, cancer_type = cancer_type),
      !!topo := classify_topo(!!topo),
      !!agegrp := cutage(
        !!age,
        method = "interval",
        breaks = age_breaks,
        label_tail = label_tail
      )
    ) |>
    select(!!!rlang::syms(stat_vars))

  class(res) <- c("FBcases", class(res))
  return(res)
}

#' @rdname cr_clean
#' @method cr_clean SWcases
#' @export
#' @examples
#' swcases <- purrr::pluck(canregs[[1]], "SWcases")
#' swcases <- cr_clean(swcases)
#'
cr_clean.SWcases <- function(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
) {
  year <- rlang::sym("year")
  age <- rlang::sym("age")
  sex <- rlang::sym("sex")
  birthda <- rlang::sym("birthda")
  deathda <- rlang::sym("deathda")
  icd10 <- rlang::sym("icd10")
  cancer <- rlang::sym("cancer")
  agegrp <- rlang::sym("agegrp")
  regi_num <- rlang::sym("regi_num")
  res <- x |>
    select(
      !!sex,
      !!birthda,
      !!deathda,
      !!icd10,
      all_of(intersect(".canreg_index", names(x)))
    ) |>
    mutate(
      !!regi_num := row_number(),
      !!year := as.character(format(!!deathda, "%Y")),
      !!sex := tidy_sex(!!sex),
      !!age := calc_age(!!birthda, !!deathda),
      !!icd10 := toupper(!!icd10),
      !!cancer := classify_icd10(!!icd10, cancer_type = cancer_type),
      !!agegrp := cutage(
        age,
        method = "interval",
        breaks = age_breaks,
        label_tail = label_tail
      )
    ) |>
    select(
      !!regi_num,
      !!year,
      !!sex,
      !!cancer,
      !!agegrp,
      !!icd10,
      all_of(intersect(".canreg_index", names(x)))
    )

  class(res) <- c("SWcases", class(res))
  return(res)
}


#' @rdname cr_clean
#' @method cr_clean POP
#' @export
#' @examples
#' pop <- purrr::pluck(canregs[[1]], "POP")
#' pop <- cr_clean(pop)
#'
cr_clean.POP <- function(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
) {
  age_breaks <- validate_age_breaks(age_breaks)
  if (age_breaks[1] != 0 || tail(age_breaks, 1) > 85) {
    stop("Population target breaks must start at 0 and cannot split 85+.")
  }
  sum_vars <- intersect(c("rks", "death"), names(x))
  if (!all(c("year", "sex", "agegrp", "rks") %in% names(x))) {
    stop("Population requires year, sex, agegrp and rks columns.")
  }
  res <- as_tibble(x) |>
    mutate(year = as.character(.data$year), sex = tidy_sex(.data$sex))
  if (nrow(res) == 0L) {
    res <- res[, c("year", "sex", "agegrp", sum_vars)]
    res$agegrp <- cutage(
      numeric(),
      method = "interval",
      breaks = age_breaks,
      label_tail = label_tail
    )
  } else {
    res <- res |>
      group_by(.data$year, .data$sex) |>
      dplyr::group_modify(function(.x, .y) {
        source <- suppressWarnings(as.numeric(as.character(.x$agegrp)))
        standard <- list(seq(0, 85, 5), c(0, 1, seq(5, 85, 5)))
        if (
          anyNA(source) ||
            anyDuplicated(source) ||
            !any(vapply(
              standard,
              function(b) identical(sort(source), b),
              logical(1)
            ))
        ) {
          stop(
            "Population agegrp must contain a complete, unique standard 18- or 19-group schedule (numeric lower bounds)."
          )
        }
        .x <- .x[order(source), ]
        source <- sort(source)
        aligned <- all(age_breaks %in% source)
        ages <- if (aligned) source else 0:85
        result <- tibble(
          agegrp = cutage(
            ages,
            method = "interval",
            breaks = age_breaks,
            label_tail = label_tail
          )
        )
        for (variable in sum_vars) {
          raw_values <- .x[[variable]]
          values <- if (is.numeric(raw_values)) {
            raw_values
          } else {
            suppressWarnings(as.numeric(as.character(raw_values)))
          }
          raw_missing <- is.na(raw_values) |
            trimws(as.character(raw_values)) == ""
          invalid <- (!raw_missing & !is.finite(values)) |
            is.nan(values) |
            (!is.na(values) & values < 0) |
            (variable == "rks" & raw_missing)
          if (any(invalid)) {
            invalid_age <- paste(source[invalid], collapse = ", ")
            invalid_value <- paste(
              as.character(raw_values[invalid]),
              collapse = ", "
            )
            stop(
              "Invalid `",
              variable,
              "` population count(s) for year ",
              .y$year,
              ", sex ",
              .y$sex,
              ", agegrp ",
              invalid_age,
              ": ",
              invalid_value,
              ". `rks` must be complete; counts must be finite, ",
              "non-negative numbers. Missing optional `death` values are allowed."
            )
          }
          result[[variable]] <- if (aligned) {
            values
          } else {
            missing_source <- which(is.na(values))
            expanded <- expand_age_pop(
              replace(values, missing_source, 0),
              source_breaks = source,
              method = "natural"
            )
            if (length(missing_source) > 0L) {
              expanded$y[expanded$source_group %in% missing_source] <- NA_real_
            }
            expanded$y
          }
        }
        result |>
          group_by(.data$agegrp) |>
          summarise(across(all_of(sum_vars), sum), .groups = "drop")
      }) |>
      ungroup()
  }
  class(res) <- unique(c("POP", class(res)))
  return(res)
}
