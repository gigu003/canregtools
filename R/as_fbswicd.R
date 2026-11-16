#' Construct `fbswicd` objects from summary data frames
#'
#' `as_fbswicd()` constructs the summary-level object used by
#' [create_asr()], [create_quality()], and related functions. Supply one
#' summarized incidence table and its matching population table.
#' Optional site/morphology frequencies and excluded records can also be
#' included.
#'
#' @param fbsw A data frame with the key columns `year`, `sex`, `cancer`, and
#'   `agegrp`, plus the required incidence count `fbs`. Optional count columns
#'   `sws`, `mv`, `dco`, `mvs`, `ub`, `sub`, `ids`, and `mult` are retained
#'   when supplied.
#' @param pop A population data frame with `year`, `sex`, `agegrp`, and `rks`.
#'   An optional `death` column is retained.
#' @param sitemorp Optional data frame with `year`, `sex`, `cancer`, `icd10`,
#'   and `morp`. The last two columns must be list-columns of named numeric
#'   frequency vectors.
#' @param exclude Optional data frame containing records excluded during case
#'   cleaning. Its columns are retained without imposing a case schema.
#' @param regicode One non-missing registry identifier. It must be `NULL` when
#'   `regicode_var` is supplied.
#' @param regicode_var Optional name of a registry-code column. When supplied,
#'   `fbsw` and `pop` must contain exactly the same codes. Optional components
#'   may contain a subset of those codes. The grouping column is removed from
#'   constructed components.
#'
#' @return One object of class `fbswicd`, or a named `fbswicds` object when
#'   `regicode_var` contains multiple registry codes. A single code still
#'   returns one `fbswicd` object.
#'
#' @details Count columns may be numeric or numeric text. They are converted
#'   to numeric values and must be finite and non-negative. Population
#'   denominators cannot be missing; missing optional `death` counts are
#'   preserved. Omitted optional count columns are not filled with zeros.
#'   When `sitemorp` or `exclude` is `NULL`, that component is omitted. An
#'   explicitly supplied zero-row data frame is retained, distinguishing an
#'   available empty component from one that was not supplied.
#'   Rate and quality functions interpret absent age rows within an observed
#'   summary year/sex/cancer stratum as zero counts, using matching population
#'   ages. Entirely absent strata and missing population rows are not inferred.
#'   Registry order follows first appearance in `fbsw`. Functions calculating
#'   mortality or quality indicators still require their corresponding count
#'   columns.
#'
#' @examples
#' data("canregs")
#' counted <- count_canreg(canregs[[1]])
#' rebuilt <- as_fbswicd(
#'   fbsw = counted$fbswicd,
#'   pop = counted$pop,
#'   sitemorp = counted$sitemorp,
#'   exclude = counted$exclude,
#'   regicode = counted$regicode
#' )
#' @export
as_fbswicd <- function(
  fbsw,
  pop,
  sitemorp = NULL,
  exclude = NULL,
  regicode = NULL,
  regicode_var = NULL
) {
  if (is.null(regicode_var)) {
    return(as_fbswicd_single(
      fbsw = fbsw,
      pop = pop,
      sitemorp = sitemorp,
      exclude = exclude,
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
  validate_regicode_group_data(fbsw, "fbsw", regicode_var)
  validate_regicode_group_data(pop, "pop", regicode_var)
  codes <- regicode_codes(fbsw, regicode_var)
  check_regicode_code_sets(
    list(fbsw = fbsw, pop = pop),
    regicode_var,
    codes
  )

  optional <- list(sitemorp = sitemorp, exclude = exclude)
  for (component in names(optional)) {
    data <- optional[[component]]
    if (is.null(data)) {
      next
    }
    validate_regicode_group_data(data, component, regicode_var)
    unknown <- setdiff(regicode_codes(data, regicode_var), codes)
    if (length(unknown) > 0L) {
      stop(
        "`",
        component,
        "` contains registry code(s) not found in `fbsw`: ",
        paste(unknown, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  fbsw_groups <- split_regicode_data(fbsw, regicode_var, codes)
  pop_groups <- split_regicode_data(pop, regicode_var, codes)
  optional_groups <- lapply(optional, function(data) {
    if (is.null(data)) {
      return(rep(list(NULL), length(codes)))
    }
    split_regicode_data(data, regicode_var, codes)
  })
  result <- Map(
    function(
      code,
      registry_fbsw,
      registry_pop,
      registry_sitemorp,
      registry_exclude
    ) {
      as_fbswicd_single(
        fbsw = registry_fbsw,
        pop = registry_pop,
        sitemorp = registry_sitemorp,
        exclude = registry_exclude,
        regicode = code
      )
    },
    codes,
    fbsw_groups,
    pop_groups,
    optional_groups$sitemorp,
    optional_groups$exclude
  )
  names(result) <- codes
  if (length(result) == 1L) {
    return(result[[1L]])
  }
  structure(result, class = c("fbswicds", "list"))
}

as_fbswicd_single <- function(fbsw, pop, sitemorp, exclude, regicode) {
  regicode <- as.character(regicode)
  if (length(regicode) != 1L || is.na(regicode) || !nzchar(trimws(regicode))) {
    stop(
      "`regicode` must be one non-missing registry identifier.",
      call. = FALSE
    )
  }
  fbsw <- as_fbswicd_component(
    fbsw,
    "fbsw",
    c("year", "sex", "cancer", "agegrp", "fbs")
  )
  fbsw <- as_fbswicd_counts(
    fbsw,
    intersect(
      c("fbs", "sws", "mv", "dco", "mvs", "ub", "sub", "ids", "mult"),
      names(fbsw)
    ),
    "fbsw"
  )
  pop <- as_fbswicd_component(pop, "pop", c("year", "sex", "agegrp", "rks"))
  pop <- as_fbswicd_counts(
    pop,
    intersect(c("rks", "death"), names(pop)),
    "pop",
    allow_missing = "death"
  )
  class(pop) <- unique(c("POP", class(pop)))

  if (!is.null(sitemorp)) {
    sitemorp <- as_fbswicd_component(
      sitemorp,
      "sitemorp",
      c("year", "sex", "cancer", "icd10", "morp")
    )
    for (variable in c("icd10", "morp")) {
      valid <- vapply(
        sitemorp[[variable]],
        function(value) {
          is.numeric(value) &&
            !is.null(names(value)) &&
            all(is.finite(value)) &&
            all(value >= 0)
        },
        logical(1)
      )
      if (!all(valid)) {
        stop(
          "`sitemorp$",
          variable,
          "` must be a list-column of named, finite, non-negative numeric vectors.",
          call. = FALSE
        )
      }
    }
  }

  if (!is.null(exclude)) {
    exclude <- as_fbswicd_component(exclude, "exclude", character())
  }

  result <- list(regicode = regicode, fbswicd = fbsw)
  if (!is.null(sitemorp)) {
    result$sitemorp <- sitemorp
  }
  result$pop <- pop
  if (!is.null(exclude)) {
    result$exclude <- exclude
  }
  structure(result, class = c("fbswicd", "list"))
}

as_fbswicd_component <- function(data, name, required) {
  if (!is.data.frame(data)) {
    stop("`", name, "` must be a data frame.", call. = FALSE)
  }
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      "Missing required variables in `",
      name,
      "`: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  standardize_year_sex(data)
}

as_fbswicd_counts <- function(
  data,
  variables,
  component,
  allow_missing = character()
) {
  for (variable in variables) {
    raw <- data[[variable]]
    value <- if (is.numeric(raw)) {
      raw
    } else {
      suppressWarnings(as.numeric(as.character(raw)))
    }
    missing <- is.na(raw) | trimws(as.character(raw)) == ""
    invalid <- (!missing & !is.finite(value)) |
      is.nan(value) |
      (!is.na(value) & value < 0) |
      (missing & !variable %in% allow_missing)
    if (any(invalid)) {
      stop(
        "Invalid `",
        component,
        "$",
        variable,
        "` value(s) at row(s) ",
        paste(which(invalid), collapse = ", "),
        ": ",
        paste(as.character(raw[invalid]), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    data[[variable]] <- value
  }
  data
}
