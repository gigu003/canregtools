#' Count and classify `canreg` data
#'
#' The `count_canreg()` function is a generic method used to summarize
#' population-based cancer registry data. It supports both single (`canreg`)
#' and multiple (`canregs`) registry objects. The function aggregates cancer
#' cases by age group and classifies cancer types using standardized coding
#' systems.
#'
#' @rdname count_canreg
#' @param x Object with class of 'canreg' or 'canregs'.
#' @param age_breaks A numeric vector specifying the breakpoints for age
#'    grouping. Defaults to `c(0, 1, seq(5, 85, 5))`.
#' @param label_tail Optional. A string to append to age group labels
#'    (e.g., `"+"` for open-ended intervals). Defaults to `NULL`.
#' @param strategy Counting strategy for a canregs object. The default,
#'    "batch", cleans and aggregates all registry case tables together.
#'    Use "registry" for the original one-registry-at-a-time path.
#' @template cancer_type
#' @return Object with class of 'fbswicd' or 'fbswicds'.
#' @seealso [cr_clean()], [classify_icd10()], [create_asr()]
#' @export
#'
#' @examples
#' data("canregs")
#' fbsw <- count_canreg(canregs, age_breaks = c(0, 15, 65), cancer_type = "big")
#' fbsw <- count_canreg(canregs, cancer_type = "gco")
#' fbsw <- count_canreg(canregs, cancer_type = "ci5")
#'
count_canreg <- function(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big",
  strategy = c("batch", "registry")
) {
  UseMethod("count_canreg", x)
}

#' @rdname count_canreg
#' @method count_canreg canregs
#' @param ... Additional arguments passed to the method for individual
#'    `canreg` objects.
#' @export
#'
#' @examples
#' # Count object with class of `canregs`
#' fbsw <- count_canreg(canregs, cancer_type = "small")
#'
count_canreg.canregs <- function(
  x,
  ...,
  strategy = c("batch", "registry")
) {
  strategy <- match.arg(strategy)
  if (strategy == "batch") {
    return(count_canreg_batch(x, ...))
  }

  res <- purrr::map(
    x,
    count_canreg,
    ...,
    .progress = "Counting canregs #"
  )
  structure(res, class = c("fbswicds", "list"))
}

count_canreg_batch <- function(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big"
) {
  if (length(x) == 0L) {
    return(structure(
      list(),
      names = names(x),
      class = c("fbswicds", "list")
    ))
  }

  registry_index <- rlang::sym(".canreg_index")
  regi_num <- rlang::sym("regi_num")
  year <- rlang::sym("year")
  cancer <- rlang::sym("cancer")
  agegrp <- rlang::sym("agegrp")
  c44 <- rlang::sym("c44")
  fbs <- rlang::sym("fbs")
  sws <- rlang::sym("sws")
  mv <- rlang::sym("mv")
  ub <- rlang::sym("ub")
  sub <- rlang::sym("sub")
  mvs <- rlang::sym("mvs")
  dco <- rlang::sym("dco")
  mult <- rlang::sym("mult")
  basi <- rlang::sym("basi")
  icd10 <- rlang::sym("icd10")
  morp <- rlang::sym("morp")
  topo <- rlang::sym("topo")

  prepared <- vector("list", length(x))
  for (index in seq_along(x)) {
    registry <- x[[index]]
    fb <- registry$FBcases
    fb[[".canreg_index"]] <- index
    sw <- registry$SWcases
    sw[[".canreg_index"]] <- index
    pop <- cr_clean(
      registry$POP,
      age_breaks = age_breaks,
      label_tail = label_tail,
      cancer_type = cancer_type
    )
    pop[[".canreg_index"]] <- index
    prepared[[index]] <- list(FBcases = fb, SWcases = sw, POP = pop)
  }
  fb_raw <- purrr::map(prepared, "FBcases") |>
    bind_rows()
  sw_raw <- purrr::map(prepared, "SWcases") |>
    bind_rows()

  fb_format <- cr_clean(
    structure(fb_raw, class = c("FBcases", class(fb_raw))),
    age_breaks = age_breaks,
    label_tail = label_tail,
    cancer_type = cancer_type
  ) |>
    group_by(!!registry_index) |>
    mutate(!!regi_num := row_number()) |>
    ungroup()
  sw_format <- cr_clean(
    structure(sw_raw, class = c("SWcases", class(sw_raw))),
    age_breaks = age_breaks,
    label_tail = label_tail,
    cancer_type = cancer_type
  ) |>
    group_by(!!registry_index) |>
    mutate(!!regi_num := row_number()) |>
    ungroup()

  fb <- drop_sex_cancer(
    fb_format,
    drop_na = c("year", "sex", "agegrp", "cancer")
  )
  sw <- drop_sex_cancer(
    sw_format,
    drop_na = c("year", "sex", "agegrp", "cancer")
  )

  valid_fb <- fb |>
    dplyr::distinct(!!registry_index, !!regi_num)
  valid_sw <- sw |>
    dplyr::distinct(!!registry_index, !!regi_num)
  registr <- bind_rows(
    purrr::map2(x, seq_along(x), function(registry, index) {
      tibble(
        .canreg_index = index,
        regi_num = seq_len(nrow(registry$FBcases)),
        registr = registry$FBcases$registr
      )
    }) |>
      bind_rows() |>
      mutate(source = "FBcases"),
    purrr::map2(x, seq_along(x), function(registry, index) {
      tibble(
        .canreg_index = index,
        regi_num = seq_len(nrow(registry$SWcases)),
        registr = registry$SWcases$registr
      )
    }) |>
      bind_rows() |>
      mutate(source = "SWcases")
  )
  exclude <- bind_rows(
    dplyr::anti_join(
      fb_format,
      valid_fb,
      by = c(".canreg_index", "regi_num")
    ) |>
      mutate(source = "FBcases"),
    dplyr::anti_join(
      sw_format,
      valid_sw,
      by = c(".canreg_index", "regi_num")
    ) |>
      mutate(source = "SWcases")
  ) |>
    left_join(
      registr,
      by = c(".canreg_index", "regi_num", "source")
    ) |>
    mutate(!!topo := as.character(!!topo)) |>
    select(source, registr, everything())
  class(exclude) <- c("FBcases", "FBcases", class(exclude))

  sumvars <- c("fbs", "sws", "mv", "dco", "mvs", "ub", "sub")
  ubs <- c("C26", "C39", "C48", "C76", "C77", "C78", "C79", "C80", "C97")
  res <- fb |>
    mutate(
      !!fbs := 1L,
      !!c44 := as.integer(substr(!!icd10, 1, 3) == "C44"),
      !!mv := as.integer(!!basi %in% 5:7),
      !!dco := as.integer(!!basi == 0),
      !!mvs := as.integer(
        !!morp %in% c("8000", "8001", "8010", "8020", "8021") &
          !!basi %in% 5:7
      ),
      !!ub := as.integer(substr(!!icd10, 1, 3) %in% ubs),
      !!sub := as.integer(substr(!!icd10, 5, 5) == "9")
    )
  if ("mult" %in% names(res)) {
    res <- res |>
      group_by(!!registry_index) |>
      mutate(!!mult := as.integer(!!mult) == 1) |>
      ungroup()
    sumvars <- c(sumvars, "mult")
  }
  if ("id" %in% names(res)) {
    res <- res |>
      group_by(!!registry_index) |>
      mutate(
        ids = {
          ids <- .data$id
          as.integer(
            !is.na(ids) & (duplicated(ids) | duplicated(ids, fromLast = TRUE))
          )
        }
      ) |>
      ungroup()
    sumvars <- c(sumvars, "ids")
  }

  byvars <- c(".canreg_index", "year", "sex", "cancer", "agegrp")
  res <- bind_rows(
    res |>
      select(
        all_of(byvars),
        !!c44,
        all_of(dplyr::setdiff(sumvars, "sws"))
      ),
    sw |>
      mutate(
        !!c44 := as.integer(substr(!!icd10, 1, 3) == "C44"),
        !!sws := 1L
      ) |>
      select(
        all_of(byvars),
        !!c44,
        !!sws
      )
  ) |>
    group_by(across(all_of(byvars)), !!c44) |>
    reframe(across(all_of(sumvars), ~ sum(.x, na.rm = TRUE))) |>
    mutate(across(all_of(sumvars), as.integer))

  fbswicd <- bind_rows(
    group_by(res, across(all_of(byvars))) |>
      reframe(across(all_of(sumvars), sum)),
    group_by(res, across(all_of(dplyr::setdiff(byvars, "cancer")))) |>
      reframe(across(all_of(sumvars), ~ sum(.x, na.rm = TRUE))) |>
      mutate(!!cancer := "60"),
    filter(res, !!c44 == 0) |>
      group_by(across(all_of(dplyr::setdiff(byvars, "cancer")))) |>
      reframe(across(all_of(sumvars), ~ sum(.x, na.rm = TRUE))) |>
      mutate(!!cancer := "61")
  )

  registry_years <- bind_rows(
    fb |> select(!!registry_index, year),
    sw |> select(!!registry_index, year)
  ) |>
    dplyr::distinct()
  cancer_codes <- unique(c(get_cancer(cancer_type), fbswicd[["cancer"]]))
  age_levels <- levels(res$agegrp)
  fbswicd <- expand_grid(
    registry_years,
    sex = c("1", "2"),
    cancer = cancer_codes,
    agegrp = factor(age_levels, levels = age_levels)
  ) |>
    left_join(fbswicd, by = byvars) |>
    mutate(across(all_of(sumvars), ~ tidyr::replace_na(.x, 0))) |>
    standardize_year_sex()

  sitemorp <- fb |>
    group_by(
      !!registry_index,
      !!!rlang::syms(c("year", "sex", "cancer"))
    ) |>
    reframe(
      !!icd10 := list(ctp(!!icd10)),
      !!morp := list(ctp(!!morp))
    ) |>
    standardize_year_sex()

  pop <- purrr::map(prepared, "POP") |>
    bind_rows() |>
    standardize_year_sex()

  split_result <- function(data) {
    purrr::map(seq_along(x), function(index) {
      data[data[[".canreg_index"]] == index, , drop = FALSE] |>
        select(-any_of(".canreg_index"))
    })
  }
  fbswicd_split <- split_result(fbswicd)
  sitemorp_split <- split_result(sitemorp)
  pop_split <- split_result(pop)
  exclude_split <- purrr::map(
    split_result(exclude),
    structure,
    class = class(exclude)
  )
  result <- purrr::map(seq_along(x), function(index) {
    structure(
      list(
        regicode = as.character(x[[index]]$regicode),
        fbswicd = fbswicd_split[[index]],
        sitemorp = sitemorp_split[[index]],
        pop = pop_split[[index]],
        exclude = exclude_split[[index]]
      ),
      class = c("fbswicd", "list")
    )
  })
  structure(result, names = names(x), class = c("fbswicds", "list"))
}

#' @rdname count_canreg
#' @method count_canreg canreg
#' @export
#'
#' @examples
#' # Count object with class of `canreg`
#' fbsw <- count_canreg(canregs[[1]], cancer_type = "big")
#'
count_canreg.canreg <- function(
  x,
  age_breaks = c(0, 1, seq(5, 85, 5)),
  label_tail = NULL,
  cancer_type = "big",
  strategy = c("batch", "registry")
) {
  regi_num <- rlang::sym("regi_num")
  format <- cr_clean(
    x,
    age_breaks = age_breaks,
    label_tail = label_tail,
    cancer_type = cancer_type
  )
  fb <- drop_sex_cancer(
    format$FBcases,
    drop_na = c("year", "sex", "agegrp", "cancer")
  )
  sw <- drop_sex_cancer(
    format$SWcases,
    drop_na = c("year", "sex", "agegrp", "cancer")
  )
  exclude <- bind_rows(
    format$FBcases |>
      filter(!!regi_num %nin% fb[["regi_num"]]) |>
      mutate(
        source = "FBcases",
        .before = everything(),
        registr = x$FBcases$registr[!!regi_num]
      ),
    format$SWcases |>
      filter(!!regi_num %nin% sw[["regi_num"]]) |>
      mutate(
        source = "SWcases",
        .before = everything(),
        registr = x$SWcases$registr[!!regi_num]
      )
  ) |>
    mutate(topo = as.character(topo))

  year <- rlang::sym("year")
  sex <- rlang::sym("sex")
  cancer <- rlang::sym("cancer")
  sumvars <- c("fbs", "sws", "mv", "dco", "mvs", "ub", "sub")

  ubs <- c("C26", "C39", "C48", "C76", "C77", "C78", "C79", "C80", "C97")
  basi <- rlang::sym("basi")
  icd10 <- rlang::sym("icd10")
  topo <- rlang::sym("topo")
  morp <- rlang::sym("morp")
  byvars <- c("year", "sex", "cancer", "agegrp")

  c44 <- rlang::sym("c44")
  fbs <- rlang::sym("fbs")
  sws <- rlang::sym("sws")
  mv <- rlang::sym("mv")
  ub <- rlang::sym("ub")
  sub <- rlang::sym("sub")
  mvs <- rlang::sym("mvs")
  dco <- rlang::sym("dco")
  mult <- rlang::sym("mult")

  res <- fb |>
    mutate(
      !!fbs := 1L,
      !!c44 := as.integer(substr(!!icd10, 1, 3) == "C44"),
      !!mv := as.integer(!!basi %in% 5:7),
      !!dco := as.integer(!!basi == 0),
      !!mvs := as.integer(
        !!morp %in% c("8000", "8001", "8010", "8020", "8021") & !!basi %in% 5:7
      ),
      !!ub := as.integer(substr(!!icd10, 1, 3) %in% ubs),
      !!sub := as.integer(substr(!!icd10, 5, 5) == "9")
    )
  if ("mult" %in% names(res)) {
    res <- dplyr::mutate(res, !!mult := as.integer(!!mult) == 1)
    sumvars <- c(sumvars, "mult")
  }
  if ("id" %in% names(res)) {
    ids <- res[["id"]]
    res$ids <- as.integer(
      !is.na(ids) & (duplicated(ids) | duplicated(ids, fromLast = TRUE))
    )
    sumvars <- c(sumvars, "ids")
  }
  res <- bind_rows(
    res |>
      select(
        !!!rlang::syms(byvars),
        !!c44,
        all_of(dplyr::setdiff(sumvars, "sws"))
      ),
    sw |>
      mutate(!!c44 := as.integer(substr(!!icd10, 1, 3) == "C44")) |>
      dplyr::transmute(
        !!!rlang::syms(byvars),
        !!c44,
        !!sws := 1L
      )
  ) |>
    group_by(!!!rlang::syms(byvars), !!c44) |>
    reframe(across(all_of(sumvars), ~ sum(.x, na.rm = TRUE))) |>
    mutate(across(all_of(sumvars), as.integer))

  # Construct the `fbswicd`
  fbswicd <- bind_rows(
    group_by(res, !!!rlang::syms(byvars)) |>
      reframe(across(all_of(sumvars), sum)),
    group_by(res, !!!rlang::syms(dplyr::setdiff(byvars, "cancer"))) |>
      reframe(across(all_of(sumvars), ~ sum(.x, na.rm = TRUE))) |>
      mutate(!!cancer := "60"),
    filter(res, !!c44 == 0) |>
      group_by(!!!rlang::syms(dplyr::setdiff(byvars, "cancer"))) |>
      reframe(across(all_of(sumvars), ~ sum(.x, na.rm = TRUE))) |>
      mutate(!!cancer := "61")
  )

  years <- unique(c(pluck(fb, "year"), pluck(sw, "year")))
  # expand full combinations
  fbswicd <- expand_grid(
    year = years,
    sex = c("1", "2"),
    cancer = unique(c(get_cancer(cancer_type), fbswicd[["cancer"]])),
    agegrp = factor(levels(res$agegrp), levels = levels(res$agegrp))
  ) |>
    left_join(fbswicd, by = byvars, ) |>
    mutate(across(all_of(sumvars), ~ tidyr::replace_na(.x, 0))) |>
    standardize_year_sex()

  # Count the sub-sites and morphology codes
  sitemorp <- fb |>
    group_by(!!!rlang::syms(c("year", "sex", "cancer"))) |>
    reframe(
      !!icd10 := list(ctp(!!icd10)),
      !!morp := list(ctp(!!morp))
    ) |>
    standardize_year_sex()

  # construct the result list
  res <- list(
    regicode = purrr::pluck(format, "regicode"),
    fbswicd = fbswicd,
    sitemorp = sitemorp,
    pop = purrr::pluck(format, "POP") |> standardize_year_sex(),
    exclude = exclude |> standardize_year_sex()
  )

  attr(res, "class") <- c("fbswicd", class(res))

  return(res)
}
