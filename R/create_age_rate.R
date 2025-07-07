#' Calculate age specific rate
#'
#' `create_age_rate()` computes age-specific rates from object with class of
#' `canreg`,`fbswicd`, or `canregs`, `fbswicds`. It calculates the rates for
#' specified events (e.g., `fbs`) across age groups, stratified by variables
#' such as year, sex, or cancer type.
#'
#' @template data
#' @template strat_vars
#' @template event
#' @template cancer_type
#' @param format Format of the output data frame, either "long" or "wide".
#' @template mp_decimal
#' @param show_pop Logical value whether output population or not.
#' @param collapse Logical value whether output result as age_rate or age_rates.
#' @return A data frame of age-specific rates.
#' @export
#' @examples
#' data("canregs")
#' agerate <- create_age_rate(canregs, year, sex, cancer)
#'
create_age_rate <- function(x,
                            ...,
                            event = "fbs",
                            cancer_type = "big",
                            format = "long",
                            mp = 100000,
                            decimal = 6,
                            show_pop = FALSE,
                            collapse = TRUE) {
  UseMethod("create_age_rate", x)
}

#' @rdname create_age_rate
#' @method create_age_rate canreg
#' @export
#' @examples
#' data <- canregs[[1]]
#' agerate <- create_age_rate(data, year, sex)
#'
create_age_rate.canreg <- function(
    x,
    ...,
    cancer_type = "big"
    ) {
  structure(
    count_canreg(x, cancer_type = cancer_type) |>
      create_age_rate(...),
    class = c("age_rate", "tbl_df", "tbl", "data.frame")
  )
}

#' @rdname create_age_rate
#' @method create_age_rate canregs
#' @export
#' @examples
#' agerate <- create_age_rate(canregs, year, cancer_type = "system")
#'
create_age_rate.canregs <- function(
    x,
    ...,
    cancer_type = "big",
    collapse = TRUE
    ) {
  count_canreg(x, cancer_type = cancer_type) |>
    create_age_rate(..., collapse = collapse)
}

#' @method create_age_rate fbswicds
#' @rdname create_age_rate
#' @export
#' @examples
#' fbsws <- count_canreg(canregs)
#' agerate <- create_age_rate(fbsws, year, sex)
#'
create_age_rate.fbswicds <- function(
    x,
    ...,
    event = "fbs",
    format = "long",
    mp = 100000,
    decimal = 6,
    show_pop = FALSE,
    collapse = TRUE
    ) {
  res <- purrr::map(x,
    create_age_rate,
    ...,
    event = {{ event }},
    format = format,
    mp = mp,
    decimal = decimal,
    show_pop = show_pop,
    .progress = "Calculating age rate #"
  )
  class(res) <- c("age_rates", "list")
  if (collapse) {
    return(cr_merge(res))
  }
  
  res
}

#' @method create_age_rate fbswicd
#' @rdname create_age_rate
#' @export
#' @examples
#' data <- canregs[[2]]
#' fbsw <- count_canreg(data, cancer_type = "small")
#' agerate <- create_age_rate(fbsw, year, sex, cancer)
create_age_rate.fbswicd <- function(
    x,
    ...,
    event = "fbs",
    format = "long",
    mp = 100000,
    decimal = 6,
    show_pop = FALSE
    ) {
  event <- rlang::sym(event)
  rks <- rlang::sym("rks")
  agegrp <- rlang::sym("agegrp")
  agegrp2 <- rlang::sym("agegrp2")
  sex <- rlang::sym("sex")
  cases <- rlang::sym("cases")
  rate <- rlang::sym("rate")

  # Deal with population data
  gvars <- rlang::enquos(...) |>
    purrr::map_chr(rlang::as_name)
  sex_not_pass <- ifelse("sex" %nin% gvars, TRUE, FALSE)
  if ("sex" %nin% gvars) {
    gvars <- c(gvars, "sex")
  }
  gvars2 <- setdiff(gvars, c("cancer", "agegrp"))

  fbsw <- pre_deal_fbswicd(x, ...)
  pop_modi <- pre_deal_pop(x, ...)

  output <- fbsw |>
    group_by(!!!rlang::syms(gvars), !!agegrp) |>
    reframe(across(!!event, sum)) |>
    left_join(pop_modi, by = c(gvars2, "agegrp")) |>
    mutate(
      !!cases := !!event,
      !!rate := round(!!event / !!rks * mp, decimal)
    ) |>
    select(-!!event)

  if (format == "long") {
    if (!show_pop) {
      output <- output |>
        select(-starts_with("rks"))
    }
  } else if (format == "wide") {
    output <- output %>%
      mutate(!!agegrp2 := as.numeric(!!agegrp)) %>%
      select(-!!agegrp) %>%
      rename(f = cases, r = rate, p = rks) %>%
      pivot_wider(
        names_from = !!agegrp2,
        names_sep = "",
        values_from = c("f", "p", "r"),
        values_fill = 0
      )

    output <- output |>
      rowwise() |>
      mutate(
        !!rlang::sym("f0") := sum(c_across(starts_with("f"))),
        !!rlang::sym("p0") := sum(c_across(starts_with("p"))),
        !!rlang::sym("r0") := round(
          !!rlang::sym("f0") / !!rlang::sym("p0") * mp,
          decimal
        )
      ) |>
      ungroup() |>
      select(
        starts_with(c("year", "sex", "cancer")),
        starts_with("f0"), starts_with("f"),
        starts_with("p0"), starts_with("p"),
        starts_with("r0"), starts_with("r")
      )

    if (!show_pop) {
      output <- select(output, -starts_with("p"))
    }
  }

  # Deal with group vars
  output <- output |>
    post_vars() |>
    post_sex_specific_cancer()
  if (sex_not_pass) {
    output <- filter(output, !!sex == 0L)
  }
  attr(output, "class") <- c("age_rate", "tbl_df", "tbl", "data.frame")

  return(output)
}
