#' Count 
#'
#' @param x Fbswicd
#' @param ... Strata
#' @param wrap_subsite Logical 
#' @param class_morp Logical
#' @param drop_nos  Logical
#'
#' @returns Data frame
#' @export
#'
create_site <- function(x,
                        ...,
                        wrap_subsite = FALSE,
                        class_morp = TRUE,
                        drop_nos = TRUE
                        ) {
  sitemorp <- purrr::pluck(x, "sitemorp")
  strat_vars <- get_expr_vars(...)
  stats_vars <- rlang::syms(c("icd10", "morp"))
  sitemorp |>
    group_by(!!!rlang::syms(strat_vars)) |>
    reframe(
      across(all_of(c("icd10", "morp")), function(x){
        if (cur_column() == "morp") {
          res <- combine_tp(x)
          res <- tibble(!!sym("morp") := names(res),
                        count = unname(res)
                        )
          if (class_morp) {
            res <- dplyr::mutate(res,
                                 !!sym("morp") := classify_morp(!!sym("morp"))) |> 
              group_by(!!sym("morp")) |> 
              summarise(count = sum(count), .groups = "drop")
          }
          list(res)
        } else {
          res <- combine_tp(x)
          res <- tibble(
            !!sym("icd10") := names(res),
            count = unname(res)
            )
          if (wrap_subsite) {
            res <- dplyr::mutate(res,
                                 !!sym("icd10") := ifelse(grepl("\\.9", !!sym("icd10")), "CXX.9", "CXX.0")
                                 ) |> 
              group_by(!!sym("icd10")) |>
              summarise(count = sum(count), .groups = "drop")
          }
          list(res)
        }
      })
    )
}
