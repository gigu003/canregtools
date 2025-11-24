#' Get standardized population data
#'
#' Retrieves standardized population data for a specified standard from dict_maps.
#'
#' @param std Character string specifying the population standard. 
#'   Supported values are "cn64", "cn82", "cn2000", "wld85", "wld2000".
#'   Defaults to "wld85".
#' @param sep_zero Logical value indicating whether age 0 should be treated
#'      as a separate group.
#' @return A vector or data structure containing the standardized population data 
#'   for the specified standard, or NULL if the standard is not supported.
#' @examples
#' \dontrun{
#'   get_std("cn64")
#'   get_std("wld2000")
#' }
#' @importFrom purrr pluck
#' @export
get_stdpop <- function(std = "wld85", sep_zero = TRUE) {
  if (!std %in% c("cn64", "cn82", "cn2000", "wld85", "wld2000")) {
    cat(paste(std, "was not supported."))
  } else {
    rks <- purrr::pluck(dict_maps, "std_pop") |>
      purrr::pluck(std)
    if (!sep_zero) {
      c(sum(rks[1:2]), rks[3:19])
    } else {
      rks
    }
  }
}
