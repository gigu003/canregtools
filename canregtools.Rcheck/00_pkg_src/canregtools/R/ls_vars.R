#' List variable names and their descriptions by category
#'
#' @description
#' This function returns a `tibble` containing variable names and their
#' descriptions based on the specified category.
#'
#' @param type A character string specifying the type of variables to list.
#'   Options are `"std"` for standard population variables,
#'   `"summary"` for summary variables, and `"reframe"` for variables used in
#'   `cr_reframe()`.
#'
#' @return A tibble with five columns:
#'   - `code`: The name of the variable.
#'   - `cname`: A detailed description of the variable in Chinese.
#'   - `ename`: A detailed description of the variable in English.
#'   - `abbr_cn`: An abbreviated description of the code label in Chinese.
#'   - `abbr_en`: An abbreviated description of the code label in English.
#' @export
#' 
#' @seealso [cr_reframe()], [summary()], [create_asr()]
#'
#' @examples
#' ls_vars("std")
#' ls_vars("summary")
#' ls_vars("reframe")
ls_vars <- function(type = "std") {
  valid_types <- names(tidy_var_maps)
  type <- match.arg(type, choices = valid_types)
  tidy_var_maps[[type]] |>
      select(!!rlang::sym("code"),
             !!rlang::sym("cname"),
             !!rlang::sym("ename"),
             !!rlang::sym("abbr_cn"),
             !!rlang::sym("abbr_en")
             )
}

#' list dictionary used in package
#'
#' @param dict Character, name of dictionary.
#'
#' @return A tibble of dictionary.
#' @export
#'
#' @examples
#' ls_dict("registry")
#' ls_dict("area_type")
#'
ls_dict <- function(dict = "registry") {
  file_path <- dict_addr(dict)
  if (!file.exists(file_path)) {
    message("Dictionary file not found: ", file_path)
  } else {
    dict <- readRDS(file_path)
    dplyr::tibble(
      areacode = names(dict),
      value = unname(dict)
    )  
  }
}

#' List dictionary files used by canregtools
#'
#' Lists all dictionary `.rds` files stored in the user's config directory
#' for `canregtools`.
#'
#' @param full.names Logical. Whether to return full file paths.
#'    Default is `FALSE`.
#' @param with_info Logical. Whether to return file size and last modified time.
#'    Default is `FALSE`.
#'
#' @return A character vector of file names, or a tibble with file info
#'    if `with_info = TRUE`.
#' @export
#'
#' @examples
#' ls_dict_files()
#' ls_dict_files(full.names = TRUE)
#' ls_dict_files(with_info = TRUE)
ls_dict_files <- function(full.names = FALSE, with_info = FALSE) {
  cache_dir <- tools::R_user_dir("canregtools", "config")
  all_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (!with_info) {
    return(if (full.names) all_files else basename(all_files))
  }
  
  dplyr::tibble(
    file = basename(all_files),
    size_kb = round(file.info(all_files)$size / 1024, 1),
    modified = file.info(all_files)$mtime,
    path = all_files
  )
}

#' list built-in attributes name used in `cr_reframe()`
#'
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' ls_attrs()
#' 
ls_attrs <- function() {
  c("registry", "region", "area_type", "province", "city")
}

#' Delete a dictionary file from canregtools config
#'
#' Removes a specified dictionary file (e.g., "registry", "area_type", "custom")
#' from the user's local configuration directory.
#'
#' @param dict A character string specifying the dictionary name.
#'   Default is "custom".
#'
#' @return Invisibly returns TRUE if deleted, FALSE otherwise.
#' @export
#'
#' @examples
#' del_dict_files("custom")
del_dict_files <- function(dict = "custom") {
  addr <- dict_addr(dict)
  
  if (file.exists(addr)) {
    file.remove(addr)
    message("Dictionary '", dict, "' deleted from cache.")
    return(invisible(TRUE))
  } else {
    message("Dictionary '", dict, "' does not exist.")
    return(invisible(FALSE))
  }
}
