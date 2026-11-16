#' List variable names and their descriptions by category
#'
#' @description
#' This function returns a `tibble` containing variable names and their
#' descriptions based on the specified category. For `type = "reframe"`,
#' valid custom grouping dictionaries from the active dictionary directory are
#' appended to the built-in attributes, using their names as default labels.
#' Registry label files and backup files are excluded; invalid grouping files
#' are skipped with a warning.
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
  var_map <- if (type == "regicode") {
    labels <- read_registry_dictionary("regicode")
    names(labels)[names(labels) == "regicode"] <- "code"
    labels
  } else {
    tidy_var_maps[[type]]
  }
  if (type == "reframe") {
    var_map <- append_custom_reframe_vars(var_map)
  }
  var_map |>
    select(
      !!rlang::sym("code"),
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
#' @return The effective dictionary including defaults and user overrides.
#'   Mapping dictionaries have `regicode` and `value` columns; `regicode`
#'   labels have `regicode`, `cname`, `ename`, `abbr_cn`, and `abbr_en`.
#' @export
#'
#' @examples
#' ls_dict("registry")
#' ls_dict("area_type")
#'
ls_dict <- function(dict = "registry") {
  registry_dict_table(read_registry_dictionary(dict), dict)
}

#' List dictionary files used by canregtools
#'
#' Lists all dictionary `.rds` files stored in the user's config directory
#' for `canregtools`, or the project directory set by `canregtools.dict_dir`.
#' Migration and reset backup files are excluded.
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
  cache_dir <- dirname(dict_addr("registry"))
  all_files <- list.files(
    cache_dir,
    pattern = "_dict\\.rds$",
    full.names = TRUE
  )

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
    lock <- paste0(addr, ".lock")
    if (!dir.create(lock, showWarnings = FALSE)) {
      stop("Dictionary is locked by another writer: ", lock, call. = FALSE)
    }
    on.exit(unlink(lock, recursive = TRUE, expand = FALSE), add = TRUE)
    if (!file.remove(addr)) {
      return(invisible(FALSE))
    }
    message("Dictionary '", dict, "' deleted from cache.")
    return(invisible(TRUE))
  } else {
    message("Dictionary '", dict, "' does not exist.")
    return(invisible(FALSE))
  }
}


append_custom_reframe_vars <- function(built_in) {
  files <- ls_dict_files(full.names = TRUE)
  candidates <- sub("_dict\\.rds$", "", basename(files))
  candidates <- unique(candidates[
    !tolower(candidates) %in% tolower(built_in$code)
  ])
  candidates <- candidates[tolower(candidates) != "regicode"]
  valid <- vapply(
    candidates,
    function(dict) {
      tryCatch(
        {
          state <- read_registry_state(dict)
          validate_registry_mapping(state$overrides)
          TRUE
        },
        error = function(e) {
          warning(
            "Ignoring invalid grouping dictionary '",
            dict,
            "'.",
            call. = FALSE
          )
          FALSE
        }
      )
    },
    logical(1)
  )
  custom <- candidates[valid]
  dplyr::bind_rows(
    built_in,
    dplyr::tibble(
      code = custom,
      cname = custom,
      ename = custom,
      abbr_cn = custom,
      abbr_en = custom
    )
  )
}
