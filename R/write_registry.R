#' Write registry attributes to a user-defined dictionary
#'
#' Stores the mapping between six-digit administrative division codes
#' areacode and their corresponding attributes e.g.,
#' registry names or area types into a dictionary file. This supports
#' consistent labeling and downstream processing in cancer registry data.
#'
#' @param x A named list, a named character vector or a data frame that
#'   includes the columns `areacode` and the target attribute
#'   e.g., `registry`, `area_type`.
#' @param dict A character string specifying the type of dictionary to
#'   update. Supported values are "registry" and "area_type".
#' @param cache_refresh If TRUE, refresh the dictionary to default values before
#'    updating.
#' @param quiet If TRUE, message will be supressed.
#' @return A tibble with two columns: `areacode` and the corresponding
#'   attribute values.
#'
#' @details
#' This function allows users to build or update local mapping dictionaries for
#' area-level attributes. It supports multiple input formats and updates
#' internal files saved in the user-specific R cache directory.
#'
#' @export
#' @examples
#' write_registry(list('410302' = '410301'))
#' 
write_registry <- function(
    x = NULL,
    dict = "registry",
    cache_refresh = FALSE,
    quiet = TRUE
    ) {
  UseMethod("write_registry", x)
}

#' @rdname write_registry
#' @method write_registry data.frame
#' @export
#' @examples
#' # Registry attributes stored in data frame
#' registry_dict <- data.frame(
#' areacode = c("410302", "410303", "410304", "410305", "410306", "410307"),
#' registry = c(rep("410301", 5), "410300"),
#' area_type = rep("urban", 6)
#' )
#' write_registry(registry_dict)
#' 
write_registry.data.frame <- function(
    x = NULL,
    dict = "registry",
    cache_refresh = FALSE,
    quiet = TRUE
    ) {
  vars_exist <- all(c("areacode", dict) %in% colnames(x))
  if (vars_exist) {
    res <- as.character(x[[dict]])
    names(res) <- as.character(x[["areacode"]])
  }
  write_registry.character(res,
                           dict = dict,
                           cache_refresh = cache_refresh,
                           quiet = quiet)
}

#' @rdname write_registry
#' @method write_registry list
#' @export
#' @examples
#' # Registry attributes stored in list
#' dict <- list(
#' '410302' = '410301',
#' '410303' = '410301',
#' '410304' = '410301',
#' '410305' = '410301',
#' '410306' = '410301',
#' '410307' = '410301'
#' )
#' write_registry(dict)
#' 
write_registry.list <- function(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
  ) {
  c_x <- as.character(x)
  names(c_x) <- names(x)
  write_registry.character(c_x,
                           dict = dict,
                           cache_refresh = cache_refresh,
                           quiet = quiet)
}

#' @rdname write_registry
#' @method write_registry NULL
#' @export
#' @examples
#' # Registry attributes using built-in information
#' dict <- NULL
#' write_registry(dict)
#' 
write_registry.NULL <- function(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
  ) {
  dict_name <- paste0("areacode_", dict)
  default_dict <- dict_maps[[dict_name]]
  write_registry.character(x = default_dict,
                           dict = dict,
                           cache_refresh = cache_refresh,
                           quiet = quiet)
}

#' @rdname write_registry
#' @method write_registry character
#' @export
#' @examples
#' # Registry attributes stored in named character vector with areacode as
#' # name and attributes as values 
#' dict <- rep("410301", 5)
#' names(dict) <- c("410302", "410303", "410304", "410305", "410306")
#' write_registry(dict)
#' 
write_registry.character <- function(
  x = NULL,
  dict = "registry",
  cache_refresh = FALSE,
  quiet = TRUE
  ) {
  # Validate dictionary type
  valid_types <- c("registry", "area_type", "custom", "yyyy")
  if (!dict %in% valid_types) {
    stop("Invalid 'dict'. Must be one of: ",
         paste(valid_types, collapse = ", "))
  }
  
  # Validate input vector
  if (!is.character(x) || is.null(names(x))) {
    stop("'x' must be a named character vector with areacode or area_type as names.")
  }
  
  # Compose dictionary file path and default map name
  dict_file <- dict_addr(dict = dict)
  dict_name <- paste0("areacode_", dict)
  
  # Load existing dictionary or fallback to predefined map
  dict_data <- if (cache_refresh || !file.exists(dict_file)) {
    dict_maps[[dict_name]]
  } else {
    tryCatch(
      readRDS(dict_file),
      error = function(e) {
        warning("Failed to read existing dictionary. Starting fresh.")
        dict_maps[[dict_name]]
      }
    )
  }
  
  existing_keys <- intersect(names(x), names(dict_data))
  if (length(existing_keys) > 0) {
    if (!quiet) {
      message(length(existing_keys), " entries will be overwritten. Example(s):")
      print(utils::head(dplyr::tibble(
        areacode = existing_keys,
        old = unlist(dict_data[existing_keys], use.names = FALSE),
        new = unlist(x[existing_keys], use.names = FALSE)
      ), 5)) 
    }
  }
  
  # Update the dictionary with new key-value pairs
  dict_data[names(x)] <- x
  # Save the updated dictionary
  saveRDS(dict_data, file = dict_file)
  # Return the updated dictionary as a tibble
  if (!quiet) {
    dplyr::tibble(
      areacode = names(dict_data),
      value = unlist(dict_data, use.names = FALSE)
    )
  }
}

#' Write custom six-digit administrative division codes to dictionary
#'
#' Saves user-defined administrative division codes and their associated labels
#' (in both Chinese and English) to the local dictionary used by `canregtools`.
#'
#' @param x A data frame containing at least the following columns:
#'   `areacode`, `cname`, `ename`, `abbr_cn`, and `abbr_en`.
#' @param cache_refresh Logical. If TRUE, refresh the dictionary to default values
#'    before updating.
#'
#' @return Invisibly returns `NULL`. The function is called for its side effect.
#' @export
#' @examples
#' \dontrun{
#' dict <- data.frame(
#'   areacode = c("410302"),
#'   cname = c("\u8001\u57CE\u533A"),
#'   ename = c("Laocheng District"),
#'   abbr_cn = c("\u8001\u57CE"),
#'   abbr_en = c("Laocheng")
#' )
#' write_areacode(dict)
#' }
write_areacode <- function(x = NULL, cache_refresh = FALSE) {
  dict_file <- dict_addr(dict = "areacode")
  
  # Load or refresh the dictionary
  if (cache_refresh || !file.exists(dict_file)) {
    label_areacode <- tidy_var_maps[["areacode"]]
  } else {
    label_areacode <- readRDS(dict_file)
  }
  
  # If new values provided
  if (!missing(x) && !is.null(x)) {
    stopifnot(all(c("areacode", "cname", "ename", "abbr_cn", "abbr_en") %in% names(x)))
    
    areacode_sym <- rlang::sym("areacode")
    areacode_vec <- purrr::pluck(x, "areacode")
    
    # Overlap check
    overlapping <- dplyr::intersect(label_areacode$areacode, areacode_vec)
    if (length(overlapping) > 0) {
      message(length(overlapping), " entries will be overwritten. Example(s):")
      print(utils::head(
        dplyr::tibble(
          areacode = overlapping,
          old = label_areacode[label_areacode$areacode %in% overlapping, ],
          new = x[x$areacode %in% overlapping, ]
        ),
        5
      ))
    }
    
    # Remove old entries with the same areacode
    label_areacode <- dplyr::filter(label_areacode, !!areacode_sym %nin% areacode_vec)
    
    # Skip certain province/region codes
    skip_codes <- paste0(c(91:92, 72:79), "0000")
    new_areacode <- dplyr::filter(x, !!areacode_sym %nin% skip_codes)
    
    # Combine and save
    label_areacode <- dplyr::bind_rows(label_areacode, new_areacode)
  }
  
  saveRDS(label_areacode, dict_file)
  invisible(NULL)
}
