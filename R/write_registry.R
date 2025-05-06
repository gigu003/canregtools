#' Write registry attributes to the user-defined dictionary.
#'
#' This function stores the mapping between administrative division codes 
#' (`areacode`) and their corresponding attributes (`values`) into a specific 
#' dictionary file, such as `registry_dict` or `area_type_dict`.
#'
#' @rdname write_registry
#' @param x A named list where names are six-digit administrative
#'        division codes, and values are corresponding attributes
#'        (e.g., registry names or area types such as 'Urban').
#' @param dict_type A string indicating the type of dictionary ('registry' or
#'        area_type').
#'
#' @return A tibble containing the updated dictionary.
#' @export
#'
write_registry <- function(x = NULL,
                           dict_type = "registry") {
  UseMethod("write_registry", x)
}

#' @rdname write_registry
#' @method write_registry data.frame
#' @export
#' 
write_registry.data.frame <- function(x = NULL,
                                      dict_type = "registry") {
  
  vars_exist <- all(c("areacode", dict_type) %in% colnames(x))
  if (vars_exist){
    res <- as.character(x[[dict_type]])
    names(res) <- as.character(x[["areacode"]])
  }
  write_registry.character(res, dict_type = dict_type)
}

#' @rdname write_registry
#' @method write_registry list
#' @export
#' 
write_registry.list <- function(x = NULL,
                                dict_type = "registry") {
  new_x <- as.character(x)
  names(new_x) <- names(x)
  write_registry.character(x, dict_type)
}

#' @rdname write_registry
#' @method write_registry NULL
#' @export
#' 
write_registry.NULL <- function(x = NULL,
                                dict_type = "registry") {
  dict_name <- paste0("areacode_",dict_type)
  default_dict <- dict_maps[[dict_name]]
  write_registry.character(x = default_dict, dict_type = dict_type)
}

#' @rdname write_registry
#' @method write_registry character
#' @export
#' 
write_registry.character <- function(x = NULL,
                                     dict_type = "registry") {
  # Ensure dict_type is valid
  valid_types <- c("registry", "area_type")
  if ((!dict_type %in% valid_types) & (!is.null(x)) ) {
    stop("Invalid 'dict_type'. Must be one of: ",
         paste(valid_types, collapse = ", "))
  }
  
  # Check if key_value_pairs is a named list
  if (!is.character(x) || is.null(names(x))) {
    stop("'x' must be a named character vector with areacode or area_type as names.")
  }
  
  # Define the cache directory and dictionary file path
  cache_dir <- tools::R_user_dir("canregtools", "config")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  dict_file <- file.path(cache_dir, paste0(dict_type, "_dict.rds"))
  dict_name <- paste0("areacode_",dict_type)
  # Load existing dictionary or create a new one
  dict <- if (file.exists(dict_file)) {
    tryCatch(
      readRDS(dict_file),
      error = function(e) {
        warning("Failed to read the existing dictionary. Starting fresh.")
        list() })
    } else {
      dict_maps[[dict_name]]
    }
  
  # Update the dictionary with new key-value pairs
  if (!is.null(x)){ dict[names(x)] <- x }
  # Save the updated dictionary
  saveRDS(dict, file = dict_file)
  # Return the updated dictionary as a tibble
  tibble::tibble(
    areacode = names(dict),
    value = unlist(dict, use.names = FALSE)
  )
}

#' Write Six-digit administrative division code to the user defined dict.
#'
#' @param x A data frame contains `areacode`, `cname`, `ename`, `abbr_cn`
#'        `abbr_en`
#'
#' @return Null
#' @export
#'
write_areacode <- function(x = NULL) {
  cache_dir <- tools::R_user_dir("canregtools", "config")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  dict_file <- file.path(cache_dir, "label_areacode.rds")
  
  if (file.exists(dict_file)) {
    label_areacode <- readRDS(dict_file)
  } else {
    label_areacode <- tidy_var_maps[["areacode"]]
  }
  if (!missing(x) && !is.null(x)){
    areacode <- rlang::sym("areacode")
    label_areacode <- label_areacode |>
      filter(!!areacode %nin% pluck(x, "areacode"))
    new_areacode <- x |>
      filter(!!areacode %nin% paste0(c(91:92, 72:79), "0000"))
    label_areacode <- rbind(label_areacode, new_areacode)
  }
  saveRDS(label_areacode, dict_file)
}

