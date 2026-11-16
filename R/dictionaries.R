# All consumers resolve defaults and overrides here. Reads never migrate files.
registry_dict_kind <- function(dict) {
  validate_registry_dict_name(dict)
  if (tolower(dict) == "regicode") "labels" else "mapping"
}

registry_dict_defaults <- function(dict) {
  if (registry_dict_kind(dict) == "labels") {
    result <- tidy_var_maps[["regicode"]]
    names(result)[names(result) == "code"] <- "regicode"
    return(result)
  }
  dict_maps[[paste0("china_regicode_", dict)]] %||% character()
}

empty_registry_overrides <- function(dict) {
  defaults <- registry_dict_defaults(dict)
  if (is.data.frame(defaults)) defaults[FALSE, ] else character()
}

validate_registry_labels <- function(x) {
  fields <- c("regicode", "cname", "ename", "abbr_cn", "abbr_en")
  if (
    !is.data.frame(x) ||
      !all(fields %in% names(x)) ||
      anyDuplicated(names(x)) ||
      !all(vapply(x[fields], is.character, logical(1)))
  ) {
    stop(
      "Registry labels must be a data frame with character columns: ",
      paste(fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  validate_registry_mapping(stats::setNames(rep("", nrow(x)), x$regicode))
  invisible(x)
}

validate_registry_overrides <- function(x, dict) {
  if (registry_dict_kind(dict) == "labels") {
    validate_registry_labels(x)
  } else {
    validate_registry_mapping(x)
  }
  invisible(x)
}

read_registry_state <- function(dict) {
  path <- dict_addr(dict)
  if (!file.exists(path)) {
    return(list(overrides = empty_registry_overrides(dict), legacy = FALSE))
  }
  tryCatch(
    {
      stored <- readRDS(path)
      modern <- is.list(stored) &&
        !is.data.frame(stored) &&
        identical(stored$format, "canregtools-dictionary")
      if (modern) {
        if (
          !identical(stored$version, 1L) ||
            !identical(stored$dict, dict) ||
            !identical(stored$kind, registry_dict_kind(dict))
        ) {
          stop("Unsupported dictionary version or mismatched dictionary type.")
        }
        overrides <- stored$overrides
      } else {
        overrides <- stored
      }
      validate_registry_overrides(overrides, dict)
      list(overrides = overrides, legacy = !modern)
    },
    error = function(e) {
      stop(
        "Cannot read dictionary '",
        dict,
        "': ",
        conditionMessage(e),
        " Original file retained at ",
        path,
        call. = FALSE
      )
    }
  )
}

merge_registry_overrides <- function(base, overrides, dict) {
  if (registry_dict_kind(dict) == "labels") {
    return(dplyr::bind_rows(
      base[!base$regicode %in% overrides$regicode, ],
      overrides
    ))
  }
  if (length(overrides)) {
    base[names(overrides)] <- unname(overrides)
  }
  base
}

read_registry_dictionary <- function(dict) {
  merge_registry_overrides(
    registry_dict_defaults(dict),
    read_registry_state(dict)$overrides,
    dict
  )
}

registry_dict_table <- function(data, dict) {
  if (registry_dict_kind(dict) == "labels") {
    return(dplyr::as_tibble(data))
  }
  dplyr::tibble(regicode = names(data) %||% character(), value = unname(data))
}

save_registry_dictionary <- function(value, path) {
  temporary <- tempfile(".dictionary-", tmpdir = dirname(path))
  on.exit(unlink(temporary, expand = FALSE), add = TRUE)
  saveRDS(value, temporary)
  if (!identical(readRDS(temporary), value)) {
    stop(
      "Dictionary verification failed; original file retained.",
      call. = FALSE
    )
  }
  if (!file.rename(temporary, path)) {
    stop("Cannot replace dictionary; original file retained.", call. = FALSE)
  }
}

lock_registry_dictionary <- function(path) {
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(dirname(path))) {
      stop("Cannot create dictionary directory.", call. = FALSE)
    }
  }
  lock <- paste0(path, ".lock")
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop(
      "Dictionary is locked by another writer: ",
      lock,
      ". Retry after that writer finishes; remove a stale lock only after verifying no writer is active.",
      call. = FALSE
    )
  }
  lock
}

update_registry_dictionary <- function(
  dict,
  x,
  refresh = FALSE,
  quiet = TRUE,
  .locked = FALSE
) {
  validate_registry_flag(refresh, "cache_refresh")
  validate_registry_flag(quiet, "quiet")
  validate_registry_overrides(x, dict)
  path <- dict_addr(dict)
  if (!.locked) {
    lock <- lock_registry_dictionary(path)
    on.exit(unlink(lock, recursive = TRUE, expand = FALSE), add = TRUE)
  }
  state <- read_registry_state(dict)
  overrides <- if (refresh) empty_registry_overrides(dict) else state$overrides
  overrides <- merge_registry_overrides(overrides, x, dict)
  effective <- merge_registry_overrides(
    registry_dict_defaults(dict),
    overrides,
    dict
  )
  if (file.exists(path) && (state$legacy || refresh)) {
    backup <- tempfile(
      paste0(basename(path), ".backup-"),
      tmpdir = dirname(path)
    )
    if (!file.copy(path, backup)) {
      stop("Cannot back up dictionary; original file retained.", call. = FALSE)
    }
    if (!quiet) message("Dictionary backed up to ", backup)
  }
  save_registry_dictionary(
    list(
      format = "canregtools-dictionary",
      version = 1L,
      dict = dict,
      kind = registry_dict_kind(dict),
      overrides = overrides
    ),
    path
  )
  if (!quiet) {
    message("Updated dictionary '", dict, "'.")
  }
  registry_dict_table(effective, dict)
}

#' Reset user dictionary overrides
#'
#' Clears user overrides for one dictionary, restoring current built-in defaults.
#' Existing files are backed up in the same directory before replacement.
#' Corrupt dictionaries are never overwritten; inspect or move them first.
#'
#' @param dict Dictionary name; use `"regicode"` for registry labels.
#' @param quiet Suppress update and backup messages.
#' @return The effective dictionary, invisibly.
#' @export
#' @examples
#' withr::with_options(
#'   list(canregtools.dict_dir = tempfile("canregtools-example-")),
#'   reset_dict("registry")
#' )
reset_dict <- function(dict = "registry", quiet = TRUE) {
  invisible(update_registry_dictionary(
    dict,
    empty_registry_overrides(dict),
    refresh = TRUE,
    quiet = quiet
  ))
}


update_registry_dictionary_batch <- function(updates, refresh, quiet) {
  validate_registry_flag(refresh, "cache_refresh")
  validate_registry_flag(quiet, "quiet")
  attrs <- names(updates)
  paths <- vapply(attrs, dict_addr, character(1))
  locks <- character()
  rollback <- rep(NA_character_, length(paths))
  attempted <- rep(FALSE, length(paths))
  complete <- FALSE
  on.exit(
    {
      retained <- character()
      if (!complete) {
        for (i in rev(which(attempted))) {
          restored <- if (is.na(rollback[i])) {
            !file.exists(paths[i]) || unlink(paths[i], expand = FALSE) == 0L
          } else {
            file.rename(rollback[i], paths[i])
          }
          if (!restored) {
            retained <- c(retained, rollback[i])
            warning(
              "Could not roll back dictionary '",
              attrs[i],
              "'. Recovery copy retained at ",
              rollback[i],
              call. = FALSE
            )
          }
        }
      }
      unlink(setdiff(rollback[!is.na(rollback)], retained), expand = FALSE)
      unlink(locks, recursive = TRUE, expand = FALSE)
    },
    add = TRUE
  )
  for (i in order(paths)) {
    locks <- c(locks, lock_registry_dictionary(paths[i]))
  }
  # Read every target under its writer lock before changing any target.
  for (i in seq_along(attrs)) {
    validate_registry_mapping(updates[[i]])
    read_registry_state(attrs[i])
    if (file.exists(paths[i])) {
      rollback[i] <- tempfile(
        ".dictionary-rollback-",
        tmpdir = dirname(paths[i])
      )
      if (!file.copy(paths[i], rollback[i])) {
        stop(
          "Cannot prepare dictionary rollback copies; no mappings changed.",
          call. = FALSE
        )
      }
    }
  }
  result <- stats::setNames(vector("list", length(attrs)), attrs)
  for (i in seq_along(attrs)) {
    attempted[i] <- TRUE
    result[[i]] <- update_registry_dictionary(
      attrs[i],
      updates[[i]],
      refresh,
      quiet = TRUE,
      .locked = TRUE
    )
  }
  complete <- TRUE
  if (!quiet) {
    message("Updated dictionaries: ", paste(attrs, collapse = ", "), ".")
  }
  result
}
