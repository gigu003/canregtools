#' Combine cancer registry collections
#'
#' `cr_combine()` combines multiple `canregs` or multiple `fbswicds` objects by
#' registry name. Collections cannot be mixed in one call. For each registry,
#' corresponding data-frame components are appended by row. Optional components
#' available in only some inputs are appended from those inputs. Registries that
#' occur in only some inputs are retained.
#'
#' @param object A non-empty list containing only `canregs` objects or only
#'   `fbswicds` objects. Every collection must be named, and each name must
#'   match the `regicode` stored in its corresponding element.
#'
#' @return A `canregs` or `fbswicds` object, matching the input collection
#'   class, with one combined element per unique registry name.
#' @export
#'
#' @examples
#' data("canregs")
#' combined <- cr_combine(list(canregs, canregs))
#' names(combined)
#' fbsws <- count_canreg(canregs)
#' combined_fbsw <- cr_combine(list(fbsws, fbsws))
cr_combine <- function(object) {
  if (!is.list(object) || length(object) == 0) {
    stop(
      "`object` must be a non-empty list of canregs or fbswicds objects.",
      call. = FALSE
    )
  }

  collection_type <- vapply(
    object,
    function(x) {
      if (inherits(x, "canregs")) {
        "canregs"
      } else if (inherits(x, "fbswicds")) {
        "fbswicds"
      } else {
        NA_character_
      }
    },
    character(1)
  )
  if (anyNA(collection_type) || length(unique(collection_type)) != 1L) {
    stop(
      "Every element of `object` must have the same collection class: ",
      "all canregs or all fbswicds.",
      call. = FALSE
    )
  }

  type <- collection_type[[1]]
  if (type == "canregs") {
    lapply(object, check_combine_canregs)
    combine_element <- combine_canreg_parts
  } else {
    lapply(object, check_combine_fbswicds)
    combine_element <- combine_fbswicd_parts
  }
  registry_names <- sort(unique(unlist(
    lapply(object, names),
    use.names = FALSE
  )))
  result <- lapply(registry_names, function(registry) {
    inputs <- lapply(object, function(x) x[[registry]])
    inputs <- Filter(Negate(is.null), inputs)
    combine_element(inputs, registry)
  })
  names(result) <- registry_names

  structure(result, class = c(type, "list"))
}

check_combine_canregs <- function(x) {
  if (length(x) == 0) {
    stop(
      "Each canregs object must contain at least one registry.",
      call. = FALSE
    )
  }

  registry_names <- names(x)
  if (
    is.null(registry_names) ||
      any(is.na(registry_names) | !nzchar(registry_names)) ||
      anyDuplicated(registry_names)
  ) {
    stop(
      "Every canregs object must have unique, non-missing registry names.",
      call. = FALSE
    )
  }

  for (registry in registry_names) {
    item <- x[[registry]]
    if (!inherits(item, "canreg") || !is.list(item)) {
      stop("Every registry must be a canreg object.", call. = FALSE)
    }

    required <- c("regicode", "FBcases", "SWcases", "POP")
    if (!all(required %in% names(item))) {
      stop(
        "Every canreg object must contain all required components.",
        call. = FALSE
      )
    }

    if (
      length(item$regicode) != 1 ||
        is.na(item$regicode) ||
        !identical(as.character(item$regicode), registry)
    ) {
      stop(
        "Registry names must match the `regicode` in each canreg object.",
        call. = FALSE
      )
    }
  }

  invisible(x)
}

combine_canreg_parts <- function(object, registry) {
  component_classes <- c(
    FBcases = "FBcases",
    SWcases = "SWcases",
    POP = "POP"
  )
  components <- lapply(names(component_classes), function(component) {
    combine_canreg_component(object, component, component_classes[[component]])
  })
  names(components) <- names(component_classes)

  sf_inputs <- Filter(
    Negate(is.null),
    lapply(object, function(x) x[["SFcases"]])
  )
  if (length(sf_inputs) > 0) {
    components <- append(
      components,
      list(
        SFcases = combine_canreg_component(
          object,
          "SFcases",
          "SFcases",
          optional = TRUE
        )
      ),
      after = 2
    )
  }

  structure(
    c(list(regicode = registry), components),
    class = c("canreg", "list")
  )
}

check_combine_fbswicds <- function(x) {
  if (length(x) == 0L) {
    stop(
      "Each fbswicds object must contain at least one registry.",
      call. = FALSE
    )
  }
  registry_names <- names(x)
  if (
    is.null(registry_names) ||
      any(is.na(registry_names) | !nzchar(registry_names)) ||
      anyDuplicated(registry_names)
  ) {
    stop(
      "Every fbswicds object must have unique, non-missing registry names.",
      call. = FALSE
    )
  }

  for (registry in registry_names) {
    item <- x[[registry]]
    if (!inherits(item, "fbswicd") || !is.list(item)) {
      stop("Every registry must be an fbswicd object.", call. = FALSE)
    }
    required <- c("regicode", "fbswicd", "pop")
    if (!all(required %in% names(item))) {
      stop(
        "Every fbswicd object must contain regicode, fbswicd, and pop.",
        call. = FALSE
      )
    }
    if (
      length(item$regicode) != 1L ||
        is.na(item$regicode) ||
        !identical(as.character(item$regicode), registry)
    ) {
      stop(
        "Registry names must match the `regicode` in each fbswicd object.",
        call. = FALSE
      )
    }
    required_columns <- list(
      fbswicd = c("year", "sex", "cancer", "agegrp", "fbs"),
      pop = c("year", "sex", "agegrp", "rks")
    )
    for (component in names(required_columns)) {
      data <- item[[component]]
      if (!is.data.frame(data)) {
        stop("Component `", component, "` must be a data frame.", call. = FALSE)
      }
      missing <- setdiff(required_columns[[component]], names(data))
      if (length(missing) > 0L) {
        stop(
          "Component `",
          component,
          "` is missing required column(s): ",
          paste(missing, collapse = ", "),
          ".",
          call. = FALSE
        )
      }
    }
    for (component in intersect(c("sitemorp", "exclude"), names(item))) {
      if (!is.data.frame(item[[component]])) {
        stop("Component `", component, "` must be a data frame.", call. = FALSE)
      }
    }
  }
  invisible(x)
}

combine_fbswicd_parts <- function(object, registry) {
  fbswicd <- combine_fbswicd_component(object, "fbswicd")
  pop <- combine_fbswicd_component(object, "pop", component_class = "POP")
  result <- list(regicode = registry, fbswicd = fbswicd)

  if (any(vapply(object, function(x) "sitemorp" %in% names(x), logical(1)))) {
    result$sitemorp <- combine_fbswicd_component(
      object,
      "sitemorp",
      optional = TRUE
    )
  }
  result$pop <- pop
  if (any(vapply(object, function(x) "exclude" %in% names(x), logical(1)))) {
    result$exclude <- combine_fbswicd_component(
      object,
      "exclude",
      optional = TRUE
    )
  }
  structure(result, class = c("fbswicd", "list"))
}

combine_fbswicd_component <- function(
  object,
  component,
  component_class = NULL,
  optional = FALSE
) {
  inputs <- lapply(object, function(x) x[[component]])
  if (optional) {
    inputs <- Filter(Negate(is.null), inputs)
  }
  if (!all(vapply(inputs, is.data.frame, logical(1)))) {
    stop("Component `", component, "` must contain data frames.", call. = FALSE)
  }
  if (!optional && length(inputs) > 1L) {
    schemas <- lapply(inputs, names)
    compatible <- vapply(schemas, setequal, logical(1), y = schemas[[1]])
    if (!all(compatible)) {
      stop(
        "Component `",
        component,
        "` must have the same columns for matching registries.",
        call. = FALSE
      )
    }
  }
  if (length(inputs) == 1L) {
    return(inputs[[1]])
  }
  result <- dplyr::bind_rows(inputs)
  if (!is.null(component_class)) {
    class(result) <- unique(c(component_class, class(result)))
  }
  result
}

combine_canreg_component <- function(
  object,
  component,
  component_class,
  optional = FALSE
) {
  inputs <- lapply(object, function(x) x[[component]])
  if (optional) {
    inputs <- Filter(Negate(is.null), inputs)
  }
  if (!all(vapply(inputs, is.data.frame, logical(1)))) {
    stop(
      "Component `",
      component,
      "` must contain data frames.",
      call. = FALSE
    )
  }

  result <- dplyr::bind_rows(inputs)
  structure(result, class = unique(c(component_class, class(result))))
}
