#' Render reports from built-in quarto templates
#'
#' Render standardized cancer registry reports using built-in Quarto templates
#' provided by the package.
#'
#' @rdname cr_render
#'
#' @param data An object of class `canreg` or `canregs`.
#' @param template Character. Report template to use.
#' @param title Character. Title of the report. Default is `"Cancer Registry Report"`.
#' @param author Character. Author name to display in the report.
#' @param registry Character. Name of the cancer registry (e.g., city or region).
#' @param output_format Character. Output format for rendering.
#'   Supported formats include: `"html"`, `"docx"`, and `"typst"`
#' @param ref_doc Character. Path to a Word reference document (DOCX) for
#'    customizing Word output styles.
#' @param output_dir Character. Directory where the rendered report will
#'    be saved. Default is `"outputs"`.
#' @param subdir Optional. Name of a sub-directory within `output_dir`.
#'    If `NULL`, output is saved directly in `output_dir`.
#' @param execute_dir The working directory in which to execute embedded code chunks.
#' @param timestamp Character. Format string for the timestamp in the output
#'    directory name. Default is `"%Y-%m-%d"`.
#' @param quiet Logical. If `TRUE`, suppresses messages during rendering.
#' @param ... Additional arguments passed to `quarto::quarto_render()`.
#'
#' @return Invisibly returns the path to the rendered report file.
#'
#' @export
#' 
#' @importFrom fs file_copy path dir_exists dir_create path_ext_remove
#' @importFrom here here
#' @importFrom quarto quarto_render
#'
cr_render <- function(data,
                      template = "report",
                      title = "Cancer Registry Report",
                      author = "Henan Cancer Center",
                      registry = NULL,
                      output_format = "qcreport-typst",
                      ref_doc = "year-book.docx",
                      output_dir = "outputs",
                      subdir = NULL,
                      execute_dir = NULL,
                      timestamp = "%Y-%m-%d",
                      quiet = TRUE,
                      ...
                      ) {
  UseMethod("cr_render", data)
}

#' @rdname cr_render
#' @method cr_render canregs
#' @export
cr_render.canregs <- function(
    data,
    template = "report",
    title = "Cancer Registry Report",
    author = "Henan Cancer Center",
    registry = NULL,
    output_format = "qcreport-typst",
    ref_doc = "year-book.docx",
    output_dir = "outputs",
    subdir = NULL,
    execute_dir = NULL,
    timestamp = "%Y-%m-%d",
    quiet = TRUE,
    ...) {
  if (grepl("list", template)) {
    crender(data = data,
            template = template,
            title = title,
            author = author,
            registry = registry,
            output_format = output_format,
            ref_doc = ref_doc,
            output_dir = output_dir,
            subdir = subdir,
            execute_dir = execute_dir,
            timestamp = timestamp,
            quiet = quiet,
            ...)
  } else {
    res <- purrr::map(data,
                      crender,
                      template = template,
                      title = title,
                      author = author,
                      registry = registry,
                      output_format = output_format,
                      ref_doc = ref_doc,
                      output_dir = output_dir,
                      subdir = subdir,
                      execute_dir = execute_dir,
                      timestamp = timestamp,
                      quiet = quiet,
                      ...)
    invisible(res)
  }
}

#' @rdname cr_render
#' @method cr_render canreg
#' @export
cr_render.canreg <- function(
    data,
    template = "report",
    title = "Cancer Registry Report",
    author = "Henan Cancer Center",
    registry = NULL,
    output_format = "qcreport-typst",
    ref_doc = "year-book.docx",
    output_dir = "outputs",
    subdir = NULL,
    execute_dir = NULL,
    timestamp = "%Y-%m-%d",
    quiet = TRUE,
    ...
    ) {
  crender(data = data,
          template = template,
          title = title,
          author = author,
          registry = registry,
          output_format = output_format,
          ref_doc = ref_doc,
          output_dir = output_dir,
          subdir = subdir,
          execute_dir = execute_dir,
          timestamp = timestamp,
          quiet = quiet,
          ...
          )
}


#' @noRd
#' @keywords internal
crender <- function(
    data,
    template = "report",
    title = "Cancer Registry Report",
    author = "Henan Cancer Center",
    registry = NULL,
    output_format = "qcreport-typst",
    ref_doc = "year-book.docx",
    output_dir = "outputs",
    subdir = NULL,
    execute_dir = NULL,
    timestamp = "%Y-%m-%d",
    quiet = TRUE,
    ...
    ) {
  template <- paste0(template, ".qmd")
  tempfile <- system.file("quarto", template, package = "canregtools")
  if (tempfile == "" || !fs::file_exists(tempfile)) {
    stop("Template not found: ", template)
  }
  
  # Create cache dir for template file
  cache_dir <- tools::R_user_dir("canregtools", "config")
  if (!dir_exists(cache_dir)) {
    fs::dir_create(cache_dir, recursive = TRUE)
  }
  
  if (fs::file_exists(fs::path(cache_dir, template))) {
    fs::file_delete(fs::path(cache_dir, template))
  }
  
  if (is.null(execute_dir)) {
    execute_dir <- cache_dir
  } else if (!fs::dir_exists(execute_dir)){
    execute_dir <- cache_dir
  } else {
    execute_dir <- here::here(execute_dir)
  }
  
  fs::file_copy(
    path = tempfile,
    new_path = fs::path(execute_dir, template),
    overwrite = TRUE
  )
  
  data_file <- fs::path(cache_dir, "rdata.rds")
  # Generate a time-stamp
  timestamp <- format(Sys.time(), timestamp)
  output_dir <- here::here(output_dir)
  # Add subdir to the output directory.
  if (!is.null(subdir)) {
    output_dir <- fs::path(output_dir, subdir)
  }
  # Add time-stamp to the output directory.
  output_dir <- fs::path(output_dir, timestamp)
  if (!fs::dir_exists(output_dir)) {
    fs::dir_create(output_dir, recurse = TRUE)
  }
  
  if (is.null(registry)) {
    registry <- tidy_var(purrr::pluck(data, "areacode"),
                         var_name = "areacode",
                         lang = "cn")
  }
  
  saveRDS(data, data_file)
  report <- quarto::quarto_render(
    input = fs::path(execute_dir, template),
    output_format = output_format,
    quiet = quiet,
    execute_params = list(
      datafile = data_file,
      title = title,
      author = author,
      registry = registry
    ),
    execute_dir = execute_dir,
    quarto_args = c("--output-dir", output_dir,
                    "--reference-doc", ref_doc),
    ...
  )
  
  base <- fs::path_ext_remove(template)
  surfix <- ifelse(grepl("typst", output_format), ".pdf", paste0(".", output_format))
  file.rename(
    here::here(output_dir, paste0(base, surfix)),
    here::here(output_dir, paste0(base, "-", registry, surfix))
  )
  message(paste0("Report for ", registry, " has been rendered."))
  invisible(report)
}
