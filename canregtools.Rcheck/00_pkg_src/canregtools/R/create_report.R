#' Render standardized cancer registry reports from built-in templates
#'
#' The `create_report()` function generates a report from objects with class of 
#' `canreg` or `canregs` using pre-defined rmarkdown templates.
#' 
#' @rdname create_report
#' @param data An object of class `canreg` or `canregs`.
#' @param template Character string specifying the report template to use.
#'   Options include `"annual"`, `"quality"`, or `"CI5"`. Default is `"annual"`.
#' @param title Character. Title of the generated report. Default is 
#'   `"Cancer Registry Report"`.
#' @param output_format Character. Format of the rendered report. 
#'   Options are `"html_document"`, `"word_document"`, or `"pdf_document"`. 
#'   Default is `"html_document"`.
#' @param output_dir  Character. Directory where the report will be saved.
#' @param ... Additional arguments passed to `rmarkdown::render()`.
#'
#' @return No return value; generates a report as a side effect.
#' @export
#' @examples
#' \dontrun{
#' data("canregs")
#' create_report(canregs, template = "quality", title = "QC Report")
#' }
#' 
create_report <- function(data,
                          template = "annual",
                          title = "Cancer Registry Report",
                          output_format = "html_document",
                          output_dir = NULL, ...) {
  UseMethod("create_report", data)
}

#' @rdname create_report
#' @method create_report canregs
#' @export
#' @examples
#' \dontrun{
#' create_report(canregs, template = "annual", title = "Annual Report")
#' }
#' 
create_report.canregs <- function(data,
                                  template = "annual",
                                  title = "Cancer Registry Report",
                                  output_format = "html_document",
                                  output_dir = NULL, ...) {
  if (template == "quality-list-city") {
    template <- paste0(template, ".Rmd")
    tempath <- system.file("rmarkdown", template, package = "canregtools")
    if (is.null(tempath) || !file.exists(tempath)) {
      stop("Template not found: ", template)
    }
    rmarkdown::render(
      input = tempath,
      output_format = output_format,
      output_dir = output_dir,
      output_file = "index",
      params = list(
        report_data = data,
        report_title = title
      ), ...
    )
  } else {
    purrr::map(data,
      create_report.canreg,
      template = template,
      title = title,
      output_format = output_format,
      output_dir = output_dir
    )
  }
}

#' @rdname create_report
#' @method create_report canreg
#' @export
#' @examples
#' \dontrun{
#' data <- canregs[[1]]
#' create_report(data, template = "annual", title = "Annual Report")
#' }
#' 
create_report.canreg <- function(data,
                                 template = "annual",
                                 title = "Cancer Registry Report",
                                 output_format = "html_document",
                                 output_dir = NULL, ...) {
  # Query the template path
  template <- paste0(template, ".Rmd")
  tempath <- system.file("rmarkdown", template, package = "canregtools")
  if (is.null(tempath) || !file.exists(tempath)) {
    stop("Template not found: ", template)
  }
  areacode <- purrr::pluck(data, "areacode")
  output_file <- paste0(areacode)
  if (is.null(output_dir)) {
    output_dir <- getwd()
  }
  author <- tidy_var(areacode, var_name = "areacode", lang = "cn")
  # render report using rmarkdown
  rmarkdown::render(
    input = tempath,
    output_format = output_format,
    output_dir = output_dir,
    output_file = output_file,
    params = list(
      report_data = data,
      report_title = title,
      report_author = author
    ), ...
  )

  cat("Report generated at:", file.path(output_dir, output_file), "\n")
}
