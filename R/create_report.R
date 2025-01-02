#' Create report from report templates
#'
#' @rdname create_report
#' @param data Object with class of 'canreg' or 'fbswicd'.
#' @param template Name of the template used to render report, options are
#'        "annual", "quality", or "CI5", default is "annual".
#' @param title Title of the report to be rendered, default is "Cancer Registry
#'        Report".
#' @param output_format File type of the render result, options are
#'        "html_document", "word_document" or "pdf_document", default is
#'        "html_document".
#' @param output_dir Directory of the rendered report.
#'
#' @return NULL
#' @export
#'
create_report <- function(data,
                          template = "annual",
                          title = "Cancer Registry Report",
                          output_format = "html_document",
                          output_dir = NULL) {
  UseMethod("create_report", data)
}

#' @rdname create_report
#' @method create_report canregs
#' @export
#'
create_report.canregs <- function(data,
                                  template = "annual",
                                  title = "Cancer Registry Report",
                                  output_format = "html_document",
                                  output_dir = NULL){
  purrr::map(data,
             create_report.canreg,
             template = template,
             title = title,
             output_format = output_format,
             output_dir = output_dir)
}

#' @rdname create_report
#' @method create_report canreg
#' @export
#'
create_report.canreg <- function(data,
                                 template = "annual",
                                 title = "Cancer Registry Report",
                                 output_format = "html_document",
                                 output_dir = NULL){
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
  # render report using rmarkdown
  rmarkdown::render(input = tempath, 
                    output_format = output_format,
                    output_dir = output_dir,
                    output_file = output_file,
                    params = list(
                      report_data = data,
                      report_title = title
                    ))
  
  cat("Report generated at:", file.path(output_dir, output_file), "\n")
}
