#' Load Example Data Set
#'
#' This function loads an example data set from the 'canregtools' package. 
#' The data set is an Excel file included with the package.
#'
#' @return A data frame or tibble containing the example data.
#' @export
#' 
#' @examples
#' data <- load_canreg()
#' 
load_canreg <- function() {
  # Get the path to the example data file in the 'canregtools' package
  file <- system.file("extdata", "411721.xls", package = "canregtools")
  # Read the data from the file using read_canreg
  res <- read_canreg(file)
  # Return the loaded data
  return(res)
}
