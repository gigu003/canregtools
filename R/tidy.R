#' Tidy gender variable
#'
#' @param x Vector contains gender information.
#' @param lang Character, specify the output language, options are 'cn',
#'        or 'en', default is 'cn'.
#'
#' @return A factor vector contains gender information.
#' @export
#'
#' @examples
#' gender <- c("male", "men", "women", "female", "women", "man", "1", "2")
#' tidy_sex(gender)
tidy_sex <- function(x, lang = "cn") {
  x <- tolower(x)
  mks <- c("\u7537", "male", "man", "men")
  fks <- c("\u5973", "female", "women", "woman")
  x[grepl(paste(fks, collapse = "|"), x)] <- 2
  x[grepl(paste(mks, collapse = "|"), x)] <- 1
  x[!(x %in% c(1, 2))] <- NA
  if (tolower(lang) %in% c("cn", "zh-cn")) {
    labels <- c("\u7537\u6027", "\u5973\u6027")
  } else if (tolower(lang) %in% c("en", "eng", "english")) {
    labels <- c("Male", "Female")
  } else {
    warning("Unsupported language. Using default labels.")
    labels <- c("Male", "Female")
  }
  factor(as.integer(x), levels = c(1, 2), labels = labels)
}
