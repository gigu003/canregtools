#' @param cancer_type A character string specifying the classification method
#'   used to categorize ICD-10 codes. This determines how ICD-10 codes are
#'   classified. Options include `"big"` (classify ICD-10 codes into 26
#'   cancer categories), `"small"` (classify ICD-10 codes into 59 cancer
#'   categories, more specific categories), `"system"` (classify ICD-10
#'   codes into organ system), and `"gco"` (classify ICD-10 code into
#'   cancer categories same as classification published by the Global
#'   Cancer Observatory). This parameter is only available when the
#'   input data is a vector of ICD-10 codes, or object with class of
#'   `'canreg'` or `'canregs'`.
