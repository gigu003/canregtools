#' mp decimal
#'
#' @param mp A constant to multiply rates by (e.g. mp=1000 for rates per 1000).
#' @param decimal This parameter specifies the number of decimal places to
#'        round the results. The default is 2, which means rates will
#'        be rounded to two decimal places.
#' @name mp_decimal
#' @keywords internal
NULL

#' cancer_type
#'
#' @param cancer_type A character string specifying the classification method 
#'        used to categorize ICD-10 codes. This determines how ICD-10 codes are 
#'        classified. Options include `"big"` (classify ICD-10 codes into 26
#'        cancer categories), `"small"` (classify ICD-10 codes into 59 cancer
#'        categories, more specific categories), `"system"` (classify ICD-10
#'        codes into organ system), and `"gco"` (classify ICD-10 code into
#'        cancer categories same as classification published by the Global
#'        Cancer Observatory). This parameter is only available when the
#'        input data is a vector of ICD-10 codes, or object with class of
#'        `'canreg'` or `'canregs'`.
#' @name cancer_type
#' @keywords internal
NULL

#' strat_vars
#'
#' @param ... One or more variables used for stratification. For example, you
#'        can stratify by `sex`, `year`, `cancer`, or just by `year`. If sex
#'        is not passed as a parameter, the output will be the result for the
#'        combined gender.
#' @name strat_vars
#' @keywords internal
NULL

#' data
#'
#' @param x The input data, object with class of `'fbswicd'`, `'fbswicds'`,
#'        `'canreg'`, or `'canregs'`.
#' @name data
#' @keywords internal
NULL

#' event
#'
#' @param event A variable used to specify the type of calculation, options are
#'        fbs or sws, fbs for cancer incidence, and sws for cancer mortality.
#' @name event
#' @keywords internal
NULL

#' label_type
#'
#' @param label_type Type of the label used ("full" or "abbr").
#' @name label_type
#' @keywords internal
NULL

#' lang
#'
#' @param lang Character, specify the output language, options are 'cn',
#'        or 'en', default is 'cn'.
#' @name lang
#' @keywords internal
NULL


#' as_factor
#'
#' @param as_factor Logical, indicate whether output value as factor.
#' @name as_factor
#' @keywords internal
NULL
