#' Class of canreg
#'
#' @slot areacode character. 
#' @slot name character. 
#' @slot contains c. 
#'
#' @return Class of canreg .
#' @export
#'
setClass("canreg",
         slots = list(
           areacode = "character",
           name = "character",
           contains = c("FBcases", "SWcases", "population") 
         ))



#' Class of FBcases
#'
#' @slot registr character. 
#' @slot id character. 
#' @slot name character. 
#' @slot sex factor. 
#' @slot age numeric. 
#' @slot birthda Date. 
#' @slot topo character. 
#' @slot morp character. 
#' @slot beha character. 
#' @slot grad character. 
#' @slot basi character. 
#' @slot icd10 character. 
#' @slot inciden Date. 
#' @slot dlc Date. 
#' @slot status integer. 
#' @slot deathda Date. 
#' @slot caus integer. 
#' @slot areacode character. 
#'
#' @return class of FBcases.
#' @export
#'
setClass("FBcases",
         slots = list(
           registr = "character",
           id = "character",
           name = "character",
           sex = "factor",
           age = "numeric",
           birthda = "Date",
           topo = "character",
           morp = "character",
           beha = "character",
           grad = "character",
           basi = "character",
           icd10 = "character",
           inciden = "Date",
           dlc = "Date",
           status = "integer",
           deathda = "Date",
           caus = "integer",
           areacode = "character"
         ))

#' Class of SWcases
#'
#' @slot registr character. 
#' @slot id character. 
#' @slot name character. 
#' @slot sex factor. 
#' @slot age numeric. 
#' @slot agegrp factor. 
#' @slot birthda Date. 
#' @slot topo character. 
#' @slot morp character. 
#' @slot beha character. 
#' @slot icd10 character. 
#' @slot inciden Date. 
#' @slot deathda Date. 
#' @slot areacode character. 
#'
#' @return Class of SWcases
#' @export
#'
#'
setClass("SWcases",
         slots = list(
           registr = "character",
           id = "character",
           name = "character",
           sex = "factor",
           age = "numeric",
           agegrp = "factor",
           birthda = "Date",
           topo = "character",
           morp = "character",
           beha = "character",
           icd10 = "character",
           inciden = "Date",
           deathda = "Date",
           areacode = "character"
         ))

#' Class of population
#'
#' @slot year integer. 
#' @slot sex factor. 
#' @slot agegrp factor. 
#' @slot rks numeric. 
#'
#' @return class of population.
#' @export
#'
setClass("population",
         slots = list(
           year = "integer",
           sex = "factor",
           agegrp = "factor",
           rks = "numeric"
         ))
 