if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("site"))
}
utils::globalVariables(c("."))
utils::globalVariables(c("age", "birthda", "inciden", "deathda","value"))

'%nin%' <- function(chrVElements, chrVSet){
  !(chrVElements %in% chrVSet)
}

replace_na <- function(x) {
  ifelse(is.na(x), 0, x)
}
