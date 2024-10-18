if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("year", "sex", "agegrp", "agegrp2",
                           "basi", "icd10", "morp",
                           "cr", "spop", "cases", "rate", 
                           "adcode", "name", "fbs", "sws", "rks", "cancer",
                           "cn2000", "wld85","f0","p0","sws","site"))
}
utils::globalVariables(c("."))
utils::globalVariables(c("inciden", "birthda", "age", "deathda"))

'%nin%' <- function(chrVElements, chrVSet){
  !(chrVElements %in% chrVSet)
}

replace_na <- function(x) {
  ifelse(is.na(x), 0, x)
}

post_vars <- function(data){
  all_vars <- colnames(data)
  if ("year" %nin% all_vars){
    data <- data |> 
      mutate(year = 9000)
  }
  if ("sex" %nin% all_vars){
    data <- data |> 
      mutate(sex = 0)
  }
  if ("cancer" %nin% all_vars){
    data <- data |> 
      mutate(cancer = 60)
  }
  
  data <- data |> 
    select(starts_with(c("year", "sex", "cancer")), everything())
  return(data)
}