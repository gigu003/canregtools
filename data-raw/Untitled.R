reg_count <- function(data, varname = "fbs") {
  data <- as.data.table(data)
  varname <- as.name(varname)
  gvars1 <- c("year", "sex", "cancer", "agegrp")
  gvars2 <- c("year", "sex", "agegrp")
  mcount1 <- data[, .N, by = gvars1, drop = FALSE]
  mcount2 <- data[, .N, by = gvars2, drop = FALSE]
  set(mcount2, j = "cancer", value = 60)
  mcount3 <- data[!grepl("^C44", icd10), .N, by = gvars2, drop = FALSE]
  set(mcount3, j = "cancer", value = 61)
  result <- rbindlist(list(mcount1, mcount2, mcount3), fill = TRUE)
  setnames(result, "N", as.character(varname))
  return(result)
}