
summary.population <- function(object) {
  cat("Summary of Population Data:\n")
  cat("----------------------------\n")
  cat("Year: ", unique(object$year), "\n")
  cat("Sex: ", unique(object$sex), "\n")
  cat("Age Groups:\n")
  cat(unique(object$agegrp), "\n")
  cat("\nRKS (Registered Key Statistics):\n")
  cat("Total population: ", sum(object$rks))
}

fbsss <- function(x, type = "big", cutage = "18"){
  if (!inherits(x, "canreg")) {
    stop("Input must be a valid canreg object.")
  }
  reg_count <- function(data, varname = "fbs") {
    bind_rows(
      data %>%
        count(year, sex, agegrp, icd_cat, name = varname, .drop = FALSE),
      data %>%
        count(year, sex, agegrp, name = varname, .drop = FALSE) %>%
        mutate(icd_cat = "\u5408\u8ba1", icd_cat = as.factor(icd_cat))
    )
  }
  
  fb <- clean(x$incidence, icd_type = type, cutage = cutage)
  sw <- clean(x$mortality, icd_type = type, cutage = cutage)
  pop <- clean(x$population)
  res <- list(
    reg_count(fb, varname = "fbs"),
    reg_count(sw, varname = "sws"),
    fb %>% filter(basi %in% c(5, 6, 7)) %>% reg_count(varname = "mv"),
    fb %>% filter(basi == 0) %>% reg_count(varname = "dco")) %>%
    purrr::reduce(left_join, by = c("year", "sex", "agegrp", "icd_cat")) %>%
    left_join(pop, by = c("year", "sex", "agegrp")) %>%
    arrange(year, sex, icd_cat, agegrp)
  return(res)
}