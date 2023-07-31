count_canreg <- function(x,
                  type = "big",
                  length = 5,
                  maxage = 85,
                  sep_zero = T,
                  lang = "cn"){
  if (!inherits(x, "canreg")) {
    stop("Input must be a valid canreg object.")
  }
  reg_count <- function(data, varname = "fbs") {
    bind_rows(
      data %>%
        count(year, sex, agegrp, icd_cat, name = varname, .drop = FALSE),
      data %>%
        count(year, sex, agegrp, name = varname, .drop = FALSE) %>%
        mutate(icd_cat = ifelse(lang == "cn", "\u5408\u8ba1", "Total"),
               icd_cat = as.factor(icd_cat))
    )
  }
  fb <- clean_canreg(x$FBcases,
              type = type,
              length = length,
              maxage = maxage,
              sep_zero = sep_zero,
              lang = lang)
  sw <- clean_canreg(x$SWcases,
              type = type,
              length = length,
              maxage = maxage,
              sep_zero = sep_zero,
              lang = lang)
  levels <- levels(sw$agegrp)
  pop <- clean_canreg(x$POP)
  if (!sep_zero){
    pop <- pop %>% mutate(agegrp = fct_collapse(agegrp, "2" = c("1","2"))) %>% 
      group_by(year, sex, agegrp) %>% 
      summarise(rks = sum(rks), .groups = "drop")
  }
  levels(pop$agegrp) <- levels
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