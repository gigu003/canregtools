clean_canreg <- function(x,
                         length = 5,
                         maxage = 85,
                         sep_zero = T,
                         type = "big",
                         lang = "cn"
                         ){
    dclass <- class(x)
    dclass <- dclass[dclass %in% c("FBcases","SWcases","population")]
    switch(dclass,
           FBcases = {
             print(paste0("Processing data: ", length(x$sex), " FBcases."))
             year <- as.numeric(format(x$inciden, "%Y"))
             sex <- as.integer(x$sex)
             age <- calc_age(x$birthda, x$inciden)
             agegrp <- cutage(age,
                              method = "distance",
                              length = length,
                              maxage = maxage,
                              sep_zero = sep_zero)
             basi <- as.integer(x$basi)
             icd10 <- toupper(x$icd10)
             icd_cat <- classify_icd10(icd10, type = type, lang =lang)
             res <- tibble(year, sex, agegrp, icd_cat, basi)
             return(res)
           },
           SWcases = {
             print(paste0("Processing data: ", length(x$sex), " SWcases."))
             year <- as.numeric(format(x$deathda, "%Y"))
             sex <- as.integer(x$sex)
             age <- calc_age(x$birthda, x$deathda)
             agegrp <- cutage(age,
                              method = "distance",
                              length = length,
                              maxage = maxage,
                              sep_zero = sep_zero)
             icd10 <- toupper(x$icd10)
             icd_cat <- classify_icd10(icd10, type = type, lang = lang)
             res <- tibble(year, sex, agegrp, icd_cat)
             return(res)
           },
           population = {
             print(paste0("Processing POP data of year: ", unique(x$year)))
             year <- as.integer(x$year)
             sex <- as.integer(x$sex)
             agegrp <- factor(x$agegrp, labels = seq(1, 19, 1))
             rks <- as.integer(x$rks)
             if (sep_zero){
               res <- tibble(year, sex, agegrp, rks)
             } else {
               res <- tibble()
             }
             
             
             return(res)
           },
           default = {
             stop(paste("Unsupported data class:", dclass))
           })
}