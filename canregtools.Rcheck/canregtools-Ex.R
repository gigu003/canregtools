pkgname <- "canregtools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "canregtools-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('canregtools')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_labels")
### * add_labels

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_labels
### Title: Add variable labels for data set.
### Aliases: add_labels

### ** Examples

data("canregs")
asr <- create_asr(canregs[[1]], year, sex, cancer)
asr <- add_labels(asr, label_type = "full", lang = "zh")
asr <- add_labels(asr, label_type = "full", lang = "en")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_labels", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("add_var_labels")
### * add_var_labels

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_var_labels
### Title: Add Variable Labels with Units
### Aliases: add_var_labels

### ** Examples

add_var_labels(c("cr", "asr"))
add_var_labels(c("cr", "prop"), label_type = "full", lang = "en")
add_var_labels("asr", break_line = FALSE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_var_labels", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ageadjust")
### * ageadjust

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ageadjust
### Title: Calculate the age standardized rate using the direct method
### Aliases: ageadjust

### ** Examples

cases <- c(50, 60, 45, 70)
pop <- c(1000, 1200, 1100, 900)
spop <- c(800, 1000, 1100, 900)
ageadjust(cases, pop, stdpop = spop, mp = 100000)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ageadjust", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_age")
### * calc_age

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_age
### Title: Calculate the actual age in completed years between two dates
### Aliases: calc_age

### ** Examples

# Generate random birth dates
set.seed(123)
sdate <- as.Date("1960-01-01")
edate <- as.Date("1980-12-31")
bdate <- sample(seq(sdate, edate, by = "1 day"), 100, replace = TRUE)

# Generate random event dates
sdate <- as.Date("2020-01-01")
edate <- as.Date("2023-07-08")
event <- sample(seq(sdate, edate, by = "1 day"), 100, replace = TRUE)

# Calculate ages
ages <- calc_age(bdate, event)
head(ages)
# Handle missing values
bdate[1] <- NA
event[2] <- NA
calc_age(bdate, event)[1:5]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_age", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("canregs")
### * canregs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: canregs
### Title: Example Population-Based Cancer Registry Data ('canregs')
### Aliases: canregs
### Keywords: datasets

### ** Examples


data("canregs")
summary(canregs[[1]])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("canregs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("classify_areacode")
### * classify_areacode

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: classify_areacode
### Title: Classify codes for the administrative divisions of China
### Aliases: classify_areacode

### ** Examples

classify_areacode(c("110000", "320500", "440300"))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("classify_areacode", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("classify_areacode2")
### * classify_areacode2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: classify_areacode2
### Title: Classify codes for the administrative divisions of China
### Aliases: classify_areacode2

### ** Examples

classify_areacode(c("110000", "320500", "440300"))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("classify_areacode2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("classify_childhood")
### * classify_childhood

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: classify_childhood
### Title: Classify childhood cancer according to the ICCC3 standards
### Aliases: classify_childhood

### ** Examples

topo <- c("C15.2", "C16.2", "C34.2")
morp <- c("8000", "8040", "8170")
beha <- c("3", "3", "3")
child_code <- classify_childhood(topo, morp, beha, type = "main")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("classify_childhood", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("classify_icd10")
### * classify_icd10

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: classify_icd10
### Title: Classify ICD10 codes to cancer categories
### Aliases: classify_icd10

### ** Examples

icd10 <- c("C15.2", "C33.4", "C80.9", "C26.2", "C16.3")
classify_icd10(icd10, cancer_type = "big")
classify_icd10(icd10, cancer_type = "small")
classify_icd10(icd10, cancer_type = "system")
classify_icd10(icd10, cancer_type = "gco")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("classify_icd10", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("classify_morp")
### * classify_morp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: classify_morp
### Title: Classify ICD-O-3 morphology codes into categories
### Aliases: classify_morp

### ** Examples

morps <- c("8140","8070","8050","8051","9900","9800","9993")
classify_morp(morps)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("classify_morp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("count_canreg")
### * count_canreg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: count_canreg
### Title: Count and classify 'canreg' data
### Aliases: count_canreg count_canreg.canregs count_canreg.canreg

### ** Examples

data("canregs")
fbsw <- count_canreg(canregs, age_breaks = c(0, 15, 65), cancer_type = "big")
fbsw <- count_canreg(canregs, cancer_type = "gco")

# Count object with class of `canregs`
fbsw <- count_canreg(canregs, cancer_type = "small")

# Count object with class of `canreg`
fbsw <- count_canreg(canregs[[1]], cancer_type = "big")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("count_canreg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cr_clean")
### * cr_clean

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cr_clean
### Title: Clean canreg data.
### Aliases: cr_clean cr_clean.canregs cr_clean.canreg cr_clean.FBcases
###   cr_clean.SWcases cr_clean.POP

### ** Examples

data("canregs")
data <- cr_clean(canregs)

data <- cr_clean(canregs, cancer_type = "small")

data <- cr_clean(canregs[[1]], cancer_type = "big")

fbcases <- purrr::pluck(canregs[[1]], "FBcases")
fbcases <- cr_clean(fbcases)

swcases <- purrr::pluck(canregs[[1]], "SWcases")
swcases <- cr_clean(swcases)

pop <- purrr::pluck(canregs[[1]], "POP")
pop <- cr_clean(pop)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cr_clean", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cr_merge")
### * cr_merge

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cr_merge
### Title: Merge elements from objects of class 'canregs', 'fbswicds', or
###   'asrs'
### Aliases: cr_merge cr_merge.canregs cr_merge.fbswicds cr_merge.asrs
###   cr_merge.qualities cr_merge.age_rates cr_merge.summaries

### ** Examples

data("canregs")
canreg <- cr_merge(canregs)
class(canreg)

# Merge obejct with class of `fbswicds` into obejct with class of `fbswicd`
fbsws <- count_canreg(canregs)
fbsw <- cr_merge(fbsws)

# Merge obejct with class of `asrs` into object with class of `asr`
asrs <- create_asr(canregs, year, sex, cancer, collapse = FALSE)
asr <- cr_merge(asrs)

# Merge obejct with class of `qualities` into object with class of `quality`
quas <- create_quality(canregs, year, sex, cancer, collapse = FALSE)
qua <- cr_merge(quas)

# Merge obejct with class of `age_rates` into object with class of `age_rate`
agerates <- create_age_rate(canregs, year, sex, cancer, collapse = FALSE)
agerate <- cr_merge(agerates)

# Merge obejct with class of `summaries` into object with class of `summary`
summs <- summary(canregs, collapse = FALSE)
summ <- cr_merge(summs)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cr_merge", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cr_reframe")
### * cr_reframe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cr_reframe
### Title: Reframe data of class canregs or fbswicds
### Aliases: cr_reframe cr_reframe.canregs cr_reframe.fbswicds

### ** Examples

# list reframe vars that could be used in `strat` parameter
ls_vars("reframe")
data("canregs")
# Reframe the `canregs` data according to `area_type` attribute
city <- cr_reframe(canregs, strat = "area_type")

# Reframe object with class of `canregs`
# Reframe the `canregs` according to the `province` attribute
province <- cr_reframe(canregs, strat = "province")
class(province)
names(province)

# Reframe object with class of `fbswicds`
# Convert object with class of `canregs` into object with class of `fbswicds`
fbsw <- count_canreg(canregs, cancer_type = "small")
# Reframe the `fbswicds` according to the `city` attribute
city <- cr_reframe(fbsw, strat = "city")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cr_reframe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cr_select")
### * cr_select

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cr_select
### Title: Select elements from objects with class '"canregs"', or
###   '"fbswicds"'
### Aliases: cr_select cr_select.canregs cr_select.asrs cr_select.age_rates
###   cr_select.fbswicds cr_select.summaries

### ** Examples

data("canregs")
# Select elements which mi greather than 0.5 from `canregs`
canregs_mi <- cr_select(canregs, mi > 0.5)

# Select elements from obejct with class of `fbswicds`
fbsws <- count_canreg(canregs)
# Select elements which `inci` greater than 250 per 100000 population
fbsws_inci <- cr_select(fbsws, inci > 250)

# Select elements from object with class of `summaries`
summ <- summary(canregs, collapse = FALSE)
# Select elements for whcih `mi` greater than 0.5
summ_mi <- cr_select(summ, mi > 0.5)
names(summ_mi)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cr_select", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_age_rate")
### * create_age_rate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_age_rate
### Title: Calculate age specific rate
### Aliases: create_age_rate create_age_rate.canreg create_age_rate.canregs
###   create_age_rate.fbswicds create_age_rate.fbswicd

### ** Examples

data("canregs")
agerate <- create_age_rate(canregs, year, sex, cancer)

data <- canregs[[1]]
agerate <- create_age_rate(data, year, sex)

agerate <- create_age_rate(canregs, year, cancer_type = "system")

fbsws <- count_canreg(canregs)
agerate <- create_age_rate(fbsws, year, sex)

data <- canregs[[2]]
fbsw <- count_canreg(data, cancer_type = "small")
agerate <- create_age_rate(fbsw, year, sex, cancer)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_age_rate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_asr")
### * create_asr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_asr
### Title: Calculate age-standardized rate (ASR)
### Aliases: create_asr create_asr.canregs create_asr.canreg
###   create_asr.fbswicds create_asr.fbswicd

### ** Examples

data("canregs")
asr_inci <- create_asr(canregs, event = "fbs", year, sex, cancer)
asr_mort <- create_asr(canregs, event = "sws", year, sex, cancer)

# calculate ASR based on object with class of `canregs`
asr <- create_asr(canregs, event = "sws", year, sex, cancer)

# calculate ASR based on object with class of `canreg`
data <- canregs[[1]]
# calculate ASR using default parameter
asr <- create_asr(data, year, sex, cancer)
head(asr)
# calculate ASR using multiple standard population
asr_multi_std <- create_asr(data, year, sex, cancer,
  std = c("cn82", "cn2000", "wld85")
)
head(asr_multi_std)
# calculate ASR with confidence interval
asr_with_ci <- create_asr(data, year, sex, cancer, show_ci = TRUE)
head(asr_with_ci)
# calculate ASR with population at risk
asr_with_pop <- create_asr(data, year, sex, cancer, show_pop = TRUE)
head((asr_with_pop))
# calculate ASR with variance
asr_with_var <- create_asr(data, year, sex, cancer, show_var = TRUE)
head(asr_with_var)

# calculate ASR based on object with class of `fbswicds`
# convert object with class of `canregs` to object with class of `fbswicds`
fbsws <- count_canreg(canregs)
asrs <- create_asr(fbsws, event = "sws", year, sex, cancer)

# calculate ASR based on object with class of `fbswicd`
# convert object with class of `canreg` to object with class of `fbswicd`
fbsw <- count_canreg(canregs[[1]])
asr <- create_asr(fbsw, event = "sws", year, sex, cancer)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_asr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_quality")
### * create_quality

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_quality
### Title: Calculate quality indicators
### Aliases: create_quality create_quality.canreg create_quality.canregs
###   create_quality.fbswicds create_quality.fbswicd

### ** Examples

data("canregs")
fbsws <- count_canreg(canregs, cancer_type = "system")
qua2 <- create_quality(fbsws, year, sex, cancer)
head(qua2)

# Calculate the quality indicators based on object with class of `canreg`
data <- canregs[[1]]
qua <- create_quality(data, year, sex, cancer, cancer_type = "big")
head(qua)

# Calculate the quality indicators based on object with class of `canregs`
qua <- create_quality(canregs, year, sex, cancer, cancer_type = "big")
head(qua)

# Calculate the quality indicators based on object with class of `fbswicds`
fbsws <- count_canreg(canregs, cancer_type = "small")
qua <- create_quality(fbsws, year, sex, cancer)
head(qua)

# Calculate the quality indicators based on object with class of `fbswicd`
fbsw <- count_canreg(canregs[[1]], cancer_type = "big")
qua <- create_quality(fbsw, year, sex, cancer)
head(qua)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_quality", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_report")
### * create_report

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_report
### Title: Render standardized cancer registry reports from built-in
###   templates
### Aliases: create_report create_report.canregs create_report.canreg

### ** Examples

## Not run: 
##D data("canregs")
##D create_report(canregs, template = "quality", title = "QC Report")
## End(Not run)

## Not run: 
##D create_report(canregs, template = "annual", title = "Annual Report")
## End(Not run)

## Not run: 
##D data <- canregs[[1]]
##D create_report(data, template = "annual", title = "Annual Report")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_report", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cumrate")
### * cumrate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cumrate
### Title: Calculate the cumulative incidence or mortality rate
### Aliases: cumrate

### ** Examples

px <- c(
  20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
  163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909
)
dx <- c(
  156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
  891, 831, 926, 731, 269
)
mx <- dx / px
cumrate(mx, eage = 70)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cumrate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cumrisk")
### * cumrisk

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cumrisk
### Title: Calculate the cumulative risk
### Aliases: cumrisk

### ** Examples

px <- c(
  20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
  163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909
)
dx <- c(
  156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
  891, 831, 926, 731, 269
)
mx <- dx / px
cumrate(mx, eage = 70)
cumrisk(cumrate(mx, eage = 70))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cumrisk", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cutage")
### * cutage

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cutage
### Title: Group ages into categories
### Aliases: cutage

### ** Examples

ages <- sample(0:101, 200, replace = TRUE)
cutage(ages, method = "distance", length = 5, maxage = 60, sep_zero = TRUE)
# Custom breaks
cutage(ages, method = "interval", breaks = c(0, 15, 30, 45, 60, 75, Inf))
# Quantile-based grouping
cutage(ages, method = "quantile")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cutage", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("del_dict_files")
### * del_dict_files

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: del_dict_files
### Title: Delete a dictionary file from canregtools config
### Aliases: del_dict_files

### ** Examples

del_dict_files("custom")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("del_dict_files", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("draw_barchart")
### * draw_barchart

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: draw_barchart
### Title: Draw Grouped Bar Charts with Facets
### Aliases: draw_barchart

### ** Examples

data("canregs")
asr <- create_asr(canregs[[1]], year, sex, cancer, event = "fbs")
asr <- cr_filter(asr, drop = c("total", "others"))
draw_barchart(asr, x = cancer, y = cr, group =  year, facet = sex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("draw_barchart", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("draw_dumbbell")
### * draw_dumbbell

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: draw_dumbbell
### Title: Plot dumbbell chart
### Aliases: draw_dumbbell

### ** Examples

asr <- create_asr(canregs[[1]], year, cancer, show_ci = TRUE) |>
  drop_others() |>
  drop_total() |>
  add_labels(vars = "cancer", lang = "en", label_type = "abbr")
draw_dumbbell(asr, "cancer_en", asr_lower_cn2000, asr_upper_cn2000, topn = 15)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("draw_dumbbell", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("draw_linechart")
### * draw_linechart

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: draw_linechart
### Title: Draw a Custom Line Chart Using Base R
### Aliases: draw_linechart

### ** Examples

data("canregs")
fbsw <- count_canreg(canregs[[1]], label_tail="yrs")
agerate <- create_age_rate(fbsw, year, sex)
agerate <- add_labels(agerate, lang = "en")
draw_linechart(agerate, agegrp, rate, sex)
agerate <- create_age_rate(fbsw, year, sex, cancer)
agerate <- add_labels(agerate, lang = "en")
agerate <- dplyr::filter(agerate, cancer %in% as.character(c(103:106)))
draw_linechart(agerate, agegrp, rate, sex, cancer, grid = c(2, 2))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("draw_linechart", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("draw_pyramid")
### * draw_pyramid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: draw_pyramid
### Title: Plot a population pyramid
### Aliases: draw_pyramid

### ** Examples

data("canregs")
pop <- canregs[[1]]$POP
draw_pyramid(pop, agegrp, rks, sex)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("draw_pyramid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("drop_others")
### * drop_others

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: drop_others
### Title: Drop “other” cancer codes
### Aliases: drop_others

### ** Examples

data("canregs")
asr <- create_quality(canregs, year, sex, cancer)
asr2 <- asr |> drop_others()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("drop_others", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("drop_total")
### * drop_total

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: drop_total
### Title: Drop the total cancer
### Aliases: drop_total

### ** Examples

data("canregs")
qua <- create_quality(canregs, year, sex, cancer)
qua2 <- qua |> drop_total()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("drop_total", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("esti_pop")
### * esti_pop

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: esti_pop
### Title: Estimating population structure using interpolation method.
### Aliases: esti_pop

### ** Examples


pop1 <- c(
  59546, 294129, 472511, 552549, 821119, 996436, 805635, 1004506,
  989357, 1056612, 986559, 792270, 544544, 452297, 473579, 350802,
  212614, 109598, 61990
)
pop2 <- c(
  75641, 377276, 327116, 380338, 539034, 1158852, 1152329, 881443,
  903484, 1011164, 1238871, 1137832, 1022787, 645441, 464777,
  482941, 406144, 227977, 144526
)
esti_pop(pop1, pop2, c(2000, 2010))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("esti_pop", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("expand_age_pop")
### * expand_age_pop

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: expand_age_pop
### Title: Expand population data from 5-year age groups to single-year
###   ages
### Aliases: expand_age_pop

### ** Examples

# Example population data for 19 age groups: 0, 1–4, 5–9, ..., 85+
ages <- c(
  5053, 17743, 25541, 32509, 30530, 34806, 36846, 38691, 40056,
  39252, 37349, 30507, 26363, 21684, 15362, 11725, 7461, 3260, 915
)
eages <- expand_age_pop(ages)
head(eages)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("expand_age_pop", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("expand_lifetable")
### * expand_lifetable

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: expand_lifetable
### Title: Expand an five-year abridged life table to complete life table
### Aliases: expand_lifetable

### ** Examples

# Example abridged life table data (normalized to a radix of 1)
lx <- c(
  100000, 99498.39, 99294.62, 99173.88, 99047.59, 98840.46,
  98521.16, 98161.25, 97636.99, 96900.13, 95718.96, 93930.91,
  91463.21, 87131.41, 80525.02, 70907.59, 58090.75, 41630.48,
  24019.33
)
lx <- lx / 100000
expand_lifetable(lx)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("expand_lifetable", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_stdpop")
### * get_stdpop

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_stdpop
### Title: Get standardized population data
### Aliases: get_stdpop

### ** Examples

## Not run: 
##D   get_std("cn64")
##D   get_std("wld2000")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_stdpop", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ls_attrs")
### * ls_attrs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ls_attrs
### Title: list built-in attributes name used in 'cr_reframe()'
### Aliases: ls_attrs

### ** Examples

ls_attrs()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ls_attrs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ls_dict")
### * ls_dict

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ls_dict
### Title: list dictionary used in package
### Aliases: ls_dict

### ** Examples

ls_dict("registry")
ls_dict("area_type")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ls_dict", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ls_dict_files")
### * ls_dict_files

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ls_dict_files
### Title: List dictionary files used by canregtools
### Aliases: ls_dict_files

### ** Examples

ls_dict_files()
ls_dict_files(full.names = TRUE)
ls_dict_files(with_info = TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ls_dict_files", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ls_vars")
### * ls_vars

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ls_vars
### Title: List variable names and their descriptions by category
### Aliases: ls_vars

### ** Examples

ls_vars("std")
ls_vars("summary")
ls_vars("reframe")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ls_vars", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lt")
### * lt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lt
### Title: Compute a life table from age-specific mortality rates
### Aliases: lt

### ** Examples

# Example 1: Using mortality rates derived from death and population counts
px <- c(
  20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
  163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909
)
dx <- c(
  156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
  891, 831, 926, 731, 269
)
mx <- dx / px
lt(mx, sage = 0, age_width = 5, sex = "male")

# Example 2: Using predefined mortality rates
mx <- c(
  0.01685, 0.00085, 0.00044, 0.00045, 0.00064, 0.00086, 0.00103,
  0.00136, 0.00195, 0.00291, 0.00429, 0.00672, 0.00985, 0.01596,
  0.02605, 0.04536, 0.07247, 0.12078, 0.17957, 0.25938, 0.25989
)
lt(mx, sage = 0, age_width = 5, sex = "total")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lt2")
### * lt2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lt2
### Title: Compute a life table from age-specific mortality rates
### Aliases: lt2

### ** Examples

# Example 1: Using mortality rates derived from death and population counts
px <- c(
  20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
  163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909
)
dx <- c(
  156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
  891, 831, 926, 731, 269
)
mx <- dx / px
lt(mx, sage = 0, age_width = 5, sex = "male")

# Example 2: Using predefined mortality rates
mx <- c(
  0.01685, 0.00085, 0.00044, 0.00045, 0.00064, 0.00086, 0.00103,
  0.00136, 0.00195, 0.00291, 0.00429, 0.00672, 0.00985, 0.01596,
  0.02605, 0.04536, 0.07247, 0.12078, 0.17957, 0.25938, 0.25989
)
lt(mx, sage = 0, age_width = 5, sex = "total")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lt2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("quality")
### * quality

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: quality
### Title: Quality Indicators from the China Cancer Registry Annual Report
### Aliases: quality
### Keywords: datasets

### ** Examples


data("quality")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("quality", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_canreg")
### * read_canreg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_canreg
### Title: Read cancer registration data
### Aliases: read_canreg

### ** Examples

## Not run: 
##D file_address <- "410302.xlsx"
##D canreg <- read_canreg(file_address, pop_type = "long")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_canreg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("show_registry")
### * show_registry

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: show_registry
### Title: Get the areacodes of the registry
### Aliases: show_registry

### ** Examples

show_registry(1:4)
show_registry(c("1", "2"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("show_registry", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary")
### * summary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.canreg
### Title: Summary object of class 'canreg'
### Aliases: summary.canreg summary.canregs

### ** Examples

data("canregs")
data <- canregs[[1]]
summary(data)


summary(canregs)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_age")
### * tidy_age

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_age
### Title: Tidy age description
### Aliases: tidy_age

### ** Examples

agedes <- c(
  "50\u5c8110\u67083\u6708", "19\u5c815\u6708",
  "1\u5c8130\u6708", "3\u670820\u6708", "30\u6708"
)
tidy_age(agedes, unit = "year")
tidy_age(agedes, unit = "month")
tidy_age(agedes, unit = "day")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_age", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_sex")
### * tidy_sex

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_sex
### Title: Tidy gender variable
### Aliases: tidy_sex

### ** Examples

gender <- c("male", "men", "women", "female", "women", "man", "1", "2")
tidy_sex(gender)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_sex", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_var")
### * tidy_var

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_var
### Title: Reformat variable values for Cancer Registration in China
### Aliases: tidy_var

### ** Examples

occu <- c("11", "13", "17", "21", "24", "27", "31", "37", "51", "80", "90")
tidy_var(occu, var_name = "occu", lang = "cn")
tidy_var(occu, var_name = "occu", lang = "en")
tidy_var(occu, var_name = "occu", lang = "cn", label_type = "abbr")
tidy_var(occu, var_name = "occu", lang = "en", label_type = "abbr")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_var", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("truncrate")
### * truncrate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: truncrate
### Title: Calculate truncated age standardized rate
### Aliases: truncrate

### ** Examples

px <- c(
  20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
  163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909
)
dx <- c(
  156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
  891, 831, 926, 731, 269
)
stdpop <- c(2.4, 9.6, 10, 9, 9, 8, 8, 6, 6, 6, 6, 5, 4, 4, 3, 2, 1, 0.5, 0.5)
truncrate(dx, px, stdpop, trunc_age = c(35, 64))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("truncrate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_areacode")
### * write_areacode

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_areacode
### Title: Write custom six-digit administrative division codes to
###   dictionary
### Aliases: write_areacode

### ** Examples

## Not run: 
##D dict <- data.frame(
##D   areacode = c("410302"),
##D   cname = c("\u8001\u57CE\u533A"),
##D   ename = c("Laocheng District"),
##D   abbr_cn = c("\u8001\u57CE"),
##D   abbr_en = c("Laocheng")
##D )
##D write_areacode(dict)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_areacode", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_registry")
### * write_registry

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_registry
### Title: Write registry attributes to a user-defined dictionary
### Aliases: write_registry write_registry.data.frame write_registry.list
###   write_registry.NULL write_registry.character

### ** Examples

write_registry(list('410302' = '410301'))

# Registry attributes stored in data frame
registry_dict <- data.frame(
areacode = c("410302", "410303", "410304", "410305", "410306", "410307"),
registry = c(rep("410301", 5), "410300"),
area_type = rep("urban", 6)
)
write_registry(registry_dict)

# Registry attributes stored in list
dict <- list(
'410302' = '410301',
'410303' = '410301',
'410304' = '410301',
'410305' = '410301',
'410306' = '410301',
'410307' = '410301'
)
write_registry(dict)

# Registry attributes using built-in information
dict <- NULL
write_registry(dict)

# Registry attributes stored in named character vector with areacode as
# name and attributes as values 
dict <- rep("410301", 5)
names(dict) <- c("410302", "410303", "410304", "410305", "410306")
write_registry(dict)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_registry", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
