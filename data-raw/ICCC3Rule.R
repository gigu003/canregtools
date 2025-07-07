## code to prepare `ICCC3Rule` dataset goes here
library(rvest)
library(crcheck)
# Extract the ICCC3-2005 main classification from SEER website.
url <- "https://seer.cancer.gov/iccc/iccc3.html"
table <- html_table(read_html(url))[[1]]
iccc3_2005 <- table[-c(1, 7, 13, 23, 28, 34, 39, 48, 61, 69, 77, 81), ]
colnames(iccc3_2005) <- c("desc", "morp", "topo", "recode")
iccc3_2005 <- list(
  morp = lapply(strsplit(iccc3_2005$morp, ","), recode_morp),
  topo = lapply(strsplit(iccc3_2005$topo, ","), recode_topo),
  recode = as.list(iccc3_2005$recode)
)

v2005 <- list(
  main = iccc3_2005
)

# Extract the ICCC3-2005 extended classification from SEER website.
url <- "https://seer.cancer.gov/iccc/iccc3_ext.html"
table <- html_table(read_html(url))[[1]]


# Extract the ICCC3-2017 version from SEER website.
url2017 <- "https://seer.cancer.gov/iccc/iccc-iarc-2017.html"
table <- html_table(read_html(url2017))[[1]]
mm <- data.frame(
  stringsAsFactors = FALSE,
  V0 = c(rep("(a.4) Lymphoid leukemia, NOS", 2)),
  V1 = c("9591", "9820"),
  V2 = c("420, 421, 423, 424", "000-809"),
  V3 = c("3", "3"),
  V4 = c("004", "004"),
  V5 = c("011", "011")
)
cols <- c("group", "morp", "topo", "beha", "recode1", "recode2")
colnames(table) <- cols
colnames(mm) <- cols

table2 <- table |>
  filter(
    recode1 %in% sprintf("%03d", c(0:115, 999)),
    !group == "9820"
  ) |>
  bind_rows(mm) |>
  arrange(recode1, group) |>
  filter(!morp == "")

iccc3_2017 <- list(
  morp = lapply(strsplit(table2$morp, ","), recode_morp),
  topo = lapply(strsplit(table2$topo, ","), recode_topo),
  extend = as.list(table2$recode1),
  recode = as.list(table2$recode2)
)


ICCC3Rule <- list(
  v2005 = v2005,
  v2017 = iccc3_2017
)

rm(url, table, iccc3_2005, iccc3_2017, v2005, mm, table2, cols, url2017)
