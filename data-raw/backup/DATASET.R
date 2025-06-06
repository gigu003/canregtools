
## code create the system label dataset.
child_main <- data.frame(
  en = c("I. Leukemias, MPD, & MDS",
         "II. Lymphomas & ralated",
         "III. CNS",
         "IV. Neuroblastoma & related",
         "V. Retinoblastoma",
         "VI. Renal tumors",
         "VII. Hepatic tumors",
         "VIII. Malignant bone tumors",
         "IX. Soft tissue sarcomas",
         "X. Germ cell tumors",
         "XI. Other epithelial & melanomas",
         "XII. Other & unspecified",
         "Not Classified or in situ"),
  cn = c("I. 白血病",
         "II. 淋巴瘤",
         "III. 中枢神经系统",
         "IV. 神经母细胞瘤",
         "V. 视网膜母细胞瘤",
         "VI. 肾肿瘤",
         "VII. 肝脏肿瘤",
         "VIII. 骨肿瘤",
         "IX. 软组织肉瘤",
         "X. 生殖细胞肿瘤",
         "XI. 其他恶性上皮和黑色素瘤",
         "XII. 其他和未特指",
         "未分类或原位癌;"))
child_sub <- data.frame(
  en = c(
    "Ia. Lymphod leukemias", "Ib. Acute myeloid", "Ic. CMD", "Id. MDS & other", "Ie. Unspecified",
    "IIa. Hodgkin", "IIb. Non-Hodgkin except BL", "IIc. Burkitt(BL)", "IId. Lymphoreticular", "IIe. Unspecified",
    "IIIa. Ependymoma", "IIIb. Astroocytomas", "IIIc. Intracranial and intraspinal embryonal", "IIId. Other gliomas", "IIIe. Other specified", "IIIf. Unspecified CNS",
    "IVa. Neuroblastoma and ganglioneuroblastoma", "IVb. Other peripheral nervous cell tumors",
    "V. Retinoblastoma",
    "VIa. Nephroblastoma and other nonepithelial renal tumors", "VIb. Renal carcinomas", "VIc. Unspecified malignant renal tumors",
    "VIIa. Hepatoblastoma", "VIIb. Hepatic carcinomas", "VIIc.Unspecified malignant hepatic tumors",
    "VIIIa. Osteosarcomas", "VIIIb. Chondrosarcomas", "VIIIc. Ewing tumor and related sarcomas of bone", "d. Other specified malignant bone tumors", "e. Unspecified malignant bone tumors",
    "IXa. Rhabdomyosarcomas", "IXb. Fibrosarcomas, peripheral nerve sheath tumors, and other fibrous neoplasms", "IXc. Kaposi sarcoma", "IXd. Other specified soft tissue sarcomas", "IXe. Unspecified soft tissue sarcomas",
    "Xa. Intracranial and intraspinal germ cell tumors", "Xb. Malignant extracranial and extragonadal germ cell tumors", "Xc. Malignant gonadal germ cell tumors", "Xd. Gonadal carcinomas", "Xe. Other and unspecified malignant gonadal tumors",
    "XIa. Adrenocortical carcinomas", "XIb. thyroid carcinomas", "XIc. Nasopharyngeal carcinomas", "XId. Malignant melanomas", "XIe. Skin carcinomas", "XIf. Other and unspecified carcinomas",
    "XIIa. Other specified malignant tumors", "XIIb. Other unspecified malignant tumors",
    "Not Classified by ICCC or in situ"),
  cn = c(
    "Ia. 淋巴白血病", "Ib. 急性髓系白血病", "Ic. 严重骨髓增生异常症", "Id. 骨髓增生异常症及其他", "Ie. 未指定",
    "IIa. 霍奇金病", "IIb. 非霍奇金淋巴瘤（除外Burkitt淋巴瘤）", "IIc. Burkitt淋巴瘤（BL）", "IId. 淋巴造血系统及其他", "IIe. 未指定",
    "IIIa. 室管膜瘤", "IIIb. 星形细胞瘤", "IIIc. 颅内和脊髓胚胎型肿瘤", "IIId. 其他神经胶质瘤", "IIIe. 其他特指的", "IIIf. 未指定的中枢神经系统肿瘤",
    "IVa. 神经母细胞瘤和神经节细胞神经母细胞瘤", "IVb. 其他周围神经细胞肿瘤",
    "V. 视网膜母细胞瘤",
    "VIa. 肾母细胞瘤及其他非上皮性肾肿瘤", "VIb. 肾癌", "VIc. 未指定的恶性肾肿瘤",
    "VIIa. 肝母细胞瘤", "VIIb. 肝癌", "VIIc. 未指定的恶性肝肿瘤",
    "VIIIa. 骨肉瘤", "VIIIb. 软骨肉瘤", "VIIIc. 尤因肿瘤和相关的骨肉瘤", "d. 其他特指的恶性骨肿瘤", "e. 未指定的恶性骨肿瘤",
    "IXa. 横纹肌肉瘤", "IXb. 纤维肉瘤、周围神经鞘肿瘤和其他纤维性肿瘤", "IXc. 卡波西肉瘤", "IXd. 其他特指的软组织肉瘤", "IXe. 未指定的软组织肉瘤",
    "Xa. 颅内和脊髓生殖细胞瘤", "Xb. 恶性的体外和体腔外生殖细胞瘤", "Xc. 恶性的生殖腺生殖细胞瘤", "Xd. 生殖腺癌", "Xe. 其他和未指定的恶性生殖腺肿瘤",
    "XIa. 肾上腺皮质癌", "XIb. 甲状腺癌", "XIc. 鼻咽癌", "XId. 恶性黑色素瘤", "XIe. 皮肤癌", "XIf. 其他和未指定的癌症",
    "XIIa. 其他特指的恶性肿瘤", "XIIb. 其他未指定的恶性肿瘤",
    "不被ICCC分类或原位癌")
)

label_child <- list(child_main, child_sub)


## ICDO3 'Topo' sites and 'Behas' mapping with 'ICD10' codes.

library(dplyr)
library(tidyr)
library(stringr)
library(googlesheets4)


# address <- "https://docs.google.com/spreadsheets/d/"
# googlesheetid <- "1cxUcA-odXV_A3_XKaXaMzJtEmVAFkqv05Zml7W6VxuU"
# 读取google sheet保存的icdo3 topo和beha编码与icd10的对应表
# topo_to_icd10 <- read_sheet(paste0(address, googlesheetid),
#                             sheet = "topo",
#                             range = "A4:K334")
# 提取并整理ICDO3 topo编码
topo_to_icd10 <- read.csv("~/website/db/data/topo.csv")
topo <- topo_to_icd10$topo
topo_dict <- gsub("\\.", "", topo, perl = TRUE)

## ICDO3 behaviour codes list.
behas <- c("3", "6", "2", "0", "1", "mela_3", "mela_2", "meso_3", "kaposi_3")

# 去除第一列和第七列，形成topo和behaviour编码与ICD10编码对应表
topo_to_icd10 <- topo_to_icd10[, -c(1, 7)]
topo_to_icd10 <- as.matrix(topo_to_icd10)
rownames(topo_to_icd10) <- topo_dict
colnames(topo_to_icd10) <- behas


# data <- read_sheet(paste0(address, googlesheetid),
#   sheet = "histologies_2023",
#   col_types = "cccccc"
# )

data <- read.csv("~/website/db/data/morp.csv")
morp2 <- read.csv("~/website/db/data/morp2.csv")


morp1 <- data %>%
  mutate(morp = paste0("M",substr(morp,1, 6))) %>%
  select(morp, morp_desen, comments) 

morp_o3_2 <- morp1 %>%
  full_join(morp2, by = ("morp")) %>%
  arrange(morp) %>%
  filter(mark == 0) %>%
  filter(!comments == "Not listed in ICD-O-3.2")


# 提取ICDO3-2的形态学词典
morp_dict <- unique(substr(data$morp, 1, 4))



## 提取topo、morp、beha和icd10的对应表
all <- data %>%
  rename_all(tolower) %>%
  mutate(
    o3site = toupper(as.character(o3site)),
    morp1 = substr(morp, 1, 4),
    beha = substr(morp, 6, 7)
  ) %>%
  filter(!(o3site %in% c("SPECIFIC SITES", "ALL SITES"))) %>%
  mutate(
    topo = gsub(", ALL OTHER SITES", "", o3site),
    topo = gsub(", SPECIFIC SITES", "", topo),
    icd10 = gsub("\\s+", "", icd10),
    icd10 = gsub(",SeeTopoTableBColB", "", icd10),
    icd10 = gsub(",SeeTopoTableB,ColB", "", icd10),
    icd10 = gsub(",SeeTopoTableBColD", "", icd10),
    icd10 = gsub(",SeeTopoTableBColF", "", icd10),
    icd10 = gsub(",SeeTopoTableB,ColF", "", icd10),
    icd10 = gsub(",SeeTopoTableBColE", "", icd10),
    icd10 = gsub(",SeeTopoTableB,ColE", "", icd10),
    icd10 = gsub(",SeeTopoTableBColC", "", icd10),
    icd10 = gsub(",SeeTopoTableBColH", "", icd10),
    icd10 = gsub(",SeeTopoTableBColI", "", icd10),
    icd10 = gsub(",SeeTopoTableBColJ", "", icd10),
    icd10 = gsub(",SeeTopoTableCColD", "", icd10),
    icd10 = gsub("SeeTopoTableBColB", "", icd10),
    icd10 = gsub(",AllOtherSites", "", icd10)
  ) %>%
  select(morp1, beha, topo, icd10) %>%
  distinct(topo, morp1, beha, icd10) %>%
  rename(morp = morp1)
part1 <- all %>%
  filter(nchar(topo) <= 4, !is.na(icd10))
part2 <- all %>%
  filter(nchar(topo) > 4, !is.na(icd10)) %>%
  mutate(
    topo = gsub("[FM]", "", topo),
    topo = gsub("\\[\\]", "", topo),
    icd10 = gsub("[MF]", "", icd10),
    icd10 = gsub("\\[\\]|\\(\\)", "", icd10)
  ) %>%
  separate_wider_delim(topo, delim = ",", names = c("topo1", "topo2")) %>%
  separate_wider_delim(icd10,
    delim = ",", names = c("icd10_1", "icd10_2"),
    too_few = "align_start"
  )

all_to_icd10 <- bind_rows(
  part2 %>%
    select(topo1, morp, beha, icd10_1) %>%
    rename(topo = topo1, icd10 = icd10_1),
  part2 %>%
    select(topo2, morp, beha, icd10_2) %>%
    rename(topo = topo2, icd10 = icd10_2),
  part1
) %>%
  mutate(
    topo = gsub("\\s", "", topo),
    morp = gsub("\\s", "", morp),
    beha = gsub("\\s", "", beha),
    icd10 = gsub("\\s", "", icd10)
  ) %>%
  filter(!is.na(icd10)) %>%
  arrange(topo, morp, beha)


## 提取morp, beha, 和icd10的对应表
morp_to_icd10 <- data %>%
  mutate(o3site = tolower(o3site)) %>%
  filter(o3site == "all sites") %>%
  mutate(
    morp1 = substr(morp, 1, 4),
    beha = substr(morp, 6, 6)
  ) %>%
  select(morp1, beha, icd10) %>%
  rename(morp = morp1) %>%
  bind_rows(all_to_icd10[, 2:4]) %>%
  distinct(morp, beha, .keep_all = TRUE) %>%
  arrange(morp, beha) %>%
  pivot_wider(names_from = beha, values_from = icd10, values_fill = "*") %>%
  arrange(morp)

morp_to_icd10 <- as.matrix(morp_to_icd10)
rownames(morp_to_icd10) <- morp_to_icd10[, 1]
morp_to_icd10 <- morp_to_icd10[, -1]

## 把信息写入系统数据
usethis::use_data(label, std_pop, topo_dict, morp_dict, topo_to_icd10,
  morp_to_icd10, all_to_icd10, label_child, ethnic_map, occu_map, morp_o3_2,
  prov_label, region_label, registry_dict, not_icd10, iarc_check_site_morp,
  iccc3_2005,
  internal = TRUE, overwrite = TRUE
)
