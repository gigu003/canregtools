## code to prepare `label_child` dataset goes here

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


