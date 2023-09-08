#' Convert ICDO3 codes to ICD10 code.
#'
#' @description
#' Convert the ICDO3 codes to ICD10 code based on Topography, Morphology, and
#' Behavior value.
#'
#' @details
#' ICD-O-3 (International Classification of Diseases for Oncology, 3rd Edition)
#' and ICD-10 (International Classification of Diseases, 10th Edition) are two
#' different coding systems used for disease classification. ICD-10 is a widely
#' used coding system that can describe various diseases, including tumor
#' diseases. On the other hand, ICD-O-3 is specifically designed for the coding
#' of tumor diseases. Promoting and using ICD-O-3 and ICD-10 helps
#' in standardizing and unifying the classification and coding of tumor,
#' facilitating global tumor epidemiology research, statistical analysis,
#' and clinical management. ICD-O-3 consists of three main components:
#' Topography, Morphology, and Behavior. The Topography code uses a four-digit
#' number to describe the specific location where the tumor occurs. The
#' Morphology code uses a three-digit number to represent the morphology or
#' histology type of the tumor. The Behavior code describes the behavior
#' characteristics of the tumor. With this program, you can convert ICD-O-3
#' codes to ICD-10 codes, making it convenient for research and
#' clinical practice.
#'
#' @param topo A vector containing ICDO3 topography codes, it can be
#'        in the format of either "C15.1" or "C151".
#' @param morp A vector containing ICDO3 morphology codes, it could be in the
#'        format of "M8170" or "8170".
#' @param beha A vector containing ICDO3 behavior codes, it could be one of the
#'        values c("0", "1", "2", "3", "6").
#'
#' @return A vector containing ICD10 code.
#' @export
#'
#' @examples
#'
#' topo <- c("C001", "C15.1", "C349", "509", "C421", "C774")
#' morp <- c("8140", "8000", "8070", "8070", "8000", "8070")
#' beha <- rep("3", 6)
icdo3to10 <- function(topo, morp, beha) {
  if (any(lengths(list(morp, beha)) != length(topo)) ||
    length(morp) != length(beha)) {
    stop("Input vectors topo, morp, and beha must have the same length.")
  }

  # initiate the result vector
  icd10_code <- vector("character", length(topo))

  melanoma <- c(
    "8720", "8721", "8722", "8723", "8730",
    "8740", "8743", "8744", "8745", "8746",
    "8760", "8761", "8770", "8771", "8772",
    "8726", "8728", "8741", "8742", "8774", "8727"
  )
  mesothelioma <- c("9050", "9051", "9052", "9053", "9055")


  # 循环遍历输入向量的每个元素
  for (i in seq_along(topo)) {
    topo_val <- gsub("\\.", "", topo[i], perl = TRUE)
    morp_val <- gsub("M", "", morp[i])
    beha_val <- as.character(beha[i])

    # 检查topo,morp和beha变量的合法性
    topo_logi <- topo_val %in% topo_dict
    morp_logi <- morp_val %in% morp_dict
    beha_logi <- beha[i] %in% c("0", "1", "2", "3", "6")
    mess <- c("Topo", "Histology", "Behaviour")
    if (!topo_logi || !morp_logi || !beha_logi) {
      p1 <- paste0(mess[c(!topo_logi, !morp_logi, !beha_logi)], collapse = ",")
      # message(paste0(p1," values illegal."))
      icd10_code[i] <- NA
      next
    }


    # 根据topo、morp和beha获取直接映射的ICD10编码

    combined_mapping <- NA
    combined_mapping <- all_to_icd10$icd10[all_to_icd10$topo == topo_val &
      all_to_icd10$morp == morp_val &
      all_to_icd10$beha == beha[i]]
    if (length(combined_mapping) > 0 && !is.na(combined_mapping)) {
      icd10_code[i] <- combined_mapping
      next
    }

    # 根据morp和beha获取直接映射的ICD10编码
    blood_mapping <- NA
    if (morp_val %in% unique(rownames(morp_to_icd10)) &
      beha[i] %in% unique(colnames(morp_to_icd10))) {
      blood_mapping <- morp_to_icd10[morp_val, as.character(beha[i])]
    }

    if (!is.na(blood_mapping) & !(blood_mapping %in% c("*"))) {
      icd10_code[i] <- blood_mapping[1]
      next
    }

    # for Melanoma, Mesothelioma, and Kaposi sarnoma.
    if ((morp_val %in% melanoma) & (beha[i] %in% c("2", "3"))) {
      beha_val <- paste0("mela_", beha[i])
    } else if (morp_val %in% mesothelioma & beha[i] == "3") {
      beha_val <- paste0("meso_", beha[i])
    } else if (morp_val == "9140" & beha[i] == "3") {
      beha_val <- paste0("kaposi_", beha[i])
    } else {
      beha_val <- beha[i]
    }

    topo_mapping <- NA
    # 根据topo和beha获取直接映射的ICD10编码
    topo_mapping <- topo_to_icd10[topo_val, beha_val]
    if (!is.na(topo_mapping) & !(topo_mapping %in% c("*", "#"))) {
      icd10_code[i] <- topo_mapping
      next
    }

    # 没有找到匹配的ICD10编码
    icd10_code[i] <- NA
  }

  return(icd10_code)
}
