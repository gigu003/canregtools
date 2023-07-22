iarc_check <- function(data,
                       id = registr,
                       birthda = birthda,
                       inciden = inciden,
                       age = age,
                       sex = sex,
                       topo = topo,
                       morp = morp,
                       beha = beha,
                       grad = grad
                       ){
  ##单个变量的合法性检查
  data$id <- paste0("CID:", data$id)
  sex_illegal <- data$id[!(data$sex %in% c("1", "2"))]
  topo_illegal <- data$id[!(data$topo %in% topo_dict)]
  morp_illegal <- data$id[!(data$morp %in% morp_dict)]
  beha_illegal <- data$id[!(data$beha %in% c("1", "2", "3", "6"))]
  ids <- unique(c(sex_illegal, topo_illegal, morp_illegal, beha_illegal))
  single_item <- data %>%
    select(id, sex, topo, morp, beha) %>%
    filter(id %in% ids) %>%
    mutate(message = paste(
      ifelse(id %in% sex_illegal, "Sex", NA_character_),
      ifelse(id %in% topo_illegal, "Topo", NA_character_),
      ifelse(id %in% morp_illegal, "Morp", NA_character_),
      ifelse(id %in% beha_illegal, "Beha", NA_character_),
      collapse = ",")) %>%
    mutate(message = paste(message, "values illegal."))
  
  ##检查年龄是否正确
  age_cal <- round(as.numeric(difftime(as.Date(inciden),
                                 as.Date(birthda),
                                 units = "days")) / 365.25)
  age_illegal <- data$id[!data$age == age_cal]
  age <- data[id %in% age_illegal, c("id","birthda","inciden","age")]
  age$message <- "Age confict recalculated age."
  birth_illegal <- data$id[(data$inciden - data$birthda) < 0]
  ##重置年龄
  data$age <- age_cal
  
  ##age-site-histology
  
  ### childhood cancer age-site-histology
  hodgkin <- as.character(c(9650:9655,9659,9661:9665,9667))
  renal <- as.character(c(8010:8041, 8050:8075, 8082, 8120:8122, 8130:8141,
             8143, 8155, 8190:8201, 8210, 8211, 8221:8231, 8240,
             8241, 8244:8246, 8260:8263, 8290, 8310, 8320, 8323,
             8325, 8401, 8430, 8440, 8480:8490, 8504, 8510, 8550,
             8560:8562, 8570:8573, 9013))
  hepa <- as.character(c(8010:8041, 8050:8075, 8082, 8120:8122, 8140, 8141,
                         8143, 8148, 8155, 8158, 8190:8201, 8202, 8210, 8211,
                         8230, 8231, 8240, 8241, 8244:8246, 8260:8264, 8310,
                         8320, 8323, 8401, 8430, 8440, 8470, 8480:8490, 8503,
                         8504, 8510, 8550, 8560:8562, 8570:8573, 9013))
  id1 <- data$id[data$morp %in% hodgkin & age <= 2]
  id2 <- data$id[data$morp %in% c("9490","9500") & age>=10 & age <= 14]
  id3 <- data$id[data$morp %in% as.character(c(9510:9514)) & age>=6 & age <= 14]
  id4 <- data$id[data$morp %in% renal & data$topo =="C649" & data$age <= 8]
  id4_1 <- data$id[data$morp %in% as.character(c(8311, 8312, 8316:8319, 8361)) &
                data$age <= 8]
  id4 <- unique(id4, id4_1)
  id5 <- data$id[data$morp %in% c("8970", "8975") &
                   data$age >=6 & data$age <= 14]
  id6 <- data$id[data$morp %in% hepa & data$topo %in% c("C220", "C221") &
                   data$age <= 8]
  id7 <- data$id[data$morp %in% as.character(c(9180:9187, 9191:9195, 9200)) &
                  data$topo %in% paste0("C",c(400:419,760:768,809)) ]
  
  bones <- paste0("C",c(400:419,760:768,809))
  id8 <- data$id[data$morp %in%
                   as.character(c(9211:9213, 9221, 9222, 9230, 9241:9243)) &
                   data$age <= 5]
  id8_1 <- data$id[data$morp %in% as.character(c(9210, 9220, 9240)) &
                     data$topo %in% bones & data$age <= 5]
  id8_2 <- data$id[data$morp == "9231" & data$topo %in% paste0("C",c(400:419))]
  
  id8 <- unique(c(id8, id8_1, id8_2))
  
  id9 <- data$id[data$morp %in% c("9260") & data$topo %in% bones & data$age <= 3]
  
  ### adults cancer age-site-histology
  a1 <- data$id[data$age < 40 & data$age >= 15 &
                  substr(data$topo, 1, 3) == "C61" &
                  substr(data$morp, 1, 3) == "814"]
  a2 <- data$id[data$age < 20 & data$age >= 15 &
                  substr(data$topo, 1, 3) %in%
                  paste0("C",c(15, 19:21, 23:24, 50, 53:55))]
  a3 <- data$id[data$age < 20 & data$age >= 15 &
                  substr(data$topo, 1, 3) == "C17" &
                  data$morp == "9590"]
  a4 <- data$id[data$age < 20 & data$age >= 15 &
                  substr(data$topo, 1, 3) %in% c("C18", "C33", "C34") &
                  data$morp == "9100"]
  a5 <- data$id[data$age > 45 & data$age >= 15 &
                  substr(data$topo, 1, 3) == c("C58") &
                  !substr(data$morp, 1, 3) == "824"]
  a6 <- data$id[data$age >= 15 & data$age <= 25 &
                  data$morp %in% c("9732", "9823")]
  a7 <- data$id[data$age >= 15 &
                  (data$morp %in% c("8910", "8960", "8970", "8981",
                                   "8991", "9072", "9470", "9687") |
                     substr(data$morp, 1, 3) == "951")]
  

  ## Sex/histology:Unlikely
  gorup23 <- "8905"
  group24 <- paste0(c(8930, 8931))
  group25 <- paste0(c(8313, 8441:8444, 8451, 8460, 8462, 8463, 8470:8473,
                      8593, 8600:8602, 8610, 8620:8623, 8632, 8641, 8660,
                      8670, 9000, 9013:9015, 9090, 9091))
  group26 <- paste0(c(9103, 9104))
  group27 <- paste0(c(8380:8384, 8482, 8934, 8950:8951))
  sex1 <- data$id[data$sex == 1 &
                    data$morp %in% c(group23, group24, group25, group26,
                                     group27)]
  sex2 <- data$id[data$sex ==2 &
                    data$morp %in% c("8080", "9061", "9062", "9063", "9102")]
  
  ## Behaviour/site:Unlikely
  beha_site <- data$id[data$beha == "2" &
                         substr(data$topo, 1, 3) %in%
                         paste0("C",c(40:42,47,49,70:72))]
  
  ##Grade/histology:Errors
  g2 <- data$id[data$grade >= 5 & data$grade <= 8 &
                  as.numeric(data$morp) < 9590]
  g3 <- data$id[data$grade >= 1 & data$grade <= 4 &
                  as.numeric(data$morp) >= 9590]
  g4 <- data$id[!data$grade == 5 &
                  data$morp %in% as.character(c(0702, 9705:9706, 9708:9709,
                                                9717:9718, 9729, 9827, 9834,
                                                9837))]
  g5 <- data$id[!data$grade %in% c("5", "7") &
                  data$morp %in% c("9714", "9831")]
  g6 <- data$id[as.numeric(data$morp) >= 9700 & as.numeric(data$morp) <= 9719 &
                  !data$grade %in% c("5","8","9")]
  g7 <- data$id[((as.numeric(data$morp) >= 9670 & as.numeric(data$morp) <= 9699)|
                  data$morp %in% c("9728", "9823", "9826", "9833", "9836")) &
                  !data$grade == "6"]
  g8 <- data$id[data$morp == "9948" & !data$grade == "8"]
  g9 <- data$id[data$morp %in% c("8331", "9187", "9511") & !data$grade == "1"]
  g10 <- data$id[data$morp %in% c("8332", "8858", "9083", "9243", "9372") &
                   !data$grade == "2"]
  g11 <- data$id[data$morp %in% c("8631", "8634") &
                   !data$grade == "2"]
  g12 <- data$id[data$morp %in% c("8020", "8021", "8805", "9062", "9082",
                                  "9390", "9392", "9401", "9451", "9505",
                                  "9512") & !data$grade == "4"]
  
  ##Basis of diagnosis/histology
  
  
  # Errors
  ## sex/site check
  error_sex1 <- data$id[data$sex == 1 & substr(data$topo,1,3) %in% paste0("C",c(51:58))]
  error_sex2 <- data$id[data$sex == 2 & substr(data$topo,1,3) %in% paste0("C",c(60:63))]
  
  
  
  result <- list(single_item, age)
  
return(single_item)
}