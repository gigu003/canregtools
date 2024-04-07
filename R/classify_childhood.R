#' Classify the Childhood cancer into ICCC3
#' 
#' @details
#' This function classify the 'topo' and 'morp' parts of ICDO3 codes into
#' ICCC3 (International Classification of Childhood Cancer, Third edition).
#' 
#' @param topo Topography parts of ICDO3 codes in the format of 'C15.6' or
#'        'C156'. 
#' @param morp Morphology parts of ICDO3 codes in the format of '8000' or
#'        'M8140'.
#' @param type Type of Classification for the output, options are 'main' or
#'        'sub', default is 'main'.
#' @param lang Language of the output, options are 'cn' or 'en',
#'        default is 'cn'.
#'        
#' @import dplyr
#' @import Rdpack
#'
#' @return Factor of ICCC3 classification for childhood cancer.
#' @export
#'
#' @examples
#' library(canregtools)
#' file <- system.file("extdata", "411721.xls", package = "canregtools")
#' data <- read_canreg(file)
#' fbcases <- data$FBcases
#' fbcases <- fbcases[fbcases$age <= 14,]
#' classify_childhood(fbcases$topo, fbcases$morp, lang = "cn")
#' classify_childhood(fbcases$topo, fbcases$morp, lang = "en")
#' classify_childhood(fbcases$topo, fbcases$morp, type = "sub", lang = "cn")
#' classify_childhood(fbcases$topo, fbcases$morp, type = "sub", lang = "en")
#' 
classify_childhood <- function(topo, morp, type = "main", lang = "cn") {
  morp <- as.numeric(gsub("[M]", "", toupper(morp)))
  topo <- as.numeric(gsub("[.C]", "", toupper(topo)))
  cate <- NA
  for(i in seq_along(morp)){
    t <- topo[i]
    m <- morp[i]
    c8 <- between(t, 400, 419) | between(t, 760, 768) | t == 809
    if (m %in% c(9820, 9823, 9826, 9827, 9831,
                 9832, 9833, 9834, 9835, 9836,
                 9837, 9940, 9948)) {
      cate[i] <- 11
    } else if (m %in% c(9840, 9861, 9866, 9867, 9870,
                        9871, 9872, 9873, 9874, 9891,
                        9895, 9896, 9897, 9910, 9920,
                        9931)){
      cate[i] <- 12
    } else if (m %in% c(9863, 9875, 9876, 9950, 9960,
                        9961, 9962, 9963, 9964)) {
      cate[i] <- 13
    } else if (m %in% c(9945, 9946, 9975, 9980, 9982,
                        9983, 9984, 9985, 9986, 9987,
                        9989)) {
      cate[i] <- 14
    } else if (m %in% c(9800, 9801, 9805, 9860, 9930)) {
      cate[i] <- 15
    } else if (m %in% c(9650, 9651, 9652, 9653, 9654,
                        9655, 9659, 9661, 9662, 9663,
                        9664, 9665, 9667)) {
      cate[i] <- 21
    } else if (m %in% c(9591, 9670, 9671, 9673, 9675,
                        9678, 9679, 9680, 9684, 9689,
                        9690, 9691, 9695, 9698, 9699,
                        9700, 9701, 9702, 9705, 9708,
                        9709, 9714, 9716, 9717, 9718,
                        9719, 9727, 9728, 9729, 9731,
                        9732, 9733, 9734, 9760, 9761,
                        9762, 9764, 9765, 9766, 9767,
                        9768, 9769, 9970)) {
      cate[i] <- 22
    } else if (m == 9687) {
      cate[i] <- 23
    } else if (m %in% c(9740, 9741, 9742, 9750, 9754,
                        9755, 9756, 9757, 9758)) {
      cate[i] <- 24
    } else if (m %in% c(9590, 9596)) {
      cate[i] <- 25
    } else if (m %in% c(9383, seq(9390, 9394, 1))) {
      cate[i] <- 31
    } else if ((m == 9380 && t == 723)|
               m %in% c(9384, seq(9400, 9411, 1), 9420,
                        seq(9421, 9424, 1), seq(9440, 9442, 1))) {
      cate[i] <- 32
    } else if ((m %in% c(seq(9470, 9474, 1), 9480, 9508))|
               (m %in% seq(9501, 9504, 1) &
                between(t, 700, 729))){
      cate[i] <- 33
    } else if (m %in% c(9381, 9382, 9430, 9444, 9450,
                        9451, 9460) |
               (m == 9380 & (between(t, 700, 722)|
                             between(t, 724, 729)|
                             t %in% c(751, 753)))) {
      cate[i] <- 34
    } else if (m %in% c(seq(8270, 8281, 1), 8300,
                        seq(9350, 9352, 1), seq(9360, 9362, 1),
                        9412, 9413, 9492, 9493, seq(9505, 9507, 1),
                        seq(9530, 9539, 1), 9582)) {
      cate[i] <- 35
    } else if (m %in% seq(8000, 8005, 1) &
               (between(t, 700, 729) | between(t, 751, 753))) {
      cate[i] <- 36
    } else if (m %in% c(9490, 9500)) {
      cate[i] <- 41
    } else if (m %in% c(seq(8680, 8683, 1), seq(8690, 8693, 1), 8700,
                        seq(9520, 9523, 1)) |
               (m %in% c(seq(9501, 9504, 1) & 
                         (t<=699 | between(t, 739, 768) |
                          t == 809)))) {
      cate[i] <- 42
    } else if (m %in% seq(9510, 9514, 1)) {
      cate[i] <- 50
    } else if (m %in% c(8959, 8560, seq(8964, 8967, 1)) |
               (m %in% c(8963, 8964) & t == 649)) {
      cate[i] <- 61
    } else if (m %in% c(8311, 8312, seq(8316, 8319, 1), 8361) |
               (m %in% c(seq(8010, 8041, 1), seq(8050, 8075, 1),
                         8082, seq(8120, 8122, 1), seq(8130, 8141, 1),
                         8143, 8155, seq(8190, 8201, 1), 8210, 8211,
                         seq(8221, 8231, 1), 8240, 8241,
                         seq(8244, 8246, 1), seq(8260, 8263, 1), 8290,
                         8310, 8320, 8323, 8401, 8430, 8440,
                         seq(8480, 8490, 1), 8504, 8510, 8550,
                         seq(8560, 8576, 1)) & t == 649)) {
      cate[i] <- 62
    } else if (m %in% seq(8000, 8005, 1) & t == 649) {
      cate[i] <- 63
    } else if (m == 8970) {
      cate[i] <- 71
    } else if (m %in% seq(8160, 8180, 1) | 
               (t %in% c(220, 221) &
                m %in% c(seq(8010, 8041, 1), seq(8050, 8075, 1), 8082,
                seq(8120, 8122, 1), 8140, 8141, 8143, 8155,
                seq(8190, 8201, 1), 8210, 8211, 8230, 8231,
                8240, 8241, seq(8244, 8246, 1),
                seq(8260, 8264, 1), 8310, 8320, 8323, 8401,
                8430, 8440, seq(8480, 8490, 1), 8504, 8510,
                8550, seq(8560, 8576, 1)))) {
      cate[i] <- 72
    } else if (m %in% c(seq(8000, 8005, 1)) &
               t %in% c(220, 221)) {
      cate[i] <- 73
    } else if (m %in% c(seq(9180, 9187, 1),
                        seq(9191, 9195, 1), 9200) & c8) {
      cate[i] <- 81
    } else if (m %in% c(9221, 9230, seq(9241, 9243, 1)) |
               (m %in% c(9210, 9220, 9240) & c8)){
      cate[i] <- 82
    } else if ((m == 9260 & c8) | (m %in% seq(9363, 9365, 1) & 
                                   between(t, 400, 419))) {
      cate[i] <- 83
    } else if (m %in% c(8812, 9250, 9261, 9262, seq(9270, 9275, 1),
                        seq(9280, 9282, 1), 9290, seq(9300, 9302, 1),
                        seq(9310, 9312, 1), seq(9320, 9322, 1), 9330,
                        seq(9340, 9342, 1), seq(9370, 9372, 1)) |
               (m %in% c(8810, 8811, 8823, 8830) & between(t, 400, 419))) {
      cate[i] <- 84
    } else if (m %in% c(seq(8000, 8005, 1), 8800, 8801, seq(8803, 8805, 1)) &
               between(t, 400, 419)) {
      cate[i] <- 85
    } else if (m %in% c(seq(8900, 8905, 1), 8910, 8912, 8920, 8991)) {
      cate[i] <- 91
    } else if (m %in% c(8820, 8822, seq(8824, 8827, 1), 9150, 9160,
                        9491, seq(9540, 9571, 1), 9580) |
               m %in% c(8810, 8811, seq(8813, 8815, 1), 8821, 8823,
                        8834, 8835) &
               (between(t, 0, 399)| between(t, 440, 768)|t==809)) {
      cate[i] <- 92
    } else if (m == 9140) {
      cate[i] <- 93
    } else if (m %in% c(8587, seq(8710, 8713, 1), 8806, seq(8831, 8833, 1),
                        8836, seq(8840, 8842, 1), seq(8850, 8858, 1),
                        seq(8860, 8862, 1), 8870, 8880, 8881,
                        seq(8890, 8898, 1), 8921, 8982, 8990,
                        seq(9040, 9044, 1), seq(9120, 9125, 1),
                        seq(9130, 9133, 1), 9135, 9136, 9141, 9142, 9161,
                        seq(9170, 9175, 1), 9231, 9251, 9252, 9373, 9581) |
               (m == 8830 & (between(t, 0, 399)|between(t, 440, 768)|t==809))|
               (m == 8963 & (between(t, 0, 639)|between(t, 659, 699)|
                             between(t, 739, 768)|t == 809)) |
               (m %in% c(9180, 9210, 9220, 9240) & between(t, 490, 499)) |
               (m == 9260 & (between(t, 0, 399)|between(t, 470, 759))) |
               (m == 9364 & (between(t, 0, 399)|between(t, 470, 639)|
                             between(t, 659, 699)|between(t, 739, 768)|
                             t == 809)) |
               (m == 9365 & (between(t, 0, 399)|between(t, 470, 639)|
                             between(t, 659, 768)|t == 809)) ) {
      cate[i] <- 94
    } else if (m %in% seq(8800, 8805, 1) &
               (between(t, 0, 399)| between(t, 440, 768)|t == 809)) {
      cate[i] <- 95
    } else if (m %in% c(seq(9060, 9065, 1), seq(9070, 9072, 1),
                        seq(9080, 9085, 1), 9100, 9101) &
               (between(t, 700, 729) | between(t, 751, 753))) {
      cate[i] <- 101
    } else if (m %in% c(seq(9060, 9065, 1), seq(9070, 9072, 1),
                        seq(9080, 9085, 1), seq(9100, 9105, 1)) &
               (between(t, 0, 559)|between(t, 570, 619)|
                between(t, 630, 699)|between(t, 739, 750)|
                between(t, 754, 768)|t == 809)) {
      cate[i] <- 102
    } else if (m %in% c(seq(9060, 9065, 1), seq(9070, 9073, 1),
                        seq(9080, 9085, 1), 9090, 9091, 9100, 9101) &
               (t == 569 | between(t, 620, 629))) {
      cate[i] <- 103
    } else if (m %in% c(seq(8441, 8444, 1), 8450, 8451, seq(8460, 8473, 1))|
               (m %in% c(seq(8010, 8041, 1), seq(8050, 8075, 1), 8082,
                         seq(8120, 8122, 1), seq(8130, 8141, 1), 8141,
                         seq(8190, 8201, 1), 8210, 8211, seq(8221, 8241, 1),
                         seq(8244, 8246, 1), seq(8260, 8263, 1), 8290, 8310,
                         8313, seq(8380, 8384, 1), 8430, 8440,
                         seq(8480, 8490, 1), 8504, 8510, 8550,
                         seq(8560, 8573, 1), 9000, 9014, 9015) &
                (t == 569 | between(620, 629, 1)))) {
      cat[i] <- 104
    } else if (m %in% seq(8590, 8671, 1) |
               (m %in% seq(8000, 8005, 1) &(t == 569 | between(t, 620, 629)))
    ) {
      cate[i] <- 105
    } else if (m %in% seq(8370, 8375, 1)) {
      cate[i] <- 111
    } else if (m %in% c(seq(8330, 8337, 1), seq(8340, 8347, 1), 8350) |
               (m %in% c(seq(8010, 8041, 1), seq(8050, 8075, 1), 8082,
                         seq(8120, 8122, 1), seq(8130, 8141, 1), 8190,
                         8200, 8201, 8211, 8230, 8231, seq(8244, 8246, 1),
                         seq(8260, 8263, 1), 8290, 8310, 8320, 8323, 8430,
                         8440, 8480, 8481, 8510, seq(8560, 8573, 1) &
                         t == 739))) {
      cate[i] <- 112
    } else if (m %in% c(seq(8010, 8041, 1), seq(8050, 8075, 1), 8082, 8083,
                        seq(8120, 8122, 1), seq(8130, 8141, 1), 8190, 8200,
                        8201, 8211, 8230, 8231, seq(8244, 8246, 1),
                        seq(8260, 8263, 1), 8290, 8310, 8320, 8323, 8430,
                        8440, 8480, 8481, seq(8500, 8576, 1)) &
               between(t, 110, 119)) {
      cate[i] <- 113
    } else if (m %in% c(seq(8720, 8780, 1), 8790)) {
      cate[i] <- 114
    } else if (m %in% c(seq(8010, 8041, 1), seq(8050, 8075, 1), 8078, 8082,
                        seq(8090, 8110, 1), 8140, 8143, 8147, 8190, 8200,
                        8240, 8246, 8247, 8260, 8310, 8320, 8323,
                        seq(8390, 8420, 1), 8430, 8480, 8542, 8560,
                        seq(8570, 8573, 1), 8940, 8941) &
               between(t, 440, 449)) {
      cate[i] <- 115
    } else if (m %in% c(seq(8010, 8084, 1), seq(8120, 8157, 1),
                        seq(8190, 8264, 1), 8290, 8310, seq(8313, 8315, 1),
                        seq(8320, 8325, 1), 8360, seq(8380, 8384, 1),
                        seq(8430, 8440, 1), seq(8452, 8454, 1),
                        seq(8480, 8586, 1), seq(8588, 8589, 1), 8940, 8941,
                        8983, 9000, seq(9010, 9016, 1), 9020, 9030) &
               (between(t, 0, 109) | between(t, 129, 218) |
                between(t, 239, 399) |between(t, 480, 488) |
                between(t, 500, 559) |between(t, 570, 619)|
                between(t, 630, 639) |between(t, 659,729)|
                between(t, 750, 768)| t == 809) ) {
      cate[i] <- 116
    } else if (m %in% c(seq(8930, 8936, 1), 8950, 8951, seq(8971, 8981, 1),
                        seq(9050, 9055, 1), 9110) |
               (m == 9363 & (between(t, 0, 399)|between(470, 759, 1)))) {
      cate[i] <- 121
    } else if (m %in% seq(8000, 8005, 1) &
               (between(t, 0, 218) | between(t, 239, 399) |
                between(t, 420, 559) | between(t, 570, 619) |
                between(t, 630, 639) | between(t, 659, 699) |
                between(t, 739, 750) | between(t, 754, 809))) {
      cate[i] <- 122
    } else {
      cate[i] <- 999
    }
  }
  
  if (type == "main"){
    cate <- floor(cate/10)
    if (lang == "cn") {
      cate <- factor(cate, levels = c(seq(1, 12, 1), 99),
                     labels = label_child[[1]]$cn)
    } else {
      cate <- factor(cate, levels = c(seq(1, 12, 1), 99),
                     labels = label_child[[1]]$en)
    }
    return(cate)
  } else if ( type == "sub"){
    if (lang == "cn") {
      cate <- factor(cate,
                     levels = c(seq(11, 15, 1),seq(21, 25, 1),
                                seq(31, 36, 1), 41, 42, 50,
                                seq(61, 63, 1), seq(71, 73, 1),
                                seq(81, 85, 1), seq(91, 95, 1),
                                seq(101, 105, 1), seq(111, 116, 1),
                                121, 122, 999),
                     labels = label_child[[2]]$cn)
    } else {
      cate <- factor(cate,
                     levels = c(seq(11, 15, 1),seq(21, 25, 1),
                                seq(31, 36, 1), 41, 42, 50,
                                seq(61, 63, 1), seq(71, 73, 1),
                                seq(81, 85, 1), seq(91, 95, 1),
                                seq(101, 105, 1), seq(111, 116, 1),
                                121, 122, 999),
                     labels = label_child[[2]]$en)
    }
    
    return(cate)
  } else {
    stop("type only supported 'main' or 'sub'")
  }
  
}
