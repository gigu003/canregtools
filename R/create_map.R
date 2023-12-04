#' Create map.
#'
#' @param x Areacodes.
#' @param value Values. 
#' @param breaks Cutoff values.
#' @param class Province or city
#' @param bound Show bound of background map.
#' @param bg_cols background colors.
#' @param highlight_cols highlight colors.
#' @param label Logical value.
#' @param label_size size of label
#' @param legend Logical value.
#' @param legend_title Title of legend.
#'
#' @return A map
#' @export
#'
#' @import ggplot2 memoise cachem
#' @importFrom sf st_read
#' 
#' 
create_map <- function(x,
                       value = c(rep(1, length(x))),
                       breaks = NULL,
                       class = "province",
                       bound = "city",
                       bg_cols = c("lightgray","white"),
                       highlight_cols = c("white","red"),
                       label = FALSE,
                       label_size = 2,
                       legend = FALSE,
                       legend_title = "Incidence"){
  adcode <- "adcode"
  name <- "name"
  # 判断 x 和 value 的元素个数是否一致
  stopifnot(length(x) == length(value))
  # 地图数据链接
  maplink <- "https://geo.datav.aliyun.com/areas_v3/bound/"
  # 获取 JSON 地址的函数
  json_addr <- function(x){
    ifelse(substr(x, 3, 4) == "90", paste0(maplink, x, ".json"),
           paste0(maplink, x, "_full.json"))
  }
  # 读取 JSON 地图数据的函数
  read_json <- memoise(function(x){
      geo_data <- st_read(json_addr(x))
    if (!("subFeatureIndex" %in% names(geo_data))) {
      geo_data$subFeatureIndex <- 0
    }
    return(geo_data)
  }, cache = cachem::cache_disk())
  
  if (class == "province"){
    province <- paste0(unique(substr(x, 1, 2)),"0000")
    prov_bound <- bind_rows(lapply(province, read_json))
    citys <- prov_bound$adcode
    if(bound == "city"){
      geo_data <- prov_bound
    } else {
      geo_data <- bind_rows(lapply(citys, read_json))  
    }                     
    
  } else if (class == "city"){
    citys <- paste0(unique(substr(x, 1, 4)), "00")
    geo_data <- bind_rows(lapply(citys, read_json))
    show_codes <- list(adcode = geo_data$adcode, name = geo_data$name)
    cat(paste0("Available codes are listed below:"))
  }
  
  if (bound == "city"){
    high <- geo_data %>% filter(adcode %in% paste0(substr(x, 1, 4), "00"))
    code_exist <- paste0(substr(x, 1, 4), "00") %in% geo_data$adcode
    value_exist <- value[code_exist]
  } else {
    high <- geo_data %>% filter(adcode %in% x)
    code_exist <- x %in% geo_data$adcode
    value_exist <- value[code_exist]
  }
  
  # 输出不存在于地图中的区域代码
  if (length(x[!code_exist]) > 0) {
    cat("areacode list below: ", x[!code_exist], " not existed in map.\n")
  }
  
    p <- ggplot() +
      geom_sf(data = geo_data, fill = bg_cols[1],  color = bg_cols[2]) +
      geom_sf(data = high, aes(fill = value_exist), color= "white")+
      scale_fill_gradient(low = highlight_cols[1],
                          high = highlight_cols[2],
                          breaks = breaks, labels = breaks,
                          na.value = NA)
    p <- p + theme_minimal() + labs(x="", y="")
    if (legend) {
      p <- p +
        labs(fill = legend_title) +  # Change the legend title
        theme(legend.position = "top")  # Change the legend position if needed
    } else {
      p <- p + theme(legend.position = "none")
    }
    
    # Continue with other customization
    if (label) {
      p <- p + geom_sf_text(data = high, aes(label = name), size = label_size)
    }
    return(p)
}