#' Create map.
#'
#' @param x Areacodes.
#' @param value Values. 
#' @param breaks Cutoff values.
#' @param class Province or city
#' @param type ggplot2 or leaflet
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
#' @import ggplot2
#' @import leaflet
#' @importFrom sf st_read
#' 
create_map <- function(x,
                       value = NULL,
                       breaks = NULL,
                       class = "province",
                       type = "ggplot2",
                       bg_cols = c("lightgray","white"),
                       highlight_cols = c("white","red"),
                       label = TRUE,
                       label_size = 2,
                       legend = TRUE,
                       legend_title = "Incidence"){
  adcode <- "adcode"
  name <- "name"
  # 判断 x 和 value 的元素个数是否一致
  stopifnot(length(x) == length(value))
  # 地图数据链接
  maplink <- "https://geo.datav.aliyun.com/areas_v3/bound/"
  # 获取 JSON 地址的函数
  json_addr <- function(x){
    ifelse(x == "419001", paste0(maplink, x, ".json"),
           paste0(maplink, x, "_full.json"))
  }
  # 读取 JSON 地图数据的函数
  read_json <- function(x){
    sink("temp_log.txt", append = TRUE)
      geo_data <- st_read(json_addr(x))
    sink()
    if (!("subFeatureIndex" %in% names(geo_data))) {
      geo_data$subFeatureIndex <- 0
    }
    return(geo_data)
  }
  
  if (class == "province"){
    province <- unique(substr(x, 1, 2))
      prov_bound <- st_read(paste0(maplink, province, "0000_full.json"))
    citys <- prov_bound$adcode
    geo_data <- do.call(rbind, lapply(citys, read_json))
  } else if (class == "city"){
    city <- unique(substr(x, 1, 4))
    geo_data <- st_read(paste0(maplink, city, "00_full.json"))
    show_codes <- list(adcode = geo_data$adcode, name = geo_data$name)
    cat(paste0("Available codes are listed below:"))
  }
  
  high <- geo_data %>% filter(adcode %in% x)
  code_exist <- x %in% geo_data$adcode
  value_exist <- value[code_exist]
  
  # 输出不存在于地图中的区域代码
  if (length(x[!code_exist]) > 0) {
    cat("areacode list below: ", x[!code_exist], " not existed in map.\n")
  }
  
  if (type == "leaflet") {
    leaflet() %>%
      addPolygons(data = geo_data, fillColor = "gray", fillOpacity = 0.5, 
                  color = "lightgray", weight = 1) %>%
      addPolygons(data = high, fillColor = "red", fillOpacity = 0.5, 
                  color = "white", weight = 1, label = high$name)
  } else if (type == "ggplot2") {
  
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
}