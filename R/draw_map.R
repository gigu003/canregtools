#' Create map.
#'
#' @param x Areacodes.
#' @param values values. 
#' @param breaks Cutoff values.
#' @param bg_level Province or city
#' @param bg_bound Show bound of background map.
#' @param bg_cols background colors.
#' @param h_cols highlight colors.
#' @param label Logical value.
#' @param label_size size of label
#' @param legend Logical value.
#' @param legend_title Title of legend.
#' @param h_type map type.
#'
#' @return A map
#' @export
#'
#' @import ggplot2
#' @importFrom memoise memoise forget
#' @importFrom cachem cache_disk
#' @importFrom sf st_read
#' 
#' 
draw_map <- function(x,
                       values = NULL,
                       breaks = NULL,
                       bg_level = "province",
                       bg_bound = "county",
                       bg_cols = c("lightgray","white"),
                       h_type = "",
                       h_cols = c("white","darkred"),
                       label = FALSE,
                       label_size = 2,
                       legend = FALSE,
                       legend_title = "Incidence"){
  adcode <- "adcode"
  name <- "name"
  map <- read_map(x, level = bg_level, bound = bg_bound)
  
  code_exist <- x %in% map$adcode
  
  # Output the not exist areacodes to the screen.
  if (length(x[!code_exist]) > 0) {
    cat("Areacode: ",
        paste(x[!code_exist], collpse = ","),
        "were not existed in the map.\n")
    cat("Available areacodes could be as follows:\n",
        paste(map$adcode, collapse = ","), "\n")
  }
  
  
  high <- map %>% filter(adcode %in% x)
  
  if (length(bg_cols)<2) {bg_cols <- c("white", bg_cols)}
  if (length(h_cols)<2) {h_cols <- c("white", h_cols)}
  
  p <- ggplot() +
    geom_sf(data = map, fill = bg_cols[1],  color = bg_cols[2])
  
  if(is.null(values)){
      p <- p + geom_sf(data = high,
                       fill = h_cols[2],
                       color= "white",
                       show.legend = FALSE)
  } else {
    # 判断 x 和 value 的元素个数是否一致
    stopifnot(length(x) == length(values))
    values_exist <- values[code_exist]
    p <- p + geom_sf(data = high,
                     aes(fill = values_exist),
                     color= "white",
                     show.legend = TRUE)+
      scale_fill_gradient(low = h_cols[1], high = h_cols[2],
                          breaks = breaks, labels = breaks,
                          na.value = NA)
  }
  
  p <- p + theme_minimal() + labs(x="", y="")
  
  if (legend){
    p <- p + labs(fill = legend_title)+
      theme(legend.position = "top")
    } else {
    p <- p + theme(legend.position = "none")
  }
  
  # Continue with other customization
  if (label) {
    p <- p + geom_sf_text(data = high, aes(label = name), size = label_size)
  }
  
  return(p)
}


#' Get map of china from dataV.
#'
#' @param x Area codes in China, such as "410102".
#' @param level Top level of the returned map.
#' @param bound Boundary of the returned map.
#' @param cache_dir Cache directory of the returned map. 
#' @param cache_refresh Logical value, TRUE or FALSE rerfresh the stored cached
#'                      map or not.
#'
#' @return Map data of sf format.
#' @export
#'
#' @examples
#' henan <- read_map("410102", level = "province", bound = "county")
#' class(henan)
read_map <- function(x,
                     level = "province",
                     bound = "county",
                     cache_dir = "~/.cache_map",
                     cache_refresh = FALSE){
  adcode <- "adcode"
  childrenNum <- "childrenNum"
  x_exist <- substr(x, 1, 2) %in% as.character(c(11:15, 21:23, 31:37, 41:46,
                                                 50:54, 61:65, 71, 81, 82, 10))
  
  if (all(!x_exist)){
    stop(paste("stops because of illegal inputs x: ",
               paste(x, collapse = ",")))
  } else if (any(!x_exist)){
    x <- x[x_exist]
    cat(paste("x: ", paste(x[!x_exist], collapse = ","),
              "excluded due to wrong province code."))
  } else{
    #cat("all input x passed.")
  }
  # map address
  maplink <- "https://geo.datav.aliyun.com/areas_v3/bound/"
  # full address of json style map data.
  json_addr <- function(x){
    ifelse(substr(x, 3, 4) == "90", paste0(maplink, x, ".json"),
           paste0(maplink, x, "_full.json"))
  }
  
  
  # function read JSON map data.
  read_json <- memoise(function(x){
    pb$tick()
    geo_data <- st_read(json_addr(x), as_tibble = TRUE, quiet = TRUE)
    if (!("subFeatureIndex" %in% names(geo_data))) {
      geo_data$subFeatureIndex <- 0
    }
    return(geo_data)
  }, cache = cachem::cache_disk(dir = cache_dir))
  
  if (cache_refresh) { forget(read_json) }
  
  if (tolower(level) == "china"){
    if(tolower(bound) == "province"){
      pb <- progress_bar$new(total = 1,
                             format = "Map loading [:bar] :percent time left: :eta")
      geo_data <- read_json("100000")
    } else if(tolower(bound) =="city"){
      geo_data1 <- st_read(json_addr("100000"), as_tibble = TRUE, quiet = TRUE)
      province_code <- geo_data1$adcode
      province_code <- province_code[nchar(province_code)==6&!province_code=="710000"]
      pb <- progress_bar$new(total = length(province_code),
                             format = "Map loading [:bar] :percent time left: :eta")
      geo_data <- bind_rows(lapply(province_code, read_json)) %>%
        mutate(adcode = as.character(adcode))
      geo_data <- bind_rows(geo_data, geo_data1) %>%
        arrange(adcode)
    } else if (tolower(bound) == "county"){
      pb <- progress_bar$new(total = 366,
                             format = "Map loading [:bar] :percent time left: :eta")
      geo_data1 <- st_read(json_addr("100000"), as_tibble = TRUE, quiet = TRUE)
      province_code <- geo_data1$adcode
      province_code <- province_code[nchar(province_code)==6&!province_code=="710000"]
      geo_data <- bind_rows(lapply(province_code, read_json)) %>%
        mutate(adcode = as.character(adcode))
      a1 <- geo_data %>% filter(childrenNum == 0)
      a2 <- geo_data %>% filter(childrenNum > 0)
      geo_data2 <- bind_rows(lapply(a2$adcode, read_json)) %>%
        mutate(adcode = as.character(adcode))
      geo_data <- bind_rows(geo_data1, geo_data, geo_data2, a1) %>%
        arrange(adcode)
    } else {
      stop(paste("bound value: ", bound, " was not supported under level:", level))
    }
  } else if (tolower(level) == "province"){
    province <- paste0(unique(substr(x, 1, 2)), "0000")
    pb <- progress_bar$new(total = length(province),
                           format = "Map loading [:bar] :percent time left: :eta")
    prov_bound <- bind_rows(lapply(province, read_json))
    citys <- prov_bound$adcode[!prov_bound$childrenNum==0]
    if(tolower(bound) == "city"){
      geo_data <- prov_bound
    } else if (tolower(bound) == "county") {
      pb <- progress_bar$new(total = length(citys),
                             format = "Map loading [:bar] :percent time left: :eta")
      geo_data <- bind_rows(prov_bound, lapply(citys, read_json))%>%
        arrange(adcode)
    } else {
      stop(paste("bound value: ", bound, " was not supported under level:", level))
    }
  } else if (tolower(level) == "city"){
    if (tolower(bound) == "county"){
      citys <- paste0(unique(substr(x, 1, 4)), "00")
      pb <- progress_bar$new(total = length(citys),
                             format = "Map loading [:bar] :percent time left: :eta")
      geo_data <- bind_rows(lapply(citys, read_json))
    } else{
      stop(paste("bound value: ", bound, " was not supported under level:", level))
    }
  } else {
    stop(paste("level value: ", level, " was not supported."))
  }
  pb$terminate()
  return(geo_data)
}
