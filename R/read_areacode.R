#' Get area codes from National Bureau of Statistics in China.
#'
#' @param prov Province code.
#' @param year Year
#' @param level Level of returned data, options are 'city', 'county', 'xiang',
#'        'cun', default is 'county'.
#' @param cache_dir the directory of cache areacodes.
#' @param cache_refresh logical value that refresh cache or not.
#'        
#' @importFrom rvest read_html
#' @importFrom memoise memoise
#' @importFrom cachem cache_disk
#' @return A data frame contain area codes and their labels.
#' @export
#'
read_areacode <- function(prov = "41",
                          year = "2023",
                          level = "county",
                          cache_dir = "~/.cache_areacodes",
                          cache_refresh = F){
  areacode <- "areacode"
  if (length(prov)==1&prov[1] == "all") prov <- c(11:15, 21:23, 31:37, 41:46, 50:54, 61:65)
  baseurl <- "https://www.stats.gov.cn/sj/tjbz/tjyqhdmhcxhfdm"
  provurl <- paste0(paste(baseurl, year, prov, sep = "/"), ".html")

  get_data <- memoise(function(x, xiang=FALSE){
    response <- httr::GET(x, timeout = 5)
    # read html content
    html_content <- httr::content(response, "text", encoding = "UTF-8")
    html_table <- rvest::read_html(html_content) %>%
      rvest::html_table(fill = TRUE)
    res <- html_table[[5]]
    
    if (xiang){
      colnames(res) <- c("areacode", "citycode", "name")
    } else {
      colnames(res) <- c("areacode", "name")
    }
    res <- res[2:nrow(res), ]
    return(res)
  }, cache = cachem::cache_disk(dir = cache_dir))
  
  if (cache_refresh) { forget(get_data) }
  
  citys <- purrr::reduce(lapply(provurl, get_data), bind_rows)
  city_codes <- substr(citys$areacode, 1, 4)
  cityurl <- paste0(paste(baseurl,year,
                          substr(city_codes, 1, 2),
                          city_codes, sep = "/"),".html") 
  
  if (level == "city"){
    return(citys)
  }
  
  countys <- purrr::reduce(lapply(cityurl, get_data), bind_rows)
  
  county_code <- substr(countys$areacode, 1, 6)
  county_code <- county_code[!(substr(county_code,5,6)=="01"&!substr(county_code,3,4)=="90")]
  countyurl <- paste0(paste(baseurl, year,
                            substr(county_code, 1, 2),
                            substr(county_code, 3, 4),
                            county_code, sep = "/"), ".html")
  if (level =="county"){
    return(bind_rows(citys, countys) %>% arrange(areacode))
  }
  
  xiang <- purrr::reduce(lapply(countyurl, get_data), bind_rows)
  xiang_code <- substr(xiang$areacode, 1, 9)
  xiangurl <- paste0(paste(baseurl, year,
                           substr(xiang_code, 1, 2),
                           substr(xiang_code, 3, 4),
                           substr(xiang_code, 5, 6),
                           xiang_code, sep = "/"), ".html")
  if (level == "xiang"){
    return( bind_rows(citys, countys, xiang) %>% arrange(areacode))
  }
  cun <- purrr::reduce(lapply(xiangurl, get_data, xiang = TRUE), bind_rows)
}