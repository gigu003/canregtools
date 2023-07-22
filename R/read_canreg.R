read_canreg <- function(x,
                        type = "single"){
  #if the file or folder exist.
  if (!file.exists(x)) {
    stop("Error: The file or directory does not exist.")
  }
  
  tryCatch({
    
    get_file <- function(x){
      incidence <- read_excel(x, sheet = "FB")
      mortality <- read_excel(x, sheet = "SW")
      if (type == "single") {
        population <- read_excel(x,
                                 sheet = "POP",
                                 range = "B2:C20",
                                 col_names = c("male","female"))
      }
      class(incidence) <- c("incidence", "tibble", "data.frame")
      class(mortality) <- c("mortality", "tibble", "data.frame")
      class(population) <- c("population", "tibble", "data.frame")
      population <- round(population)
      areacode <- gsub("\\D", "", x)
      name <- gsub("\\d", "", x)
      res <- list(areacode = areacode,
                  name = name,
                  incidence = incidence,
                  mortality = mortality,
                  population = population)
      class(res) <- c("canreg","list")
      return(res)
    }
    
    if(file.info(x)$isdir){
      files <- list.files(x)
      isxls <- tolower(tools::file_ext(files)) %in% c("xls", "xlsx")
      files <- files[isxls]
      if (length(files) == 0) {
        stop("No Excel files found in the specified directory.")
      }
      files <- paste0(x,"/",files)
      res <- lapply(files, get_file)
      return(res)
    } else {
      get_file(x)
    }
  }, error = function(e) {
    message("An error occurred: ", conditionMessage(e))
    return(NULL)  # return null
  })
}