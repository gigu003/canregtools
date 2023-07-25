read_canreg <- function(x, pop.type = "long"){
  #if the file or folder exist.
  if (!file.exists(x)) {
    stop("Error: The file or directory does not exist.")
  }
  # Read the cancer registration file stored in excel.
  get_file <- function(x, type = pop.type){
    tryCatch({
    incidence <- read_excel(x, sheet = "FB")
    names(incidence) <- tolower(names(incidence))
    mortality <- read_excel(x, sheet = "SW")
    names(mortality) <- tolower(names(mortality))
    # Extract year information
      fyear <- table(format(as.Date(incidence$inciden), "%Y"))
      fyear <- as.numeric(names(fyear)[which.max(fyear)])
      syear <- table(format(as.Date(mortality$deathda), "%Y"))
      syear <- as.numeric(names(syear)[which.max(syear)])
      if (fyear == syear) {
        year <- fyear
      } else {
        year <- NA
      }
    
    if (type == "wide") {
      population <- read_excel(x,
                               sheet = "POP",
                               range = "B2:C20",
                               col_names = c("male","female"))
      population <- round(population)
    } else if (type == "long"){
      population <- read_excel(x, sheet = "POP")
      population$rks <- round(population$rks)
    }
    class(incidence) <- c("incidence")
    class(mortality) <- c("mortality", "tibble", "data.frame")
    class(population) <- c("population", "tibble", "data.frame")
    bsname <- tools::file_path_sans_ext(basename(x))
    areacode <- gsub("\\D", "", bsname)
    name <- gsub("\\d", "", bsname)
    res <- list(areacode = areacode,
                name = name,
                year = year,
                incidence = incidence,
                mortality = mortality,
                population = population)
    class(res) <- c("canreg","list")
    return(res)
    }, error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)  # Return NULL if an error occurs
    })
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
}


