read_canreg2 <- function(x, pop.type = "long"){
  #if the file or folder exist.
  if (!file.exists(x)) {
    stop("Error: The file or directory does not exist.")
  }
  # Read the cancer registration file stored in excel.
  get_file <- function(x, type = pop.type){
    tryCatch({
    fb <- read_excel(x, sheet = "FB")
    names(fb) <- tolower(names(fb))
    sw <- read_excel(x, sheet = "SW")
    names(sw) <- tolower(names(sw))
    # Extract year information
      fyear <- table(format(as.Date(fb$inciden), "%Y"))
      fyear <- as.numeric(names(fyear)[which.max(fyear)])
      syear <- table(format(as.Date(sw$deathda), "%Y"))
      syear <- as.numeric(names(syear)[which.max(syear)])
      if (fyear == syear) {
        year <- fyear
      } else {
        year <- NA
      }
    
    if (type == "wide") {
      pop <- read_excel(x,
                        sheet = "POP",
                        range = "B2:C20",
                        col_names = c("male","female"))
      pop <- round(pop)
      pop <- tibble(year = c(rep(fyear, 19)),
                    sex = c(rep(1,19), rep(2,19)),
                    agegrp =factor(c(rep(1:19, 2))),
                    rks = c(pop[,1], pop[,2]))
    } else if (type == "long"){
      pop <- read_excel(x, sheet = "POP")
      pop$year <- as.integer(pop$year)
      pop$rks <- round(pop$rks)
      pop$sex <- as.integer(pop$sex)
      pop$agegrp <- factor(pop$agegrp)
    }
    attr(fb, "class") <- c("FBcases", "tibble", "data.frame")
    attr(sw, "class") <- c("SWcases", "tibble", "data.frame")
    attr(pop, "class") <- c("population", "tibble", "data.frame")
    bsname <- tools::file_path_sans_ext(basename(x))
    areacode <- gsub("\\D", "", bsname)
    name <- gsub("\\d", "", bsname)
    res <- list(areacode = areacode,
                name = name,
                year = year,
                FBcases = fb,
                SWcases = sw,
                POP = pop)
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
