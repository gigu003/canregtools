create_report <- function(data,
                          template = "annual_report.Rmd",
                          output_file = "report.html"){
  # get the template path
  template_path <- system.file("rmarkdown", template, package = "canregtools")
  
  if (is.null(template_path) || !file.exists(template_path)) {
    stop("Template not found: ", template)
  }
  
  # 你可以在这里添加代码来处理数据或者准备报告需要的变量
  areacode <- data$areacode

  
    
  # 使用rmarkdown::render来生成报告
  rmarkdown::render(template_path, 
                    output_file = output_file, 
                    params = list(
                      data = data
                    ))
  
  cat("Report generated:", output_file, "\n")

}


