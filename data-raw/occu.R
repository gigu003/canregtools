## code to prepare `occu` dataset goes here
occu_map <- data.frame(
  code = sprintf("%02d", c(11, 13, 17, 21, 24, 27, 31, 37, 51, 54, 70, 80, 90)),
  cname = c("国家公务员", "专业技术人员", "职员", "企业管理人员", "工人",
            "农民", "学生", "现役军人", "自由职业者", "个体经营者", "无业人员",
            "退（离）休人员", "其他"),
  ename = c("Civil Servant", "Professional/Technical Personnel",
            "Clerk", "Enterprise Manager", "Worker", "Farmer", "Student",
            "Active Duty Military Personnel", "Self-employed",
            "Small Business Owner", "Unemployed", "Retired", "Others")
)