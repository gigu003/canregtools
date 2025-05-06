## code to prepare `registry_dict` dataset goes here
areacodes <- c("410102", "410103", "410106",
               "410302", "410303", "410304", "410305",
               "410402", "410403", "410411",
               "410602", "410603", "410611",
               "410802", "410803", "410811",
               "411102", "411104",
               "410502", "410503", "410505")

registry_dict <- as.list(c(rep("410100", 3), rep("410300", 4),
                           rep("410400", 3), rep("410600", 3),
                           rep("410800", 3), rep("411100", 2),
                           rep("410500", 3)))
names(registry_dict) <- areacodes

cc <- data.frame(
  areacode = areacodes,
  registry = unlist(registry_dict)
)

