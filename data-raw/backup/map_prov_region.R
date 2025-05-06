## code to prepare `map_prov_region` dataset goes here
names <- as.character(c(11:15, 21:23, c(31:34, 37), c(41:43, 36), c(44:46, 35),
                        50:54, 61:65, c(71, 81, 82))) 
map_prov_region <- c(rep("72", 5), rep("73", 3), rep("74", 5), rep("75", 4),
                     rep("76", 4), rep("77", 5), rep("78", 5), rep("79", 3))
names(map_prov_region) <- names

prov <- data.frame(
  prov = names,
  region = map_prov_region
)

