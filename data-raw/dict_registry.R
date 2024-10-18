## code to prepare `dict_registry` dataset goes here
dict_registry <- read_excel("data-raw/registry.xlsx")
dict_registry <- dict_registry |>
  mutate(registry = ifelse(is.na(registry), 0, registry))
  

