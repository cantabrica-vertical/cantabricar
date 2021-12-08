## code to prepare `valores` dataset goes here
valores <- read.csv("data-raw/valores.csv")
usethis::use_data(valores, overwrite = TRUE, internal = TRUE)
