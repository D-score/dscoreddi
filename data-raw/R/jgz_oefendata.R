#oefendata BDS
#voorbeelddata van JGZ gemeente Utrecht

jgz_background <- readRDS("data-raw/data/oefendata populatie.Rds")
jgz_vwo <- readRDS("data-raw/data/oefendata Van Wiechen.Rds")



usethis::use_data(jgz_background, overwrite = TRUE)
usethis::use_data(jgz_vwo, overwrite = TRUE)

