## code to prepare `fips_key` dataset goes here

pacman::p_load_current_gh('Food-Systems-Research-Institute/SMdata')
fips_key <- SMdata::fips_key
usethis::use_data(fips_key, overwrite = TRUE)
