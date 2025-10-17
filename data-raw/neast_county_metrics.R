## code to prepare `neast_county_metrics` dataset goes here

pacman::p_load(dplyr)
pacman::p_load_current_gh(
  'Food-Systems-Research-Institute/SMdata',
  'ChrisDonovan307/projecter'
)

neast_county_metrics <- SMdata::metrics %>% 
  SMdata::filter_fips('counties')
get_str(neast_county_metrics)

usethis::use_data(neast_county_metrics, overwrite = TRUE)
