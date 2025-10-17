## code to prepare `neast_state_metrics` dataset goes here

pacman::p_load(dplyr)
pacman::p_load_current_gh(
  'Food-Systems-Research-Institute/SMdata',
  'ChrisDonovan307/projecter'
)

neast_state_metrics <- SMdata::metrics %>% 
  SMdata::filter_fips('states')
get_str(neast_state_metrics)

usethis::use_data(neast_state_metrics, overwrite = TRUE)
