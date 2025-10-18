# Wrangling all metric variations here

pacman::p_load(dplyr)
pacman::p_load_current_gh(
  'Food-Systems-Research-Institute/SMdata',
  'ChrisDonovan307/projecter'
)

metrics <- list(
  # County
  neast_county_metrics = SMdata::metrics %>% 
    SMdata::filter_fips('counties'),
  
  # State
  neast_state_metrics = SMdata::metrics %>% 
    SMdata::filter_fips('states'),
  
  # All Northeast
  neast_metrics = SMdata::metrics %>% 
    SMdata::filter_fips('neast')
)
get_str(metrics)
get_str(metrics, 3)

# For now, removing anything that starts with lq or contains NAICS
# Too many to deal with in this format.
metrics <- map(metrics, ~ {
  .x %>% 
    filter(
      str_detect(
        variable_name, 
        '^lq|NAICS', 
        negate = TRUE
      )
    )
})
get_str(metrics, 3)
map(metrics, get_size)

# # Check
# var_names <- map(metrics, ~ {
#   .x$variable_name %>% 
#     unique()
# }) %>% 
#   unlist() %>% 
#   unique()
# str(var_names)

list2env(metrics, envir = .GlobalEnv)

usethis::use_data(neast_metrics, overwrite = TRUE)
usethis::use_data(neast_county_metrics, overwrite = TRUE)
usethis::use_data(neast_state_metrics, overwrite = TRUE)
