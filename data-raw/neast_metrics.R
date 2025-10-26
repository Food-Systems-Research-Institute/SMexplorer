# Wrangling all metric variations here

pacman::p_load(dplyr, duckdb)
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
        '^lq|NAICS|MeasuredIn|soilOrganicCarbon|nProducersAreaOperated.|availableWaterStorage', 
        negate = TRUE
      )
    )
})
get_str(metrics, 3)
map(metrics, get_size)

# Remove any metric that is all NA
bad_vars <- map(metrics, \(df) {
  vars <- unique(df$variable_name)
  bad_vars <- map(vars, \(var) {
    vec <- df %>% 
      filter(variable_name == var) %>% 
      pull(value)
    if (all(is.na(vec))) {
      return(var)
    } else {
      return(NULL)
    }
  }) %>% 
    keep(is.character) %>% 
    unlist()
  return(bad_vars)
}) 
bad_vars <- bad_vars %>% unlist() %>% unique()
bad_vars

# Save these to also remove from metadata
saveRDS(bad_vars, 'dev/bad_vars.rds')

# Remove bad vars from metrics  
metrics <- map(metrics, ~ {
  clean_df <- .x %>% 
    filter(!variable_name %in% bad_vars)
  return(clean_df)
})
get_str(metrics, 3)
map(metrics, get_size)

list2env(metrics, envir = .GlobalEnv)

usethis::use_data(neast_metrics, overwrite = TRUE)
usethis::use_data(neast_county_metrics, overwrite = TRUE)
usethis::use_data(neast_state_metrics, overwrite = TRUE)

saveRDS(SMdata::fips_key, 'data/fips_key.rds')


