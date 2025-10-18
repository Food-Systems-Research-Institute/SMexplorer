pacman::p_load(
  dplyr,
  snakecase
)
pacman::p_load_current_gh('Food-Systems-Research-Institute/SMdata')

metadata <- SMdata::metadata %>% 
  mutate(across(
    c(metric, dimension, index, indicator, resolution, units),
    ~ str_to_sentence(.x)
  )) %>% 
  setNames(c(
    names(.) %>% 
      snakecase::to_title_case() %>% 
      stringr::str_replace('Url', 'URL')
  )) %>% 
  select(-c(Fips, Value))
get_str(metadata)

# If axis name is not present, use metric name instead
metadata <- metadata %>% 
  mutate('Axis Name' = case_when(
    is.na(`Axis Name`) ~ Metric,
    .default = `Axis Name`
  ))
get_str(metadata)

# For now, remove anything with NAICS
metadata <- metadata %>% 
  filter(
    str_detect(
      `Variable Name`, 
      '^lq|NAICS', 
      negate = TRUE
    )
  )
get_str(metadata)

usethis::use_data(metadata, overwrite = TRUE)

