pacman::p_load(
  dplyr,
  snakecase,
  stringr,
  purrr
)
pacman::p_load_current_gh(
 'Food-Systems-Research-Institute/SMdata',
 'ChrisDonovan307/projecter'
)

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

# New column with years as a vector, so we don't have to split by comma later
metadata <- metadata %>% 
  mutate(`Year Vector` = str_split(Year, ', ') %>% 
           map(as.integer) %>% 
           map(~ sort(.x, decreasing = TRUE)))
get_str(metadata)

# Save
usethis::use_data(metadata, overwrite = TRUE)
