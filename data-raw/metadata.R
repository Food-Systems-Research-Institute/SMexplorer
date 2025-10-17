pacman::p_load(dplyr)
pacman::p_load_current_gh('Food-Systems-Research-Institute')

metadata <- SMdata::metadata %>% 
  mutate(across(
    c(metric, dimension, index, indicator, resolution),
    ~ str_to_sentence(.x)
  ))
usethis::use_data(metadata, overwrite = TRUE)
