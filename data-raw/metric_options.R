# For dropdown menus
metric_options <- SMdata::metadata %>% 
  pull(metric)

# Reorder metrics options, put NAICS last cause they are unfortunate
metric_options <- c(
  sort(metric_options[!grepl("NAICS", metric_options)]),
  sort(metric_options[grepl("NAICS", metric_options)])
)

usethis::use_data(metric_options, overwrite = TRUE)
