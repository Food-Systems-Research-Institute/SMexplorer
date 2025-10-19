
# Housekeeping ------------------------------------------------------------

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


# Metadata ----------------------------------------------------------------

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

# Fix bad metrics where resolution isn't appropriate
bad_metrics <- readRDS('dev/bad_metrics.rds')
out <- metadata %>% 
  filter(
    !(Metric %in% bad_metrics[[1]] & 'county|County' %in% Resolution),
    !(Metric %in% bad_metrics[[2]] & 'state|State' %in% Resolution)
  )
dim(metadata)
dim(out)

metadata %>% 
  filter(Metric %in% bad_metrics[[1]]) %>% 
  select(Metric, Resolution)

# If axis name is not present, use metric name instead
metadata <- metadata %>% 
  mutate('Axis Name' = case_when(
    is.na(`Axis Name`) ~ Metric,
    .default = `Axis Name`
  ))
get_str(metadata)

# If metric name is not present, use variable name instead
metadata <- metadata %>% 
  mutate('Metric' = case_when(
    is.na(Metric) ~ snakecase::to_sentence_case(`Variable Name`),
    .default = Metric 
  ))
get_str(metadata)

# For now, remove anything with NAICS or yield. Pretty janky
metadata <- metadata %>% 
  dplyr::filter(
    stringr::str_detect(
      `Variable Name`, 
      '^lq|NAICS|MeasuredIn|soilOrganicCarbon|nProducersAreaOperated.|availableWaterStorage', 
      negate = TRUE
    )
  )
get_str(metadata)
metadata$`Variable Name` %>% sort

# New column with years as a vector, so we don't have to split by comma later
metadata <- metadata %>% 
  mutate(`Year Vector` = str_split(Year, ', ') %>% 
           map(as.integer) %>% 
           map(~ sort(.x, decreasing = TRUE)))
get_str(metadata)

# New column with booleans for state and county resolution so we don't have to
# do string operations in module
metadata <- metadata %>% 
  mutate(
    'RES_Northeast' = ifelse(grepl('northeast', Resolution, ignore.case = TRUE), TRUE, FALSE),
    'RES_State' = ifelse(grepl('state', Resolution, ignore.case = TRUE), TRUE, FALSE),
    'RES_County' = ifelse(grepl('county', Resolution, ignore.case = TRUE), TRUE, FALSE)
  )
metadata %>% 
  select(starts_with('RES'), Resolution)

# Remove bad vars (identified in data-raw/neast_metrics.R)
bad_vars <- readRDS('dev/bad_vars.rds')
metadata <- metadata %>% 
  filter(!`Variable Name` %in% bad_vars)

# Save
usethis::use_data(metadata, overwrite = TRUE)


# Lookup Tables -----------------------------------------------------------

# Smaller df to translate between names within modules
metric_lookup <- metadata %>% 
  select(Metric, 'Variable Name', 'Axis Name')
get_str(metric_lookup)
usethis::use_data(metric_lookup, overwrite = TRUE)

# Named vector for one-way metric to years
metric_to_years <- split(metadata[['Year Vector']], metadata$Metric) %>% 
  purrr::flatten()
head(metric_to_years)
metric_to_years[['Acres drained by tile']]
usethis::use_data(metric_to_years, overwrite = TRUE)

# # Named vector for one-way metric to resolution
# metric_to_resolution <- setNames(metadata$Resolution, metadata$Metric)
# head(metric_to_resolution)
# metric_to_resolution[['Civilian labor force']]
# usethis::use_data(metric_to_resolution, overwrite = TRUE)

# Resolution lookup table
metric_resolution_lookup <- metadata %>% 
  select(
    Dimension, 
    Metric, 
    'Variable Name', 
    Resolution, 
    starts_with('RES'),
    'Year Vector'
  )
usethis::use_data(metric_resolution_lookup, overwrite = TRUE)
