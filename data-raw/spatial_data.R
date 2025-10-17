pacman::p_load(
  sf,
  terra,
  rmapshaper,
  mapview,
  tigris,
  purrr,
  stringr,
  dplyr
)

pacman::p_load_current_gh(
  'Food-Systems-Research-Institute/SMdata',
  'ChrisDonovan307/projecter'
)


# Download Layers ---------------------------------------------------------

neast_state_fips <- SMdata::neast_states$fips

neast_county_spatial_2024 <- tigris::counties(
  state = neast_state_fips,
  year = 2024
)

neast_county_spatial_2021 <- tigris::counties(
  state = neast_state_fips,
  year = 2021
)

neast_state_spatial <- tigris::states(cb = TRUE) %>% 
  dplyr::filter(STATEFP %in% neast_state_fips)

layers <- mget(c(
  'neast_county_spatial_2024',
  'neast_county_spatial_2021',
  'neast_state_spatial'
))


# Downsample --------------------------------------------------------------

# Check size
get_size(layers[[1]])
get_size(layers[[3]])

# Simplify
layers_simple <- map(layers, rmapshaper::ms_simplify)
map(layers_simple, get_size)


# Wrangle -----------------------------------------------------------------

# Remove some columns and keep only the good stuff in a familiar format
get_str(layers_simple)
get_str(fips_key)
crosswalk <- fips_key %>% 
  select(fips, county_name, state_name)

wrangled_layers <- imap(layers_simple, \(layer, name) {
  if (str_detect(name, 'county')) {
    df <- layer %>% 
      rename(
        fips = GEOID,
        county_name = NAMELSAD
      ) %>% 
      left_join(crosswalk, by = 'fips')
  } else {
    df <- layer %>% 
      rename(
        fips = STATEFP,
        state_name = NAME
      )
  }
  
  df %>% 
    select(any_of(c(
      'fips',
      'county_name',
      'state_name',
      'ALAND',
      'AWATER'
    )))
})
get_str(wrangled_layers)
map(wrangled_layers, get_size)

# Split list
list2env(wrangled_layers, envir = .GlobalEnv)
str(neast_county_spatial_2024)

# Save for use in dashboard
usethis::use_data(
  neast_county_spatial_2024,
  neast_county_spatial_2021,
  neast_state_spatial,
  overwrite = TRUE
)
