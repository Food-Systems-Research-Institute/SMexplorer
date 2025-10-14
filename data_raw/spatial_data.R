pacman::p_load(
  sf,
  terra,
  rmapshaper,
  mapview
)

pacman::p_load_current_gh('ChrisDonovan307/projecter')


# 2024 --------------------------------------------------------------------

load('data/counties_2024.rda')
get_size(counties_2024)

simple_2024 <- rmapshaper::ms_simplify(counties_2024)
simple_2024 <- st_simplify(counties_2024, dTolerance = 1000)
get_size(simple_2024)
counties_2024 <- simple_2024
usethis::use_data(counties_2024, overwrite = TRUE)


# 2021 --------------------------------------------------------------------

load('data/counties_2021.rda')
get_size(counties_2021)

simple_2021 <- rmapshaper::ms_simplify(counties_2021)
get_size(simple_2021)
counties_2021 <- simple_2021
usethis::use_data(counties_2021, overwrite = TRUE)

