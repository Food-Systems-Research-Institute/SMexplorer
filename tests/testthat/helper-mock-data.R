# Helper functions for creating mock data in tests

#' Create Mock Map Data
#'
#' @description Helper function to create properly structured sf objects
#' that match the map_data() structure from mod_map.R. Used for testing
#' map-related functions.
#'
#' @returns sf object with proper structure matching map_data()
create_mock_map_data <- function(values, include_county = TRUE) {
  n <- length(values)

  # Create proper sf geometry column
  geom <- sf::st_sfc(lapply(1:n, function(i) {
    sf::st_point(c(i, i))
  }), crs = 4326)

  # Build data frame matching map_data() structure from mod_map.R
  data <- data.frame(
    fips = sprintf("%05d", 1000 + 1:n),
    state_name = rep("Test State", n),
    ALAND = rep(1e9, n),
    AWATER = rep(1e6, n),
    variable_name = rep("test_var", n),
    year = rep(2024, n),
    value = values,
    stringsAsFactors = FALSE
  )

  # Add county_name for county-level data
  if (include_county) {
    data$county_name <- paste("County", 1:n)
  }

  # Convert to sf object
  sf::st_sf(data, geometry = geom)
}

#' Get Mock Metadata
#'
#' @returns
#' @export
#'
#' @examples
get_mock_metadata <- function() {
  dimensions <- c('Economics', 'Production', 'Social', 'Health', 'Environment')
  mock_metadata <- map(dimensions, ~ {
    metadata %>% 
      dplyr::filter(Dimension == .x) %>% 
      dplyr::slice(1:10)
  }) %>% 
    dplyr::bind_rows()
  return(mock_metadata)
}
