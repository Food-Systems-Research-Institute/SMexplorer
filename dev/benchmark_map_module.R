# Benchmark script for map module performance
# Run this before and after optimizations to compare performance

library(profvis)
library(leaflet)
library(sf)
library(dplyr)

# Benchmark: Data Loading ----
cat("=== Benchmarking Data Loading ===\n")

profvis({
  load('data/neast_county_metrics.rda')
  load('data/neast_state_metrics.rda')
  load('data/neast_counties_2021.rda')
  load('data/neast_counties_2024.rda')
  load('data/neast_county_spatial_2021.rda')
  load('data/neast_county_spatial_2024.rda')
  load('data/neast_state_spatial.rda')
  load('data/metadata.rda')
}, interval = 0.005)

cat("\nData loaded. Close profvis window to continue...\n\n")

# Benchmark: Initial Map Rendering ----
cat("=== Benchmarking Initial Map Rendering ===\n")

# Make sure data is loaded
if (!exists("neast_county_spatial_2024")) {
  load('data/neast_county_spatial_2024.rda')
}

# Custom popup function (from mod_map.R line 216)
custom_popup <- paste0(
  "<div style='text-align: center;'>",
  "<b>", neast_county_spatial_2024$county_name, "</b><br>",
  "Initial map - no metric selected"
)

profvis({
  initial_map <- leaflet(neast_county_spatial_2024) %>%
    addProviderTiles(
      providers$OpenStreetMap.Mapnik,
      group = 'OpenStreetMap.Mapnik'
    ) %>%
    addProviderTiles(
      providers$USGS.USImagery,
      group = 'USGS.USImagery'
    ) %>%
    addProviderTiles(
      providers$CartoDB.Positron,
      group = 'CartoDB.Positron'
    ) %>%
    addPolygons(
      color = "black",
      weight = 1,
      smoothFactor = 0.8,
      opacity = 0.7,
      fillOpacity = 0.5,
      fillColor = 'lightgray',
      highlightOptions = highlightOptions(
        color = "white",
        weight = 2,
        bringToFront = TRUE
      ),
      popup = custom_popup,
      popupOptions = popupOptions(closeButton = FALSE),
      label = ~county_name,
      group = 'Counties'
    ) %>%
    addLayersControl(
      baseGroups = c(
        'CartoDB.Positron',
        'OpenStreetMap.Mapnik',
        'USGS.USImagery'
      ),
      overlayGroups = c('Counties'),
      options = layersControlOptions(collapsed = TRUE),
      position = 'topleft'
    ) %>%
    setView(
      lng = -67.44604,
      lat = 42.58503,
      zoom = 6
    )
}, interval = 0.005)

cat("\nInitial map rendered. Close profvis window to continue...\n\n")

# Benchmark: Map Update with Data Join ----
cat("=== Benchmarking Map Update (Data Join + Re-render) ===\n")

# Load necessary data
if (!exists("metadata")) load('data/metadata.rda')
if (!exists("neast_county_metrics")) load('data/neast_county_metrics.rda')
if (!exists("neast_county_spatial_2024")) load('data/neast_county_spatial_2024.rda')

# Simulate user selecting a metric and year
test_metric <- metadata$Metric[1]  # First available metric
test_variable <- metadata %>%
  filter(Metric == test_metric) %>%
  pull('Variable Name')
test_year <- metadata %>%
  filter(Metric == test_metric) %>%
  pull('Year Vector') %>%
  unlist() %>%
  .[1]

profvis({
  # Join metrics with spatial data (lines 331-342 from mod_map.R)
  updated_metrics <- neast_county_metrics %>%
    dplyr::filter(
      variable_name == test_variable,
      year == test_year
    ) %>%
    dplyr::right_join(neast_county_spatial_2024, by = 'fips')

  # Create popup function
  custom_popup_updated <- paste0(
    "<div style='text-align: center;'>",
    "<b>", updated_metrics$county_name, "</b><br>",
    "<strong>", test_variable, ":</strong> ", round(updated_metrics$value, 2)
  )

  # Create color palette
  pal <- colorNumeric(
    palette = "YlGn",
    domain = updated_metrics$value,
    reverse = FALSE
  )

  # Make sure it's sf object
  if (!'sf' %in% class(updated_metrics)) {
    updated_metrics <- st_as_sf(updated_metrics)
  }

  # Simulate leafletProxy update (lines 383-415)
  # Note: Can't actually use leafletProxy outside of Shiny,
  # but we can measure the polygon creation which is the expensive part
  updated_map <- leaflet(updated_metrics) %>%
    addPolygons(
      color = "black",
      weight = 1,
      smoothFactor = 0.5,
      opacity = 1.0,
      fillOpacity = 0.8,
      fillColor = ~pal(value),
      highlightOptions = highlightOptions(
        color = "white",
        weight = 2,
        bringToFront = TRUE
      ),
      popup = custom_popup_updated,
      popupOptions = popupOptions(closeButton = FALSE),
      label = ~county_name,
      group = 'Counties'
    ) %>%
    addLegend(
      "bottomleft",
      pal = pal,
      values = ~value,
      title = 'Metric Values',
      labFormat = labelFormat(prefix = " "),
      opacity = 1
    )
}, interval = 0.005)

cat("\nMap update complete. Close profvis window to finish.\n\n")

# Summary ----
cat("=== Benchmark Complete ===\n")
cat("You tested:\n")
cat("  1. Data loading (8 .rda files)\n")
cat("  2. Initial map rendering with base layers\n")
cat("  3. Map update with data join and re-render\n\n")
cat("Look for bottlenecks in:\n")
cat("  - Data loading time\n")
cat("  - addPolygons() calls\n")
cat("  - Data joins (right_join, left_join)\n")
cat("  - st_as_sf() conversions\n\n")
cat("Save profvis results, then make optimizations and re-run this script.\n")
