#' Create Base Map
#'
#' @description Create base map from spatial layer to be displayed before user
#'  selects a metric
#' @param spatial_data Spatial data (sf object) with county or state boundaries
#' @param resolution Resolution level - 'County' or 'State' (default: 'County')
#'
#' @returns Rendered leaflet map with tiles, polygons
#' @export
#'
#' @examples
create_base_map <- function(spatial_data, resolution = 'County') {

  # Create initial popup showing area info
  initial_popup <- paste0(
    "<div style='text-align: center;'>",
    "<b>", spatial_data$county_name, "</b><br>",
    "<strong>Land Area:</strong> ", format(round(spatial_data$ALAND / 1e6, 2), big.mark = ","), " km²<br>",
    "<strong>Water Area:</strong> ", format(round(spatial_data$AWATER / 1e6, 2), big.mark = ","), " km²",
    "</div>"
  )
  
  leaflet::leaflet(spatial_data) %>%
    leaflet::addProviderTiles(
      leaflet::providers$OpenStreetMap.Mapnik,
      group = 'OpenStreetMap.Mapnik'
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$USGS.USImagery,
      group = 'USGS.USImagery'
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron,
      group = 'CartoDB.Positron'
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c(
        'CartoDB.Positron',
        'OpenStreetMap.Mapnik',
        'USGS.USImagery'
      ),
      overlayGroups = 'Counties',
      options = leaflet::layersControlOptions(collapsed = TRUE),
      position = 'topleft'
    ) %>%
    leaflet::setView(
      lng = -67.44604,
      lat = 42.58503,
      zoom = 6
    ) %>%
    leaflet::addPolygons(
      color = "black",
      weight = 1,
      smoothFactor = 0.8,
      opacity = 0.7,
      fillOpacity = 0.5,
      fillColor = 'lightgray',
      highlightOptions = leaflet::highlightOptions(
        color = "white",
        weight = 2,
        bringToFront = TRUE
      ),
      popup = initial_popup,
      popupOptions = leaflet::popupOptions(closeButton = FALSE),
      # label = label_col,
      group = 'Counties'
    )
}


#' Custom Popup
#'
#' @description Formatting for leaflet popup based on map_data inputs. Note that
#' this currently relies on logic in mod_map based on `input$resolution` to
#' account for when state data is used and there is no county_name. That should
#' probably be rolled into this function at some point
#' 
#' @param county_name 
#' @param state_name 
#' @param variable_name 
#' @param value 
#' @param metric User `input$metric`, passed through `get_map_formulas()`.
#'
#' @returns
#' @export
#'
#' @examples
custom_popup <- function(county_name, state_name, variable_name, value, metric) {
  # If the metric is at state level, county_name will be NA
  # In this case, use state name instead
  area_name <- ifelse(is.na(county_name), state_name, county_name)

  paste0(
    "<div style='text-align: center;'>",
    "<b>", area_name, "</b><br>",
    "<strong>", metric, ":</strong> ", round(value, 2),
    # "<strong>", variable_name, ":</strong> ", round(value, 2),
    "</div>"
  )
}


#' Get Popup and Label Formulas
#'
#' @description
#' Returns appropriate popup, label formulas, and group name based on resolution.
#' County data has county_name column, state data does not - this function
#' handles that difference.
#'
#' @param resolution Either 'County' or 'State'
#'
#' @returns List with popup_formula, label_formula, and group name
#' @export
#'
#' @examples
get_map_formulas <- function(resolution, metric) {
  # metric_label <- metric_lookup %>% 
  #   filter(Metric == metric)
  if (resolution == 'County') {
    list(
      popup = ~custom_popup(county_name, state_name, variable_name, value, metric),
      label = ~county_name,
      group = 'Counties'
    )
  } else {
    list(
      popup = ~custom_popup(NA, state_name, variable_name, value, metric),
      label = ~state_name,
      group = 'States'
    )
  }
}


#' Validate Map Data
#'
#' @description
#' Validates that map data has valid (non-NA) values before rendering.
#' Returns validation results with appropriate error/warning messages.
#'
#' @param data Filtered map data (sf object with value column)
#' @param metric_name Name of the metric being displayed
#' @param year Year being displayed
#' @param resolution Resolution level (County or State)
#' @param warn_threshold Threshold for warning about sparse data (default 0.2 = 20% valid)
#'
#' @returns List with:
#'   - valid: TRUE if data can be rendered, FALSE if it should fail
#'   - message: Error or warning message (NULL if no issues)
#'   - type: "error", "warning", or NULL
#'
#' @export
#'
#' @examples
validate_map_data <- function(data, 
                              metric_name, 
                              year, 
                              resolution, 
                              warn_threshold = 0.2) {

  # Check if value column exists
  if (!"value" %in% names(data)) {
    return(list(
      valid = FALSE,
      message = paste0(
        "No data found for metric '", metric_name, "'. ",
        "Please select a different metric or check the data source."
      ),
      type = "error"
    ))
  }

  # Count valid (non-NA) values
  total_rows <- nrow(data)
  valid_values <- sum(!is.na(data$value))

  # Check if all values are NA
  if (valid_values == 0) {
    return(list(
      valid = FALSE,
      message = paste0(
        "No valid data available for '", metric_name, "' in ",
        year, " at ", resolution, " level. ",
        "All values are missing (NA). Please select a different metric or year. ",
        "<a href='https://github.com/Food-Systems-Research-Institute/SMexplorer/issues' target='_blank'>Report this issue</a>."
      ),
      type = "error"
    ))
  }

  # Warn if mostly NA values
  pct_valid <- valid_values / total_rows
  if (pct_valid < warn_threshold) {
    pct_na <- round((1 - pct_valid) * 100, 1)
    return(list(
      valid = TRUE,
      message = paste0(
        "Warning: ", pct_na, "% of values are missing for '", metric_name, "'. ",
        "The map may appear sparse."
      ),
      type = "warning"
    ))
  }

  # All good - no issues
  return(list(
    valid = TRUE,
    message = NULL,
    type = NULL
  ))
}
