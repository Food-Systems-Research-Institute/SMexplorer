#' Get Latest Metric Value for Value Box
#'
#' @description Queries the database for the latest value of a given metric
#' for a specific location and resolution, with flexible formatting options.
#'
#' @param con DuckDB connection object
#' @param global_data List containing fips_key and metadata
#' @param variable_name Character string of the variable name to query
#' @param location Character string of the location name (county or state)
#' @param resolution Character string, either "County" or "State"
#' @param format_type Character string: "percent", "currency", "decimal", or "raw"
#' @param decimal_places Numeric, number of decimal places for "decimal" format (default 2)
#' @param multiplier Numeric, multiply value before formatting (default 1)
#'
#' @return List with two elements:
#'   - value: Formatted value string or "N/A"
#'   - year: Year of the data or NULL
#'
#' @noRd
#'
#' @importFrom dplyr filter pull %>%
#' @importFrom glue glue
get_latest_metric_value <- function(con,
                                     global_data,
                                     variable_name,
                                     location,
                                     resolution,
                                     format_type = "raw",
                                     decimal_places = 2,
                                     multiplier = 1) {

  # Get FIPS code from fips_key
  if (resolution == 'County') {
    location_fips <- global_data$fips_key %>%
      filter(county_name == location) %>%
      pull(fips) %>%
      .[1]
  } else {
    location_fips <- global_data$fips_key %>%
      filter(state_name == location) %>%
      pull(fips) %>%
      .[nchar(.) == 2] %>%
      .[1]
  }

  # Return N/A if no FIPS found
  if (is.na(location_fips) || length(location_fips) == 0) {
    return(list(value = "N/A", year = NULL))
  }

  # Query latest value from database
  table <- paste0('neast_', tolower(resolution), '_metrics')
  query <- glue::glue(
    "SELECT value, year
    FROM {table}
    WHERE variable_name = '{variable_name}'
      AND fips = '{location_fips}'
      AND value IS NOT NULL
    ORDER BY year DESC
    LIMIT 1"
  )

  result <- query_db(con, query)

  # Return N/A if no data found
  if (nrow(result) == 0) {
    return(list(value = "N/A", year = NULL))
  }

  # Extract value and year
  raw_value <- result$value[1]
  year <- result$year[1]

  # Apply multiplier
  raw_value <- raw_value * multiplier

  # Format value based on format_type
  formatted_value <- switch(
    format_type,
    "percent" = paste0(round(raw_value, decimal_places), "%"),
    "currency" = paste0("$", format(round(raw_value, decimal_places), big.mark = ",", scientific = FALSE)),
    "decimal" = format(round(raw_value, decimal_places), nsmall = decimal_places),
    "raw" = as.character(raw_value),
    as.character(raw_value)  # default to raw if unknown type
  )

  return(list(value = formatted_value, year = year))
}
