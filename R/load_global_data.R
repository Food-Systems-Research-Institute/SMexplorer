#' Load Application Data
#'
#' @description Loads all required data files once at application startup
#' and pre-calculates metric options
#'
#' @return A list containing all loaded data and pre-calculated options
#' @noRd
#'
#' @import dplyr
load_global_data <- function() {

  # Load small data files (large files in duckdb, spatial as qs only in mod map)
  list(
    metadata = readRDS('data/metadata.rds'),
    fips_key = readRDS('data/fips_key.rds'),
    metric_options = readRDS('data/metric_options.rds'),
    metric_lookup = readRDS('data/metric_lookup.rds')
  )
}
