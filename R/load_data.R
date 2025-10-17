#' Load Application Data
#'
#' @description Loads all required data files once at application startup
#' and pre-calculates metric options
#'
#' @return A list containing all loaded data and pre-calculated options
#' @noRd
#'
#' @import dplyr
load_app_data <- function() {

  # Load all data files
  load('data/js.rda')
  load('data/metadata.rda')
  load('data/metric_options.rda')

  # Return all data in a list
  list(
    js = js,
    metadata = metadata,
    metric_options = metric_options
  )
}
