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
  load('data/dat.rda')
  load('data/counties_2021.rda')
  load('data/counties_2024.rda')
  load('data/js.rda')
  load('data/sm_data.rda')
  source('R/filter_fips.R')

  # Metric options
  metric_options <- sm_data$metrics %>%
    inner_join(sm_data$metadata, by = 'variable_name') %>%
    # Pulling out some problematic layers for now, revisit this []
    filter(str_detect(metric, '^Acid|^Acre', negate = TRUE)) %>%
    pull(metric) %>%
    unique()

  # Reorder metrics, put NAICS last
  metric_options <- c(
    sort(metric_options[!grepl("NAICS", metric_options)]),
    sort(metric_options[grepl("NAICS", metric_options)])
  )

  # Return all data in a list
  list(
    dat = dat,
    counties_2021 = counties_2021,
    counties_2024 = counties_2024,
    js = js,
    sm_data = sm_data,
    metric_options = metric_options
  )
}
