filter_metrics <- function(session, axis, dat) {
  # Update dimension input
  observeEvent(input[[paste0("dimension_", axis)]], {
    filtered <- dplyr::filter(dat, dimension == input[[paste0("dimension_", axis)]])
    updateSelectInput(session, paste0("index_", axis), choices = unique(filtered$index))
  })
  
  # Update index input
  observeEvent(input[[paste0("index_", axis)]], {
    filtered <- dplyr::filter(dat, index == input[[paste0("index_", axis)]])
    updateSelectInput(session, paste0("indicator_", axis), choices = unique(filtered$indicator))
  })
  
  # Update indicator input
  observeEvent(input[[paste0("indicator_", axis)]], {
    filtered <- dplyr::filter(dat, indicator == input[[paste0("indicator_", axis)]])
    updateSelectInput(session, paste0("metric_", axis), choices = unique(filtered$variable_name))
  })
  
  # Update metric input
  observeEvent(input[[paste0("metric_", axis)]], {
    filtered <- dplyr::filter(dat, variable_name == input[[paste0("metric_", axis)]])
    updateSelectInput(
      session, 
      paste0("year_", axis), 
      choices = sort(unique(filtered$year), decreasing = TRUE)
    )
  })
}
