#' database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList sliderInput downloadHandler tags HTML
#' @importFrom reactable reactable reactableOutput renderReactable
#' @importFrom shinyWidgets actionBttn downloadBttn
#' @importFrom dplyr %>% left_join select distinct
mod_database_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Page Header
    div(
      class = 'button-box',
      style = 'background: #fff !important;',
      tags$h2('Database Explorer', style = 'text-align: left !important;'),
      tags$p(
        'Use the fields below to query metric data from the database',
        'After querying, you can further filter and explore the data in the table.',
        'The download button will render a .csv file of the current subset of data.'
      )
    ),

    # Framework filters ----
    div(
      class = 'button-box',
      style = 'background: #fff !important;',
      tags$h4('Indicator Framework'),
      tags$p('Select one or more from each category', style = 'font-size: 0.9em; color: #666;'),

      fluidRow(
        column(
          width = 6,
          selectizeInput(
            inputId = ns('dimension'),
            label = 'Dimension:',
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            width = '100%',
            options = list(placeholder = 'Select dimension(s)...')
          )
        ),
        column(
          width = 6,
          selectizeInput(
            inputId = ns('index'),
            label = 'Index:',
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            width = '100%',
            options = list(placeholder = 'Select index(es)...')
          )
        )
      ),

      fluidRow(
        column(
          width = 6,
          selectizeInput(
            inputId = ns('indicator'),
            label = 'Indicator:',
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            width = '100%',
            options = list(placeholder = 'Select indicator(s)...')
          )
        ),
        column(
          width = 6,
          selectizeInput(
            inputId = ns('metric'),
            label = 'Metric:',
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            width = '100%',
            options = list(placeholder = 'Select metric(s)...')
          )
        )
      )
    ),

    # Geography filters ----
    div(
      class = 'button-box',
      style = 'background: #fff !important;',
      tags$h4('Geography and Year'),
      tags$p(
        'Filter by data resolution, time period, and location',
        style = 'font-size: 0.9em; color: #666;'
      ),

      fluidRow(
        column(
          width = 6,
          shinyWidgets::radioGroupButtons(
            inputId = ns('resolution'),
            label = 'Resolution:',
            choices = c('County', 'State'),
            selected = 'County',
            justified = TRUE,
            width = '100%'
          )
        ),
        column(
          width = 6,
          uiOutput(ns('year_slider_ui'))
        )
      ),

      fluidRow(
        column(
          width = 6,
          selectizeInput(
            inputId = ns('state'),
            label = 'State:',
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            width = '100%',
            options = list(placeholder = 'Select state(s)...')
          )
        ),
        column(
          width = 6,
          selectizeInput(
            inputId = ns('county'),
            label = 'County:',
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            width = '100%',
            options = list(placeholder = 'Select county/counties...')
          )
        )
      )
    ),

    # Query and Download ----
    div(
      class = 'button-box',
      style = 'background: #fff !important; text-align: center;',
      fluidRow(
        style = 'display: flex; justify-content: center; gap: 20px;',
        actionBttn(
          ns('query_button'),
          'Query Database',
          style = 'unite',
          icon = icon('database')
        ),
        shinyWidgets::downloadBttn(
          ns('download_csv'),
          'Download CSV',
          icon = icon('download'),
          style = 'unite'
        )
      )
    ),

    # Results Table ----
    div(
      class = 'button-box',
      style = 'background: #fff !important;',
      tags$h4('Results'),
      uiOutput(ns('result_summary')),
      with_spinner(
        reactable::reactableOutput(ns('results_table'))
      )
    ),

    # Inline CSS for slider which is gross
    tags$style(HTML("
      .irs-bar {
        background: #72826d !important;
        border-top: 1px solid #72826d !important;
        border-bottom: 1px solid #72826d !important;
      }
      .irs-from, .irs-to, .irs-single {
        background: #72826d !important;
      }
      .irs-from:before, .irs-to:before, .irs-single:before {
        border-top-color: #72826d !important;
      }
      .irs-handle {
        border: 2px solid #72826d !important;
        background: white !important;
      }
      .irs-handle:hover {
        background: #DAE0D7 !important;
      }
      .irs-handle.state_hover, .irs-handle:hover {
        border-color: #2a2e2d !important;
      }
    "))
  )
}

#' database Server Functions
#'
#' @noRd
mod_database_server <- function(id, con, parent_input, global_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Initialize reactive values
    query_results <- reactiveVal(NULL)

    # Year Range Slider UI (dynamic based on resolution)
    output$year_slider_ui <- renderUI({
      year_range <- get_available_years_range(global_data$metadata, input$resolution)

      sliderInput(
        inputId = ns('year_range'),
        label = 'Year Range:',
        min = year_range['min'],
        max = year_range['max'],
        value = c(year_range['min'], year_range['max']),
        step = 1,
        sep = '',
        width = '100%'
      )
    })

    # Reactive: Get current year range, handles initial null state
    current_year_range <- reactive({
      req(input$year_range)
      input$year_range
    })

    # Reactive chain for classification filters --------------------------------

    # Available Dimensions
    available_dimensions <- reactive({
      req(input$resolution)
      year_range <- if (!is.null(input$year_range)) input$year_range else NULL

      get_available_dimensions(
        metadata = global_data$metadata,
        resolution = input$resolution,
        year_range = year_range
      )
    })

    # Available Indexes
    available_indexes <- reactive({
      req(input$resolution)
      year_range <- if (!is.null(input$year_range)) input$year_range else NULL

      get_available_indexes(
        metadata = global_data$metadata,
        resolution = input$resolution,
        year_range = year_range,
        dimensions = input$dimension
      )
    })

    # Available indicators
    available_indicators <- reactive({
      req(input$resolution)
      year_range <- if (!is.null(input$year_range)) input$year_range else NULL

      get_available_indicators(
        metadata = global_data$metadata,
        resolution = input$resolution,
        year_range = year_range,
        dimensions = input$dimension,
        indexes = input$index
      )
    })

    # Available metrics
    available_metrics <- reactive({
      req(input$resolution)
      year_range <- if (!is.null(input$year_range)) input$year_range else NULL

      get_available_metrics(
        metadata = global_data$metadata,
        resolution = input$resolution,
        year_range = year_range,
        dimensions = input$dimension,
        indexes = input$index,
        indicators = input$indicator
      )
    })

    # Get variable names from selected metrics (can be NULL/empty)
    selected_variable_names <- reactive({
      if (is.null(input$metric) || length(input$metric) == 0) {
        return(NULL)
      }
      get_variable_names(global_data$metadata, input$metric)
    })

    
    # Geography filters ---------------------------------------------------------

    # Available states (works independently of metrics)
    available_states <- reactive({
      req(input$resolution, current_year_range())

      get_available_states(
        con = con,
        resolution = input$resolution,
        variable_names = selected_variable_names(),  # Can be NULL
        year_range = current_year_range()
      )
    })

    # Available counties (also independent of framework)
    available_counties <- reactive({
      req(input$state, current_year_range())

      get_available_counties(
        con = con,
        states = input$state,
        variable_names = selected_variable_names(),  # Can be NULL
        year_range = current_year_range()
      )
    })

    
    # Update filter dropdowns ---------------------------------------------------

    # Update dimension choices (triggered by resolution or year range)
    observeEvent(list(input$resolution, input$year_range), {
      choices <- available_dimensions()
      # Preserve selection if still valid, otherwise clear
      current_selection <- isolate(input$dimension)
      new_selection <- if (!is.null(current_selection)) {
        intersect(current_selection, choices)
      } else {
        NULL
      }

      updateSelectizeInput(
        session,
        'dimension',
        choices = choices,
        selected = new_selection,
        server = FALSE
      )
    }, ignoreNULL = FALSE)

    # Update index choices (triggered by dimension selection)
    observeEvent(list(input$dimension, input$resolution, input$year_range), {
      choices <- available_indexes()
      current_selection <- isolate(input$index)
      new_selection <- if (!is.null(current_selection)) {
        intersect(current_selection, choices)
      } else {
        NULL
      }

      updateSelectizeInput(
        session,
        'index',
        choices = choices,
        selected = new_selection,
        server = FALSE
      )
    }, ignoreNULL = FALSE)

    # Update indicator choices (triggered by index selection)
    observeEvent(list(input$dimension, input$index, input$resolution, input$year_range), {
      choices <- available_indicators()
      current_selection <- isolate(input$indicator)
      new_selection <- if (!is.null(current_selection)) {
        intersect(current_selection, choices)
      } else {
        NULL
      }

      updateSelectizeInput(
        session,
        'indicator',
        choices = choices,
        selected = new_selection,
        server = FALSE
      )
    }, ignoreNULL = FALSE)

    # Update metric choices (triggered by indicator selection)
    observeEvent(list(input$dimension, input$index, input$indicator, input$resolution, input$year_range), {
      choices <- available_metrics()
      current_selection <- isolate(input$metric)
      new_selection <- if (!is.null(current_selection)) {
        intersect(current_selection, choices)
      } else {
        NULL
      }

      updateSelectizeInput(
        session,
        'metric',
        choices = choices,
        selected = new_selection,
        server = FALSE
      )
    }, ignoreNULL = FALSE)

    # Update state choices (works independently)
    observeEvent(list(input$resolution, input$year_range, input$metric), {
      req(input$resolution, input$year_range)

      states <- available_states()
      current_selection <- isolate(input$state)
      new_selection <- if (!is.null(current_selection)) {
        intersect(current_selection, states)
      } else {
        NULL
      }

      updateSelectizeInput(
        session,
        'state',
        choices = states,
        selected = new_selection,
        server = FALSE
      )
    }, ignoreNULL = FALSE)

    # Update county choices (only when states selected)
    observeEvent(list(input$state, input$metric, input$year_range), {
      req(input$state)

      counties <- available_counties()
      current_selection <- isolate(input$county)
      new_selection <- if (!is.null(current_selection)) {
        intersect(current_selection, counties)
      } else {
        NULL
      }

      updateSelectizeInput(
        session,
        'county',
        choices = counties,
        selected = new_selection,
        server = FALSE
      )
    }, ignoreNULL = FALSE)

    
    # Query Database Button -----------------------------------------------------
    observeEvent(input$query_button, {
      req(current_year_range())

      # Determine which metrics to query based on filters
      # Priority: explicit metrics > classification filters > all metrics
      if (!is.null(input$metric) && length(input$metric) > 0) {
        # User explicitly selected metrics - use those
        var_names <- get_variable_names(global_data$metadata, input$metric)
      } else if (!is.null(input$dimension) && length(input$dimension) > 0 ||
                 !is.null(input$index) && length(input$index) > 0 ||
                 !is.null(input$indicator) && length(input$indicator) > 0) {
        # No explicit metrics, but classification filters selected
        # Get all metrics matching the current filters
        matching_metrics <- available_metrics()

        if (length(matching_metrics) > 0) {
          var_names <- get_variable_names(global_data$metadata, matching_metrics)
        } else {
          # No metrics match the filters
          showNotification(
            'No metrics match the selected filters',
            type = 'error',
            duration = 5
          )
          return()
        }
      } else {
        # No filters at all - query everything
        var_names <- NULL
      }

      # Warn if querying all data (no filters)
      if (is.null(var_names) && is.null(input$state) && is.null(input$county)) {
        showNotification(
          HTML('Warning: Querying all metrics for all locations.<br/>This may return a large dataset and take some time.'),
          type = 'warning',
          duration = 5
        )
      }

      # Show loading notification
      showNotification(
        'Querying database...',
        id = 'query_loading',
        duration = NULL,
        closeButton = FALSE,
        type = 'message'
      )

      # Query the database
      results <- query_metric_data_bulk(
        con = con,
        variable_names = var_names,
        resolution = input$resolution,
        year_range = current_year_range(),
        states = input$state,
        counties = input$county
      )

      # Remove loading notification
      removeNotification('query_loading')

      # Check if results are empty
      if (nrow(results) == 0) {
        showNotification(
          'No data found for the selected filters',
          type = 'warning',
          duration = 5
        )
        query_results(NULL)
        return()
      }

      # Join with fips_key to get readable location names
      fips_key <- global_data$fips_key
      results <- results %>%
        dplyr::left_join(fips_key, by = 'fips')

      # Join with metadata to add classification columns
      metadata_subset <- global_data$metadata %>%
        dplyr::select(`Variable Name`, Metric, Dimension, Index, Indicator, Units) %>%
        dplyr::distinct()

      results <- results %>%
        dplyr::left_join(
          metadata_subset,
          by = c('variable_name' = 'Variable Name')
        )

      # Reorder columns for better display
      results <- results %>%
        dplyr::select(
          state_name, county_name, fips, year,
          Dimension, Index, Indicator, Metric,
          value, Units
        )

      # Store results
      query_results(results)

      # Success notification
      showNotification(
        paste0('Found ', nrow(results), ' rows'),
        type = 'message',
        duration = 3
      )
    })

    
    # Display Results Summary ---------------------------------------------------
    output$result_summary <- renderUI({
      results <- query_results()

      if (is.null(results)) {
        tags$p('No results yet. Configure filters and click "Query Database".',
               style = 'color: #666; font-style: italic;')
      } else {
        tags$p(
          paste0(
            'Displaying ', nrow(results), ' rows | ',
            length(unique(results$Metric)), ' unique metric(s) | ',
            length(unique(results$fips)), ' location(s) | ',
            length(unique(results$year)), ' year(s)'
          ),
          style = 'font-weight: bold; color: #2c5f2d;'
        )
      }
    })

    
    # Render Results Table ------------------------------------------------------
    output$results_table <- reactable::renderReactable({
      results <- query_results()

      if (is.null(results)) {
        return(NULL)
      }

      reactable::reactable(
        results,
        sortable = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        pagination = TRUE,
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        defaultPageSize = 25,
        pageSizeOptions = c(10, 25, 50, 100, 500),
        showPageSizeOptions = TRUE,
        style = list(fontSize = "13px"),
        compact = TRUE,
        defaultColDef = reactable::colDef(
          minWidth = 100
        ),
        columns = list(
          state_name = reactable::colDef(
            name = "State",
            minWidth = 120,
            sticky = "left"
          ),
          county_name = reactable::colDef(
            name = "County",
            minWidth = 150
          ),
          fips = reactable::colDef(
            name = "FIPS",
            minWidth = 80
          ),
          year = reactable::colDef(
            name = "Year",
            minWidth = 80
          ),
          Dimension = reactable::colDef(
            name = "Dimension",
            minWidth = 120
          ),
          Index = reactable::colDef(
            name = "Index",
            minWidth = 120
          ),
          Indicator = reactable::colDef(
            name = "Indicator",
            minWidth = 150
          ),
          Metric = reactable::colDef(
            name = "Metric",
            minWidth = 200
          ),
          value = reactable::colDef(
            name = "Value",
            minWidth = 100,
            format = reactable::colFormat(digits = 2)
          ),
          Units = reactable::colDef(
            name = "Units",
            minWidth = 100
          )
        )
      )
    })

    # Download CSV Handler ------------------------------------------------------
    output$download_csv <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
        paste0("smexplorer_data_", timestamp, ".csv")
      },
      content = function(file) {
        # Check if user has already queried data
        results <- query_results()

        if (is.null(results)) {
          # No query yet - use current filter state to determine what to download
          # Determine which metrics to download based on filters
          if (!is.null(input$metric) && length(input$metric) > 0) {
            var_names <- get_variable_names(global_data$metadata, input$metric)
          } else if (!is.null(input$dimension) && length(input$dimension) > 0 ||
                     !is.null(input$index) && length(input$index) > 0 ||
                     !is.null(input$indicator) && length(input$indicator) > 0) {
            # Get all metrics matching the current filters
            matching_metrics <- available_metrics()
            var_names <- get_variable_names(global_data$metadata, matching_metrics)
          } else {
            var_names <- NULL  # All metrics
          }

          # Show appropriate notification
          if (is.null(var_names) && is.null(input$state) && is.null(input$county)) {
            showNotification(
              'Downloading entire dataset (all metrics, all locations)...',
              id = 'download_loading',
              duration = NULL,
              closeButton = FALSE,
              type = 'message'
            )
          } else {
            showNotification(
              'Downloading filtered data...',
              id = 'download_loading',
              duration = NULL,
              closeButton = FALSE,
              type = 'message'
            )
          }

          # Query from database with current filters
          results <- query_metric_data_bulk(
            con = con,
            variable_names = var_names,
            resolution = input$resolution,
            year_range = current_year_range(),
            states = input$state,
            counties = input$county
          )

          # Join with fips_key
          fips_key <- global_data$fips_key
          results <- results %>%
            dplyr::left_join(fips_key, by = 'fips')

          # Join with metadata
          metadata_subset <- global_data$metadata %>%
            dplyr::select(`Variable Name`, Metric, Dimension, Index, Indicator, Units) %>%
            dplyr::distinct()

          results <- results %>%
            dplyr::left_join(
              metadata_subset,
              by = c('variable_name' = 'Variable Name')
            ) %>%
            dplyr::select(
              state_name, county_name, fips, year,
              Dimension, Index, Indicator, Metric,
              value, Units
            )

          removeNotification('download_loading')

          showNotification(
            paste0('Downloaded ', nrow(results), ' rows'),
            type = 'message',
            duration = 3
          )
        } else {
          # Use filtered results
          showNotification(
            paste0('Downloading ', nrow(results), ' filtered rows...'),
            type = 'message',
            duration = 2
          )
        }

        # Write to CSV
        write.csv(results, file, row.names = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
