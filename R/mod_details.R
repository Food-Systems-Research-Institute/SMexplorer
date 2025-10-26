#' details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column uiOutput selectizeInput moduleServer reactiveVal observe req reactive renderUI updateSelectizeInput HTML
#' @importFrom shinydashboard box valueBox valueBoxOutput renderValueBox
#' @importFrom plotly plotlyOutput renderPlotly plot_ly layout
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_classic
#' @importFrom dplyr filter inner_join pull select arrange desc slice
#' @importFrom shinyWidgets actionBttn
#' @importFrom shinycssloaders showPageSpinner hidePageSpinner
#' @importFrom glue glue
#' @importFrom stringr str_detect
mod_details_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('boxes')),

    fluidRow(
      column(
        width = 8,

        ## Time series box -----
        box(
          width = 12,
          title = 'Metric Time Series',
          solidHeader = TRUE,
          status = 'primary',
          collapsible = TRUE,
          with_spinner(
            plotlyOutput(ns('time_series_plot'))
          )
        )
      ),

      # Right Column - Controls
      column(
        width = 4,
        ## Selection box -----
        box(
          title = 'Select Location & Metric',
          width = 12,
          status = 'primary',
          solidHeader = TRUE,
          collapsible = TRUE,

          ### Choose Resolution -----
          selectizeInput(
            inputId = ns('select_resolution'),
            label = 'Choose resolution:',
            choices = c('County', 'State'),
            selected = 'County',
            width = '100%'
          ),

          ### Search Location -----
          selectizeInput(
            inputId = ns('search_location'),
            label = 'Location:',
            choices = NULL,
            selected = 'Chittenden County',
            width = '100%'
          ),

          ### Search Metric -----
          selectizeInput(
            inputId = ns('search_metric'),
            label = 'Metric:',
            choices = NULL,
            selected = NULL,
            width = '100%'
          ),

          actionBttn(
            ns('update_plot'),
            'Show Time Series',
            style = 'unite',
            icon = icon('chart-line')
          )
        )
      )
    )
  )
}
    
#' details Server Functions
#'
#' @noRd
mod_details_server <- function(id, con, parent_input, global_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Construction sign -----
    observeEvent(parent_input$tabs, {
      req(parent_input$tabs == 'details_tab')
      modal_construction('details_tab')
    })

    # Value box data reactives -----
    # Food insecurity
    rval_food_insecurity <- reactive({
      req(input$search_location, input$select_resolution)
      get_latest_metric_value(
        con = con,
        global_data = global_data,
        variable_name = 'foodInsecurityRate',
        location = input$search_location,
        resolution = input$select_resolution,
        format_type = 'percent',
        decimal_places = 1,
        multiplier = 100  # Convert proportion to percent
      )
    })

    # Gini coefficient
    rval_gini <- reactive({
      req(input$search_location, input$select_resolution)
      get_latest_metric_value(
        con = con,
        global_data = global_data,
        variable_name = 'gini',
        location = input$search_location,
        resolution = input$select_resolution,
        format_type = 'decimal',
        decimal_places = 2
      )
    })

    # Median household income
    rval_income <- reactive({
      req(input$search_location, input$select_resolution)
      get_latest_metric_value(
        con = con,
        global_data = global_data,
        variable_name = 'medHhIncome',
        location = input$search_location,
        resolution = input$select_resolution,
        format_type = 'currency',
        decimal_places = 0
      )
    })

    # Value boxes -----
    output$food_insecurity_box <- renderValueBox({
      food_insec_data <- rval_food_insecurity()

      subtitle <- if (!is.null(food_insec_data$year)) {
        paste0('Food Insecurity Rate (', food_insec_data$year, ')')
      } else {
        'Food Insecurity Rate'
      }

      valueBox(
        value = food_insec_data$value,
        subtitle = subtitle,
        icon = icon('utensils'),
        color = 'green'
      )
    })

    output$gini_box <- renderValueBox({
      gini_data <- rval_gini()

      subtitle <- if (!is.null(gini_data$year)) {
        paste0('Gini Coefficient (', gini_data$year, ')')
      } else {
        'Gini Coefficient'
      }

      valueBox(
        value = gini_data$value,
        subtitle = subtitle,
        icon = icon('scale-unbalanced'),
        color = 'aqua'
      )
    })

    output$wage_box <- renderValueBox({
      income_data <- rval_income()

      subtitle <- if (!is.null(income_data$year)) {
        paste0('Median Household Income (', income_data$year, ')')
      } else {
        'Median Household Income'
      }

      valueBox(
        value = income_data$value,
        subtitle = subtitle,
        icon = icon('dollar-sign'),
        color = 'teal'
      )
    })

    output$boxes <- renderUI({
      tagList(
        fluidRow(
          valueBoxOutput(ns('food_insecurity_box')),
          valueBoxOutput(ns('gini_box')),
          valueBoxOutput(ns('wage_box'))
        )
      )
    })

    # Location options -----
    observe({
      req(input$select_resolution)

      chosen_resolution <- tolower(input$select_resolution)

      # Get locations from fips_key
      if (chosen_resolution == 'county') {
        location_options <- global_data$fips_key %>%
          filter(nchar(fips) == 5) %>%
          arrange(county_name) %>%
          pull(county_name) %>%
          unique()
        default_location <- "Chittenden County"
      } else {
        location_options <- global_data$fips_key %>%
          filter(nchar(fips) == 2) %>%
          arrange(state_name) %>%
          pull(state_name) %>%
          unique()
        default_location <- "Vermont"
      }

      updateSelectizeInput(
        session,
        'search_location',
        choices = c('', location_options),
        selected = default_location,
        server = TRUE
      )
    })

    # Metric options -----
    observe({
      req(input$select_resolution)

      chosen_resolution <- tolower(input$select_resolution)

      # Query metrics that have > 1 data point (proper time series)
      table <- paste0('neast_', chosen_resolution, '_metrics')
      query <- glue::glue(
        "SELECT m.variable_name, COUNT(DISTINCT m.year) as year_count
        FROM {table} m
        WHERE m.value IS NOT NULL
        GROUP BY m.variable_name
        HAVING COUNT(DISTINCT m.year) > 1"
      )

      time_series_vars <- query_db(con, query)$variable_name

      # Get metric names from metadata for these variables
      metric_options <- global_data$metadata %>%
        filter(
          `Variable Name` %in% time_series_vars,
          stringr::str_detect(Resolution, input$select_resolution)
        ) %>%
        pull(Metric) %>%
        unique() %>%
        sort()

      updateSelectizeInput(
        session,
        'search_metric',
        choices = c('', metric_options),
        selected = NULL,
        server = TRUE
      )
    })

    # Filter data for time series -----
    rval_ts_data <- reactive({
      req(input$search_location, input$search_metric, input$select_resolution)

      # Get the variable name from the metric
      var_name <- global_data$metadata %>%
        filter(Metric == input$search_metric) %>%
        pull(`Variable Name`) %>%
        unique()

      if (length(var_name) == 0) {
        return(list(data = data.frame(year = numeric(), value = numeric()), var_name = ""))
      }

      # Get FIPS code from fips_key
      if (tolower(input$select_resolution) == 'county') {
        location_fips <- global_data$fips_key %>%
          filter(county_name == input$search_location) %>%
          pull(fips) %>%
          .[1]
      } else {
        location_fips <- global_data$fips_key %>%
          filter(state_name == input$search_location) %>%
          pull(fips) %>%
          .[nchar(.) == 2] %>%
          .[1]
      }

      if (is.na(location_fips) || length(location_fips) == 0) {
        return(list(data = data.frame(year = numeric(), value = numeric()), var_name = var_name))
      }

      # Query time series data from database
      table <- paste0('neast_', tolower(input$select_resolution), '_metrics')

      # Query time series data
      query <- glue::glue(
        "SELECT year, value
        FROM {table}
        WHERE variable_name = '{var_name}'
          AND fips = '{location_fips}'
          AND value IS NOT NULL
        ORDER BY year"
      )

      ts_data <- query_db(con, query)

      list(data = ts_data, var_name = var_name)
    })

    # Time series plot -----
    output$time_series_plot <- renderPlotly({
      if (input$search_location == "" || input$search_metric == "") {
        # Empty plot if no selections
        plot_ly() %>%
          layout(
            title = list(
              text = "Select a location and metric\nto display time series",
              x = 0.5,
              y = 0.6
            ),
            xaxis = list(
              showgrid = FALSE,
              showline = TRUE,
              range = c(0, 10)
            ),
            yaxis = list(
              showgrid = FALSE,
              showline = TRUE,
              range = c(0, 10)
            )
          )
      } else {
        ts_data <- rval_ts_data()$data

        # Create plotly time series
        plot_ly(
          ts_data,
          x = ~year,
          y = ~value,
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(size = 8, color = '#154734'),
          line = list(color = '#154734', width = 2)
        ) %>%
          layout(
            title = paste0(input$search_metric, " - ", input$search_location),
            xaxis = list(title = "Year"),
            yaxis = list(title = input$search_metric),
            hovermode = 'x unified'
          )
      }
    })

  })
}
    
## To be copied in the UI
# mod_details_ui("details_1")

## To be copied in the server
# mod_details_server("details_1", con, parent_input, global_data)
