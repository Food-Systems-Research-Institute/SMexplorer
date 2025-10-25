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
            selected = 'Chittenden County, VT',
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
mod_details_server <- function(id, parent_input){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Construction sign -----
    observeEvent(parent_input$tabs, {
      req(parent_input$tabs == 'details_tab')
      modal_construction('details_tab')
    })
    
    
    # Lazy loading -----
    details_data_loaded <- reactiveVal(FALSE)

    observe({
      req(parent_input$tabs == "details_tab")

      # Load once
      if (!details_data_loaded()) {
        load('data/sm_data.rda', envir = parent.frame())
        details_data_loaded(TRUE)
      }
    })

    # Food insecurity reactive -----
    rval_food_insecurity <- reactive({
      req(input$search_location)

      # Get the FIPS code for the location
      if (tolower(input$select_resolution) == 'county') {
        location_fips <- sm_data$fips_key %>%
          filter(county_name == input$search_location) %>%
          pull(fips) %>%
          unique()
      } else {
        location_fips <- sm_data$fips_key %>%
          filter(state_name == input$search_location) %>%
          pull(fips) %>%
          unique() %>%
          .[nchar(.) == 2]
      }

      # Get food insecurity variable name
      food_insec_var <- sm_data$metadata %>%
        filter(grepl("food insecurity", metric, ignore.case = TRUE)) %>%
        pull(variable_name) %>%
        unique() %>%
        .[1]

      if (length(food_insec_var) > 0 && length(location_fips) > 0) {
        # Get latest value
        latest_value <- sm_data$metrics %>%
          filter(
            variable_name == food_insec_var,
            fips %in% location_fips
          ) %>%
          arrange(desc(year)) %>%
          filter(!is.na(value)) %>%
          slice(1) %>%
          pull(value)

        if (length(latest_value) > 0) {
          return(paste0(round(as.numeric(latest_value), 1), "%"))
        }
      }

      return("N/A")
    })

    # Value boxes -----
    output$food_insecurity_box <- renderValueBox({
      valueBox(
        value = rval_food_insecurity(),
        subtitle = 'Food Insecurity Rate',
        icon = icon('utensils'),
        color = 'green'
      )
    })

    output$gini_box <- renderValueBox({
      valueBox(
        value = '12',
        subtitle = 'Gini Coefficient',
        icon = icon('scale-unbalanced'),
        color = 'aqua'
      )
    })

    output$wage_box <- renderValueBox({
      valueBox(
        value = 'ABC',
        subtitle = 'Average Weekly Wage in Food Business',
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

      if (chosen_resolution == 'county') {
        location_options <- sm_data$fips_key %>%
          filter(nchar(fips) == 5) %>%
          arrange(county_name) %>%
          pull(county_name) %>%
          unique()
        default_location <- "Chittenden County, VT"
      } else {
        location_options <- sm_data$fips_key %>%
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
      metric_options <- sm_data$metrics %>%
        inner_join(sm_data$metadata, by = 'variable_name') %>%
        filter(resolution == chosen_resolution) %>%
        pull(metric) %>%
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
      req(input$search_location, input$search_metric)

      # Get the variable name from the metric
      var_name <- sm_data$metadata %>%
        filter(metric == input$search_metric) %>%
        pull(variable_name) %>%
        unique()

      # Get the FIPS code for the location
      if (tolower(input$select_resolution) == 'county') {
        location_fips <- sm_data$fips_key %>%
          filter(county_name == input$search_location) %>%
          pull(fips) %>%
          unique()
      } else {
        location_fips <- sm_data$fips_key %>%
          filter(state_name == input$search_location) %>%
          pull(fips) %>%
          unique() %>%
          .[nchar(.) == 2]
      }

      # Filter metrics data
      ts_data <- sm_data$metrics %>%
        filter(
          variable_name == var_name,
          fips %in% location_fips
        ) %>%
        select(year, value) %>%
        arrange(year) %>%
        filter(!is.na(value))

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
# mod_details_server("details_1")
