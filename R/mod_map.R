#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import leaflet
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets actionBttn
#' @importFrom dplyr filter pull left_join %>%
#' @importFrom stringr str_detect
#' @importFrom shinycssloaders showPageSpinner hidePageSpinner
#' @importFrom glue glue
#' @importFrom qs qread
mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = 'map_container',
      
      with_spinner(
        leafletOutput(ns('map_plot'), height = '90vh', width = '100%'),
      ),
      
      # Absolute Panel -----
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 70, # 5px within map
        left = "auto",
        right = 20, # 5px within map
        bottom = "auto",
        width = 500,
        height = "auto",
        
        style = "z-index: 5001; background-color: rgba(255,255,255,0.8);
          padding: 15px; border-radius: 8px; max-width: 500; 
          box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);",        
        
        h3('Select Metrics'),

        
        div(
          class = 'button-box',
          
          # Top row
          fluidRow(
            column(
              width = 6,
              # Select resolution -----
              selectInput(
                inputId = ns('resolution'),
                label = 'Select resolution:',
                choices = c('County', 'State'),
                selected = 'County'
              )
            ),
            column(
              width = 6,
              # Select dimension -----
              selectInput(
                inputId = ns('dimension'),
                label = 'Select dimension:',
                choices = c('Economics', 'Environment', 'Production', 'Health', 'Social'),
                selected = 'Economics'
              )
            )
          ), # end top row
          
          # Bottom row
          fluidRow(
            column(
              width = 6,
              # Select metric -----
              selectizeInput(
                inputId = ns('metric'),
                label = 'Select metric:',
                choices = NULL,
                selected = NULL,
                width = '100%'
              )
            ),
            column(
              width = 6,
              # Select year -----
              selectInput(
                inputId = ns("year"),
                label = "Select year:",
                choices = NULL,
                selected = NULL,
                width = '100%'
              )
            )
          )
        ),
        
        
        
        # Metric info and full screen buttons
        fluidRow(
          column(
            width = 6,
            
            # Metric Info Button -----
            actionBttn(
              ns('show_metric_info'),
              'Metric Info',
              class = 'action-button',
              # block = TRUE,
              icon = icon('circle-info')
            )
          ),
          column(
            width = 6,
            
            # Full Screen Button -----
            actionBttn(
              ns('full_screen'),
              'Full Screen',
              # class = 'action-button',
              icon = icon('expand'),
              onclick = "openFullscreen(document.getElementById('map_container'))"
            ),
          )
        ), # fluidRow
        
        # Update Map Button -----
        actionBttn(
          ns('update_map'),
          'Update Map',
          style = 'unite',
          icon = icon('arrows-rotate')
        ),
        
        # Show Metric Info ----
        uiOutput(ns('metric_info'))
        
    ), # end absolute panel div
      
    # JS function for full screen button
    tags$script(HTML(js))
        
    ) # end full div
  ) # end tag list
}
    
#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, con, parent_input, global_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Load module data lazily when map tab is active ----
    map_data_loaded <- reactiveVal(FALSE)
    initial_map <- reactiveVal(NULL)

    observe({
      # Load map data when user goes to map tab
      req(parent_input$tabs == "map_tab")

      # Only load once
      if (!map_data_loaded()) {
        # Load spatial data from qs files
        neast_county_spatial_2021 <<- qs::qread('data/neast_county_spatial_2021.qs')
        neast_county_spatial_2024 <<- qs::qread('data/neast_county_spatial_2024.qs')
        neast_state_spatial <<- qs::qread('data/neast_state_spatial.qs')

        # Build initial map once data is loaded
        initial_map(create_base_map(neast_county_spatial_2024))

        # Set indicator to true so it doesn't happen again
        map_data_loaded(TRUE)
      }
    })

    
    # Render initial map -----
    output$map_plot <- renderLeaflet({
      req(initial_map())
      initial_map()
    })

    
    # Data filters -----
    # Filter to metrics
    available_metrics <- reactive({
      req(input$resolution, input$dimension, map_data_loaded())

      global_data$metadata %>%
        dplyr::filter(
          stringr::str_detect(Resolution, input$resolution),
          Dimension == input$dimension
        ) %>%
        dplyr::pull(Metric) %>%
        sort()
    }) %>% bindCache(input$resolution, input$dimension)

    # Get years for metric
    available_years <- reactive({
      req(input$metric, map_data_loaded())

      global_data$metadata %>%
        dplyr::filter(Metric == input$metric) %>%
        dplyr::pull(`Year Vector`) %>%
        unlist()
    }) %>%
      bindCache(input$metric)

    # Get variable name
    selected_variable <- reactive({
      req(input$metric, map_data_loaded())
      global_data$metadata %>%
        dplyr::filter(Metric == input$metric) %>%
        dplyr::pull(`Variable Name`)
    }) %>%
      bindCache(input$metric)

    # Get spatial base and join with metric data
    map_data <- reactive({
      req(
        input$metric,
        input$year,
        input$resolution,
        selected_variable(),
        map_data_loaded()
      )

      # Query metric data from database
      table <- paste0('neast_', tolower(input$resolution), '_metrics')
      query <- glue::glue(
        "SELECT *
        FROM {table}
        WHERE variable_name = '{selected_variable()}'
          AND year = {input$year}"
      )
      metric_data <- query_db(con, query)

      if (input$resolution == 'County') {
        # Choose spatial base by year (CT county boundary changes)
        spatial_base <- if (input$year >= 2023) {
          neast_county_spatial_2024
        } else {
          neast_county_spatial_2021
        }
        # Join spatial base with metric data - retain sf class with left join
        spatial_base %>%
          dplyr::left_join(metric_data, by = 'fips')
      } else {
        # State level
        neast_state_spatial %>%
          dplyr::left_join(metric_data, by = 'fips')
      }
    }) %>%
      # Cache so we don't have to reload
      bindCache(input$metric, input$year, input$resolution)

    # Update dropdowns -----
    # Update metric choices when resolution or dimension changes
    observe({
      updateSelectizeInput(
        session,
        "metric",
        choices = available_metrics(),
        server = TRUE
      )
    })

    # Update year choices when metric changes
    observe({
      updateSelectInput(
        session,
        "year",
        choices = available_years()
      )
    })
    
    
    # Metric Info -----
    show_metric_info <- reactiveVal(FALSE)
    # TODO: Why is this an if else? Should it just appear on observeEvent?
    observeEvent(input$show_metric_info, {
      
      # What does this even do
      show_metric_info(!show_metric_info())
      
      if (show_metric_info()) {
        output$metric_info <- renderUI({
          req(input$metric, input$year)

          meta <- global_data$metadata %>%
            dplyr::filter(Metric == input$metric)

          div(
            class = 'button-box',
            style = 'background-color: #fff !important;',
            tags$p(tags$strong('Metric:'), meta$Metric), tags$br(),
            tags$p(tags$strong('Definition:'), meta$Definition), tags$br(),
            tags$p(tags$strong('Units:'), meta$Units), tags$br(),
            tags$p(tags$strong('Dimension:'), meta$Dimension), tags$br(),
            tags$p(tags$strong('Indicator:'), meta$Indicator), tags$br(),
            tags$p(tags$strong('Resolution:'), meta$Resolution), tags$br(),
            tags$p(tags$strong('Source:'), tags$a(meta$Source)), tags$br(),
            tags$p(tags$strong('Citation:'), meta$Citation), tags$br()
          )

        })
      } else {
        # If show_metric_info is FALSE, clear the output or do nothing
        output$metric_info <- renderUI({
          NULL
        })
      }
      
    })
    
    
    # Update Map -----
    observeEvent(input$update_map, {
      req(map_data())

      ## Data validation -----
      # Make sure data has real values (not all NA)
      validation <- validate_map_data(
        data = map_data(),
        metric_name = input$metric,
        year = input$year,
        resolution = input$resolution
      )

      # Show notification if there's a message
      if (!is.null(validation$message)) {
        showNotification(
          HTML(validation$message),
          type = validation$type,
          duration = if (validation$type == "error") 8 else 5
        )
      }

      # Stop if validation failed
      if (!validation$valid) {
        return()
      }

      # Create color palette
      pal <- colorNumeric(
        palette = "YlGn",
        domain = map_data()$value,
        reverse = FALSE
      )

      # Get popup and label formulas based on resolution
      formulas <- get_map_formulas(input$resolution, input$metric)

      
      # Leaflet Proxy -----
      leafletProxy(
        ns("map_plot"),
        data = map_data()
      ) %>%
        clearGroup('Counties') %>%
        clearGroup('States') %>%
        clearGroup('Boundaries') %>%
        addPolygons(
          color = "black",
          weight = 1,
          smoothFactor = 1.0,
          opacity = 1.0,
          fillOpacity = 0.8,
          fillColor = ~pal(value),
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ),
          label = formulas$label,
          popup = formulas$popup,
          popupOptions = popupOptions(closeButton = FALSE),
          group = 'Boundaries'
        ) %>%
        clearControls() %>%
        addLegend(
          "bottomleft",
          pal = pal,
          values = ~value,
          title = 'Values',
          labFormat = labelFormat(prefix = " "),
          opacity = 1
        )
    })
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_1")
    
## To be copied in the server
# mod_map_server("map_1")
