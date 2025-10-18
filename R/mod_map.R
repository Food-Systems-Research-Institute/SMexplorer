#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' @import leaflet
#' @import mapview
#' @import sf
#' @import leaflet.extras
#' @import shinyWidgets
#' @import dplyr
#' @import shinycssloaders
#' @import stringr
mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(
      id = 'map_container',
      
      # Leaflet output -----
      withSpinner(
        type = 6,
        color = '#154734',
        caption = HTML('Loading Map...'),
        leafletOutput(ns('map_plot'), height = '90vh', width = '100%')
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

        
        # Select resolution -----
        selectInput(
          inputId = ns('resolution'),
          label = 'Select resolution:',
          choices = c('County', 'State'),
          selected = NULL,
          width = '100%'
        ),
        
        # Select metric -----
        selectizeInput(
          inputId = ns('metric'),
          label = 'Select Metric:',
          choices = NULL,
          selected = NULL,
          width = '100%',
          multiple = FALSE
        ),
        
        # Select year -----
        selectInput(
          inputId = ns("year"),
          label = "Select Year:",
          choices = NULL,
          selected = NULL,
          width = '100%'
        ),
        
        # Metric info and full screen buttons
        fluidRow(
          column(
            width = 6,
            
            # Metric Info Button -----
            actionBttn(
              ns('show_metric_info'),
              'Metric Info',
              # class = 'action-button',
              # block = TRUE,
              icon = icon('info')
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
      
    )
  )
}
    
#' map Server Functions
#'
#' @param app_data Pre-loaded application data from load_app_data()
#'
#' @noRd
mod_map_server <- function(id, app_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Load module data ----
    load('data/neast_county_metrics.rda')
    load('data/neast_state_metrics.rda')
    
    load('data/neast_counties_2021.rda')
    load('data/neast_counties_2024.rda')
    
    load('data/neast_county_spatial_2021.rda')
    load('data/neast_county_spatial_2024.rda')
    load('data/neast_state_spatial.rda')
    
    load('data/metadata.rda')
    
    # load('data/neast_states.rda') # What is this
    
    # Use pre-loaded data instead of loading here
    sm_data <- app_data$sm_data
    metric_options <- app_data$metric_options

    # Metric Dropdown -----
    updateSelectizeInput(
      session,
      'metric',
      choices = metric_options
    )

    # Render initial map -----
    output$map_plot <- renderLeaflet({

      # Initial Map -----
      leaflet(neast_county_spatial_2024) %>%
        addProviderTiles(
          providers$OpenStreetMap.Mapnik,
          group = 'OpenStreetMap.Mapnik'
        ) %>%
        addProviderTiles(
          providers$USGS.USImagery,
          group = 'USGS.USImagery'
        ) %>%
        addProviderTiles(
          providers$CartoDB.Positron,
          group = 'CartoDB.Positron'
        ) %>%
        addPolygons(
          color = "black",
          weight = 1,
          smoothFactor = 0.8,
          opacity = 0.7,
          fillOpacity = 0.5,
          fillColor = 'lightgray',
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ),
          popup = custom_popup,
          popupOptions = popupOptions(closeButton = FALSE),
          label = ~county_name,
          group = 'Counties'
        ) %>%
        addLayersControl(
          baseGroups = c(
            'CartoDB.Positron',
            'OpenStreetMap.Mapnik',
            'USGS.USImagery'
          ),
          overlayGroups = c('Counties'),
          options = layersControlOptions(collapsed = TRUE),
          position = 'topleft'
        ) %>% 
        # leaflet.extras::addResetMapButton() %>% 
        # Default view to Gulf of Maine, puts map on left, panel on right
        leaflet::setView(
          lng = -67.44604,
          lat = 42.58503,
          zoom = 6
        )
    })
    
    # Update metric field -----
    # Based on resolution input
    observeEvent(input$resolution, {
      req(input$resolution)
      metric_options <- metadata %>%
        dplyr::filter(stringr::str_detect(Resolution, input$resolution)) %>% 
        dplyr::pull(Metric) %>% 
        sort()

      updateSelectInput(
        session,
        "metric",
        choices = metric_options
      )
    })
    
    # Update year field -----
    # Based on metric input
    # TODO: avoid this str split here. get years better
    observeEvent(input$metric, {
      req(input$metric)
      year_options <- metadata %>%
        dplyr::filter(Metric == input$metric) %>% 
        dplyr::pull(Year) %>% 
        stringr::str_split_1(', ') %>% 
        sort(decreasing = TRUE)

      updateSelectInput(
        session,
        "year",
        choices = year_options
      )
    })
    
    
    # Metric Info -----
    # Show Metric Button -----
    show_metric_info <- reactiveVal(FALSE)
    # TODO: Why is this an if else? Should it just appear on observeEvent?
    observeEvent(input$show_metric_info, {
      
      # What does this even do
      show_metric_info(!show_metric_info())
      
      if (show_metric_info()) {
        output$metric_info <- renderUI({
          req(input$metric, input$year)
          
          meta <- metadata %>% 
            filter(Metric == input$metric)
          
          div(
            class = 'button-box',
            style = 'background-color: #fff !important;',
            tags$h4('Metric:', input$metric),
            tags$p(tags$strong('Definition:'), meta$Definition), tags$br(),
            tags$p(tags$strong('Units:'), meta$Units), tags$br(),
            tags$p(tags$strong('Dimension:'), meta$Dimension), tags$br(),
            tags$p(tags$strong('Index:'), meta$Index), tags$br(),
            tags$p(tags$strong('Indicator:'), meta$Indicator), tags$br(),
            tags$p(tags$strong('Resolution:'), meta$Resolution), tags$br(),
            tags$p(tags$strong('Updates:'), meta$Updates), tags$br(),
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
    
    
    # Update Map (Prep) -----
    observeEvent(input$update_map, {
      req(input$metric, input$year)

      # Get corresponding variable_name
      # TODO: Could just use smaller lookup table here if indeed we need it
      chosen_variable <- metadata %>% 
        filter(Metric == input$metric) %>% 
        pull('Variable Name')
      
      # Join with counties or states depending on resolution
      # Also choose county map depending on year (CT discrepancies)
      if (input$resolution == 'County') {
        # Ideally would join starting with sf object. Rethink this at some point
        updated_metrics <- neast_county_metrics %>% 
            dplyr::filter(
              variable_name == chosen_variable,
              year == input$year
            )
        if (input$year >= 2023) {
          updated_metrics <- updated_metrics %>% 
            dplyr::right_join(neast_county_spatial_2024, by = 'fips')
        } else if (input$year <= 2022) {
          updated_metrics <- updated_metrics %>% 
            dplyr::right_join(neast_county_spatial_2021, by = 'fips')
        }
      } else if (input$resolution == 'State') {
        # Join, starting with spatial to keep it as sf object
        updated_metrics <- neast_state_spatial %>% 
          left_join(
            neast_state_metrics %>% 
            dplyr::filter(
              variable_name == chosen_variable,
              year == input$year
            )
          )
      }
     
      # Popups and palette
      custom_popup <- function(county_name,
                               state_name,
                               variable_name,
                               value) {
        # If the metric is at state level, county_name will be NA
        # In this case, use state name instead
        area_name <- ifelse(is.na(county_name), state_name, county_name)
        paste0(
          "<div style='text-align: center;'>",
          "<b>", area_name, "</b><br>",
          "<strong>", variable_name, ":</strong> ", round(value, 2)
        )
      }
      
      pal <- colorNumeric(
        palette = "YlGn",
        domain = updated_metrics$value,
        reverse = FALSE
      )
      
      # Make sure updated_dat is an sf object after joins
      if (!'sf' %in% class(updated_metrics)) {
        updated_metrics <- st_as_sf(updated_metrics)
      }

      
      # LeafletProxy -----
      leafletProxy(
        ns("map_plot"), 
        data = updated_metrics
      ) %>%
        clearGroup('Counties') %>%
        addPolygons(
          color = "black",
          weight = 1, 
          smoothFactor = 0.5,
          opacity = 1.0, 
          fillOpacity = 0.8,
          fillColor = ~pal(updated_metrics$value),
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ),
          popup = ~custom_popup(county_name, state_name, variable_name, value),
          popupOptions = popupOptions(closeButton = FALSE),
          label = ~county_name,
          group = 'Counties'
        ) %>% 
        clearControls() %>% 
        addLegend(
          "bottomleft",
          pal = pal,
          values = ~value,
          # title = ~`Axis Name`[1], # Not working because we don't have it handy
          title = 'Metric Values',
          labFormat = labelFormat(prefix = " "),
          # labFormat = labelFormat(prefix = "$"),
          opacity = 1
        )
    })
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_1")
    
## To be copied in the server
# mod_map_server("map_1")
