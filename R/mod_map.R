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
        width = 400,
        height = "auto",
        
        style = "z-index: 5001; background-color: rgba(255,255,255,0.8);
          padding: 15px; border-radius: 8px; max-width: 300; 
          box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);",        
        
        h3('Select Metrics'),

        
        # Search metric -----
        selectizeInput(
          inputId = ns('metric'),
          label = 'Select Metric:',
          choices = NULL,
          selected = NULL,
          width = '100%',
          multiple = FALSE
        ),
        
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
            actionButton(
              ns('show_metric_info'),
              'Metric Info',
              class = 'action-button',
              block = TRUE,
              icon = icon('info')
            )
          ),
          column(
            width = 6,
            
            # Full Screen Button -----
            actionButton(
              ns('full_screen'),
              'Full Screen',
              class = 'action-button',
              icon = icon('expand'),
              onclick = "openFullscreen(document.getElementById('map_container'))"
            ),
          )
        ), # fluidRow
        
        # Update Map Button -----
        actionButton(
          ns('update_map'),
          'Update Map',
          class = 'action-button',
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
#' @noRd
mod_map_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Load data ONCE at module initialization -----
    load('data/dat.rda')
    load('data/counties_2021.rda')
    load('data/counties_2024.rda')
    load('data/js.rda')
    load('data/sm_data.rda')
    source('R/filter_fips.R')

    # Calculate metric options ONCE -----
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

    # Update UI with metric options once on initialization -----
    updateSelectInput(
      session,
      'metric',
      choices = metric_options
    )

    # Render initial map -----
    output$map_plot <- renderLeaflet({
      
      # Baseline data to make map
      init_data <- sm_data$ne_counties_2024 %>% 
        left_join(sm_data$fips_key, by = 'fips')
      
      # Prep popup and palette
      custom_popup <- ~paste0(
        "<div style='text-align: center;'><b>", county_name, "</b></div>",
        "<strong>Land Area:</strong> ", round(aland / 1000000, 1), " sq km<br>",
        "<strong>Water Area:</strong> ", round(awater / 1000000, 1), " sq km<br>"
      )
      # county_palette <- colorFactor(
      #   "viridis",
      #   initial_dat$county_name
      # )
      
      
      # centroids <- st_centroid(initial_dat)
      # centroid_coords <- data.frame(
      #   county_name = centroids$county_name,
      #   lng = st_coordinates(centroids)[, 1],
      #   lat = st_coordinates(centroids)[, 2]
      # )
      
      # Initial Map -----
      leaflet(init_data) %>% 
        addProviderTiles(
          providers$OpenStreetMap.Mapnik, 
          group = 'OpenStreetMap'
        ) %>%
        addProviderTiles(
          providers$Stadia.AlidadeSmooth, 
          group = 'Stadia AlidadeSmooth'
        ) %>%
        addProviderTiles(
          providers$Stadia.StamenTerrain, 
          group = 'Stadia.StamenTerrain'
        ) %>%
        addProviderTiles(
          providers$Stadia.Outdoors, 
          group = 'Stadia.Outdoors'
        ) %>%
        addProviderTiles(
          providers$Stadia.StamenWatercolor, 
          group = 'Stadia.StamenWatercolor'
        ) %>%
        addProviderTiles(
          providers$USGS.USImagery, 
          group = 'USGS.USImagery'
        ) %>% 
        addPolygons(
          color = "black",
          weight = 1, 
          smoothFactor = 0.5,
          opacity = 1.0, 
          fillOpacity = 0.8,
          # fillColor = ~county_palette(initial_dat$county_name),
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
        # addLabelOnlyMarkers(
        #   data = centroid_coords,
        #   label = ~county_name,
        #   lng = ~lng,
        #   lat = ~lat,
        #   labelOptions = labelOptions(
        #     noHide = TRUE, 
        #     direction = "auto", 
        #     textsize = "10px",
        #     textOnly = TRUE
        #   )
        # ) %>%
        addLayersControl(
          baseGroups = c(
            'OpenStreetMap',
            'Stadia AlidadeSmooth',
            'Stadia.StamenTerrain',
            'Stadia.StamenWatercolor',
            'Stadia.Outdoors',
            'USGS.USImagery'
          ), 
          overlayGroups = c('Counties'),
          options = layersControlOptions(collapsed = TRUE),
          position = 'topleft'
        )
        # addFullscreenControl()
    })
    
    
    # Update year field -----
    observeEvent(input$metric, {
      req(input$metric)
      year_options <- sm_data$metadata %>%
        dplyr::filter(metric == input$metric) %>% 
        pull(year) %>% 
        str_split_1(', ') %>% 
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
    observeEvent(input$show_metric_info, {
      
      show_metric_info(!show_metric_info())
      
      if (show_metric_info()) {
        output$metric_info <- renderUI({
          req(input$metric, input$year)
          
          meta <- sm_data$metadata %>% 
            filter(metric == input$metric) %>% 
            mutate(across(
              c(metric, dimension, index, indicator, resolution), 
              ~ str_to_sentence(.x)
            ))
          
          div(
            class = 'button-box',
            style = 'background-color: #fff !important;',
            HTML(
              '<h4><b>Metric: </b>', input$metric, '</h4>',
              '<p><b>Definition:</b> ', meta$definition, '<br>',
              '<b>Dimension:</b> ', meta$dimension, '<br>',
              '<b>Index:</b> ', meta$index, '<br>',
              '<b>Indicator:</b> ', meta$indicator, '<br>',
              '<b>Resolution:</b> ', meta$resolution, '<br>',
              '<b>Updates:</b> ', meta$updates, '<br>',
              '<b>Source: </b><a href="', meta$url, '">', meta$source, '</a><br>',
              '<b>Citation:</b> ', meta$citation, '</p>'
            )
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
      chosen_variable <- sm_data$metadata %>% 
        filter(metric == input$metric) %>% 
        pull(variable_name)
      
      # Filter dataset based on user choices
      # Also join to metadata to get axis names and metric names
      updated_dat <- sm_data$metrics %>% 
        dplyr::filter(
          variable_name == chosen_variable,
          year == input$year
        ) %>% 
        mutate(value = as.numeric(value)) %>% 
        left_join(select(sm_data$metadata, -year), by = 'variable_name')
      
      # Get resolution of metric
      res <- sm_data$metadata %>% 
        filter(variable_name == chosen_variable) %>% 
        pull(resolution)
      
      # Join with counties or states depending on resolution
      # Also choose county map depending on year (CT Discrancies)
      if (res == 'county') {
        if (input$year >= 2023) {
          updated_dat <- updated_dat %>%
            dplyr::right_join(sm_data$ne_counties_2024, by = 'fips')
        } else if (input$year <= 2022) {
          updated_dat <- updated_dat %>%
            dplyr::right_join(sm_data$ne_counties_2021, by = 'fips')
        }
      } else if (res == 'state') {
        updated_dat <- updated_dat %>% 
          dplyr::right_join(sm_data$ne_states_2024, by = 'fips')
      }
      
      # Add county name
      updated_dat <- updated_dat %>% 
        left_join(sm_data$fips_key, by = 'fips')
      
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
        domain = updated_dat$value,
        reverse = FALSE
      )
      
      # Make sure updated_dat is an sf object after joins
      updated_dat <- st_as_sf(updated_dat)

      
      # LeafletProxy -----
      leafletProxy(
        ns("map_plot"), 
        data = updated_dat
      ) %>%
        clearGroup('Counties') %>%
        addPolygons(
          color = "black",
          weight = 1, 
          smoothFactor = 0.5,
          opacity = 1.0, 
          fillOpacity = 0.75,
          fillColor = ~pal(updated_dat$value),
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
          title = ~axis_name[1],
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
