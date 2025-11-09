#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags renderText
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('welcome_text'))
  )
}
    
#' welcome Server Functions
#'
#' @noRdj
mod_welcome_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output$welcome_text <- renderUI({
      tagList(
        # Intro -----
        div(
          class = 'button-box',
          style = 'max-width: 800px; background-color: #fff;',
          tags$h2('Sustainability Metrics Explorer'),
          tags$p(
            tags$img(
              src = 'www/SMexplorer.png',
              style = 'float: right; width: 200px; margin-left: 10px; margin-bottom: 10px;',
              alt = ''
            ),
            'Welcome! This dashboard lets you explore secondary data collected for the',
            tags$a(
              'Sustainability Metrics', 
              href = 'https://www.uvm.edu/ovpr/food-systems-research/sustainability-metrics-project',
              target = '_blank'
            ),
            'project, a collaboration between USDA ARS and the',
            tags$a(
              'Food Systems Research Institute (FSRI)', 
              href = 'https://www.uvm.edu/ovpr/food-systems-research',
              target = '_blank'
            ),
            'at the University of Vermont.', 
            tags$br(),
            tags$br(),
            'For more information on secondary data collection and analyses, check out the',
            tags$a(
              'Sustainability Metrics Methods', 
              href = 'https://fsrc.w3.uvm.edu/sustainability_metrics/pages/index.html',
              target = '_blank'
            ), 
            'website and the',
            tags$a(
              'FSRI GitHub Page', 
              href = 'https://www.github.com/Food-Systems-Research-Institute/',
              target = '_blank'
            ), 
            tags$br(),
            tags$br(),
            'Navigate to the pages SMexplorer has to offer below or by using the navigation bar on the left.'
          )
        ),
        
        # Link blocks -----
        div(
          style = 'max-width: 800px; margin: 0 auto;',
          fluidRow(
            link_block(
              id = 'go_to_map_tab',
              title = 'Interactive Map',
              text = 'Explore an interactive map of metrics by county and state.',
              image = 'www/thumbnail_map.png'
            ),
            link_block(
              id = 'go_to_graph_tab',
              title = 'Metric Comparisons',
              text = 'Compare metrics among any two counties and explore correlations.',
              image = 'www/thumbnail_metric_comparison.png'
            )
          ),
          fluidRow(
            link_block(
              id = 'go_to_details_tab',
              title = 'Details',
              text = 'Dig deeper into county and state statistics and time series.',
              image = 'www/thumbnail_details.png'
            ),
            link_block(
              id = 'go_to_database_tab',
              title = 'Database',
              text = 'Explore and download secondary metrics.',
              image = 'www/thumbnail_database.png'
            )
          ),
          fluidRow(
            link_block(
              id = 'go_to_table_tab',
              title = 'Metadata',
              text = 'Explore metadata and source information.',
              image = 'www/thumbnail_table.png'
            )
          )
        )
      )
    })
  })
}
    
## To be copied in the UI
# mod_landing_ui("landing_1")
    
## To be copied in the server
# mod_landing_server("landing_1")
