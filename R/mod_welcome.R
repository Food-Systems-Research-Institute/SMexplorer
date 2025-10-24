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
              style = 'float: right; width: 250px; margin-left: 10px; margin-bottom: 10px;',
              alt = ''
            ),
            'Welcome! This page pretends to be a nice welcome page that directs you to where you want to go.',
            'Really it\'s only here to buy time for other pages to load before you click on them though.',
            tags$br(),
            tags$br(),
            'This dashboard helps you explore secondary data collected for the Sustainability Metrics project of the',
            tags$a('Food Systems Research Institute (FSRI).', href = 'https://www.uvm.edu/ovpr/food-systems-research'),
            'For more information, check out the',
            tags$a('Sustainability Metrics Methods', href = 'https://fsrc.w3.uvm.edu/sustainability_metrics/pages/index.html'),
            'website.',
            tags$br(),
            tags$br(),
            'Explore the pages SMexplorer has to offer below or by using the navigation bar on the left.'
          )
        ),
        
        # Links -----
        div(
          style = 'max-width: 800px; margin: 0 auto;',
          fluidRow(
            column(
              width = 6,
              div(
                class = 'button-box',
                style = 'background-color: #fff;',
                fluidRow(
                  style = 'display: flex; align-items: stretch; min-height: 100px;',
                  column(
                    width = 6,
                    style = 'display: flex; justify-content: center; align-items: center; text-align: center;',
                    tags$span(
                      style = 'display: inline-block; vertical-align: middle; line-height: normal;',
                      'Explore an interactive map of metrics by county and state.')
                  ),
                  column(
                    width = 6,
                    style = 'display: flex; justify-content: center; align-items: center;',
                    tags$img(
                      src = 'www/thumbnail_map.png',
                      style = 'height: 100px; margin: 0px;',
                      alt = ''
                    )
                  )
                )
              )
            ),
            column(
              width = 6,
              div(
                class = 'button-box',
                style = 'background-color: #fff;',
                fluidRow(
                  style = 'display: flex; align-items: stretch; min-height: 100px;',
                  column(
                    width = 6,
                    style = 'display: flex; justify-content: center; align-items: center; text-align: center;',
                    tags$span(
                      style = 'display: inline-block; vertical-align: middle; line-height: normal;',
                      'Compare metrics among any two counties and explore correlations.')
                  ),
                  column(
                    width = 6,
                    style = 'display: flex; justify-content: center; align-items: center;',
                    tags$img(
                      src = 'www/thumbnail_metric_comparison.png',
                      style = 'height: 100px; margin: 0px;',
                      alt = ''
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              div(
                class = 'button-box',
                style = 'background-color: #fff;',
                tags$p('Dig into county specifics with the details page.')
              )
            ),
            column(
              width = 6,
              div(
                class = 'button-box',
                style = 'background-color: #fff;',
                tags$p('Explore metadata and download a table of source information.')
              )
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
