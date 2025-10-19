#' ipsum_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' @import shinipsum ggplot2
mod_ipsum_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # actionButton(ns("show_graph"), "Show Graph"),
    plotOutput(ns('random_plot'))
  )
}
 
#' ipsum_graph Server Functions
#'
#' @noRd 
mod_ipsum_graph_server <- function(id, trigger){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(trigger(), {
      output$random_plot <- renderPlot({
        random_ggplot()
      })
    })
  })
}
    
## To be copied in the UI
# mod_ipsum_graph_ui("ipsum_graph_1")
    
## To be copied in the server
# mod_ipsum_graph_server("ipsum_graph_1")
