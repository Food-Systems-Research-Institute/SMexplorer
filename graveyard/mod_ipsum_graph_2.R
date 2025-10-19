#' ipsum_graph_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ipsum_graph_2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # actionButton(ns("show_graph"), "Show Graph"),
    plotOutput(ns('random_plot_2'))
  )
}

#' ipsum_graph_2 Server Functions
#'
#' @noRd 
mod_ipsum_graph_2_server <- function(id, trigger){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(trigger(), {
      output$random_plot_2 <- renderPlot({
        random_ggplot()
      })
    })
  })
}

## To be copied in the UI
# mod_ipsum_graph_2_ui("ipsum_graph_2_1")
    
## To be copied in the server
# mod_ipsum_graph_2_server("ipsum_graph_2_1")
