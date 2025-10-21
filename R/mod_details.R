#' details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_details_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # uiOutput(ns('disclaimer')),
    uiOutput(ns('boxes'))
  )
}
    
#' details Server Functions
#'
#' @noRd 
mod_details_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$disclaimer <- renderUI({
      tags$p('Welcome! This page is under construction and will be here soon.')
    })
    
    output$food_insecurity_box <- renderValueBox({
      valueBox(
        value = '100 million',
        subtitle = 'Subtitle here',
        icon = icon('list'),
        color = 'green'
      )
    })
    
    output$gini_box <- renderValueBox({
      valueBox(
        value = '12',
        subtitle = 'Subtitle here',
        icon = icon('list'),
        color = 'aqua'
      )
    })
    
    output$wage_box <- renderValueBox({
      valueBox(
        value = 'ABC',
        subtitle = 'Subtitle here',
        icon = icon('list'),
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
    
    
  })
}
    
## To be copied in the UI
# mod_details_ui("details_1")
    
## To be copied in the server
# mod_details_server("details_1")
