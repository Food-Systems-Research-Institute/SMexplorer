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
#' @noRd 
mod_welcome_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output$welcome_text <- renderUI({
      tags$p(
        'Welcome! This page is under construction and will be here soon.',
        'Really it\'s only here to buy time for other pages to load before you click on them though.',
        'Navigate to other pages using the sidebar on the left.'
      )
    })
  })
}
    
## To be copied in the UI
# mod_landing_ui("landing_1")
    
## To be copied in the server
# mod_landing_server("landing_1")
