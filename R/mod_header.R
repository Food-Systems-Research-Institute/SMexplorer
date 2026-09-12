#' header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_header_ui <- function(id) {
  shinydashboard::dashboardHeader(
    title = "SM Explorer",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.github.com/Food-Systems-Research-Institute/SMexplorer",
        target = "_blank",
        `aria-label` = "SMexplorer GitHub repository",
        icon("github")
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.uvm.edu/ovpr/food-systems-research",
        target = "_blank",
        `aria-label` = "Food Systems Research Institute website",
        tags$img(
          src = "www/fsri_spirit_marker.png",
          alt = 'Food Systems Research Institute logo'
        ),
      )
    )
  )
}
    
#' header Server Functions
#'
#' @noRd 
mod_header_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_header_ui("header_1")
    
## To be copied in the server
# mod_header_server("header_1")
