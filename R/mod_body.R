#' body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinydashboard dashboardBody tabItems tabItem
mod_body_ui <- function(id) {
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "welcome_tab",
        mod_welcome_ui("welcome")
      ),
      tabItem(
        tabName = "map_tab",
        mod_map_ui("map_plot")
      ),
      tabItem(
        tabName = "graph_tab",
        mod_graph_ui("graph")
      ),
      tabItem(
        tabName = "details_tab",
        mod_details_ui("details")
      ),
      tabItem(
        tabName = "database_tab",
        mod_database_ui("database")
      ),
      tabItem(
        tabName = "table_tab",
        mod_table_ui("table")
      )
    )
  )
}
    
#' body Server Functions
#'
#' @noRd 
mod_body_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_body_ui("body_1")
    
## To be copied in the server
# mod_body_server("body_1")
