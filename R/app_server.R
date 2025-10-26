#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs onclick
#' @importFrom shinydashboard updateTabItems
#' @noRd
app_server <- function(input, output, session) {
  # Turn link_block buttons on welcome module into links to tabs
  shinyjs::onclick("go_to_map_tab", {
    updateTabItems(session, "tabs", "map_tab")
  })
  shinyjs::onclick("go_to_graph_tab", {
    updateTabItems(session, "tabs", "graph_tab")
  })
  shinyjs::onclick("go_to_details_tab", {
    updateTabItems(session, "tabs", "details_tab")
  })
  shinyjs::onclick("go_to_table_tab", {
    updateTabItems(session, "tabs", "table_tab")
  })
  
  # Contains small DFs that are used across all (or most) modules
  global_data <- load_global_data()
  
  # Connect to duckdb, pass connection to modules
  con <- create_db_connection()
  
  # Close connection when app session ends
  session$onSessionEnded(function() {
    close_db_connection(con)
  })
   
  # Run server functions for each module and provide inputs
  mod_welcome_server('welcome')
  mod_map_server(
    'map_plot', 
    con = con, 
    parent_input = input, 
    global_data = global_data
  )
  mod_graph_server(
    'graph', 
    con = con, 
    parent_input = input, 
    global_data = global_data
  )
  mod_details_server(
    'details', 
    parent_input = input
  )
  mod_table_server(
    'table', 
    parent_input = input,
    global_data = global_data
  )
}
