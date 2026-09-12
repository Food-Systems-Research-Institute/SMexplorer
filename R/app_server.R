#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
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
    con = con,
    parent_input = input,
    global_data = global_data
  )
  mod_table_server(
    'table', 
    parent_input = input,
    global_data = global_data
  )
  mod_database_server(
    'database', 
    con = con,
    parent_input = input,
    global_data = global_data
  )
}
