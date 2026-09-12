#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id) {
  shinydashboard::dashboardSidebar(
    width = "175px",
    shinydashboard::sidebarMenu(
      id = "tabs",

      ## Tabs -----
      # welcome_tab
      shinydashboard::menuItem("Home", tabName = "welcome_tab", icon = icon("house")),

      # map_tab
      menuItem("Map", tabName = "map_tab", icon = icon("map")),

      # graph_tab
      menuItem("Metric Comparisons", tabName = "graph_tab", icon = icon("chart-simple")),

      # details_tab
      menuItem("Details", tabName = "details_tab", icon = icon("circle-info")),

      # database_tab
      menuItem("Database", tabName = "database_tab", icon = icon("database")),

      # table_tab
      menuItem("Metadata", tabName = "table_tab", icon = icon("table"))
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
