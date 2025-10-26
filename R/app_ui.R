#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders showPageSpinner
#' @noRd
#' 
# source('R/my_theme.R')
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    dashboardPage(
      skin = 'green',
      
      # Header ---------------------------------------------------------------
      shinydashboardPlus::dashboardHeader(
        title = "SM Explorer",
        rightUi = tags$li(
          class = "dropdown",
          tags$li(
            tags$a(
              href = "https://www.github.com/Food-Systems-Research-Institute/SMexplorer",
              target = "_blank",
              icon("github"),
              style = "display: flex; align-items: center; height: 50px;"
            )
          ),
          tags$li(
            tags$a(
              href = "https://www.uvm.edu/ovpr/food-systems-research",
              target = "_blank",
              tags$img(
                src = "www/fsri_spirit_marker.png",
                height = "20px"
              ),
              style = "display: flex; align-items: center; height: 50px;"
            )
          )
        )
      ),

      # Sidebar -----------------------------------------------------------
      dashboardSidebar(
        width = '175px',
        
        sidebarMenu(
          id = 'tabs',
          
          ## Tabs -----
          # welcome_tab
          menuItem('Home', tabName = 'welcome_tab', icon = icon('house')),
          
          # map_tab
          menuItem("Interactive Map", tabName = "map_tab", icon = icon("map")),
          
          # graph_tab
          menuItem("Metric Comparisons", tabName = "graph_tab", icon = icon("chart-simple")),
          
          # details_tab
          menuItem("Details", tabName = "details_tab", icon = icon("circle-info")),
          
          # table_tab
          menuItem("Metadata", tabName = "table_tab", icon = icon("table"))
          # conditionalPanel(
          #   condition = "input.tabs == 'map_tab'",
          #   div(
          #     class = 'sidebar-conditional-text',
          #     tags$p(
          #       "Use the input panel on the right to choose a metric by either
          #       searching or using the dropdown. Push the 'Update Map' button to
          #       view a metric."
          #     )
          #   )
          # ),
          
          
        )
      ),
      
      # Body ---------------------------------------------------------------
      dashboardBody(
        shinyjs::useShinyjs(),
        tabItems(
          tabItem(
            tabName = 'welcome_tab',
            mod_welcome_ui('welcome')
          ),
          tabItem(
            tabName = 'map_tab',
            mod_map_ui('map_plot')
          ),
          tabItem(
            tabName = 'graph_tab',
            mod_graph_ui("graph")
          ),
          tabItem(
            tabName = 'tree_tab',
            mod_tree_ui('tree')
          ),
          tabItem(
            tabName = 'table_tab',
            mod_table_ui('table')
          ),
          tabItem(
            tabName = 'details_tab',
            mod_details_ui('details')
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", 
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SMexplorer"
    )
  )
}
