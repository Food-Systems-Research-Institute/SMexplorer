#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
#' 
source('R/my_theme.R')
app_ui <- function(request) {
  tagList(
    dashboardPage(
      dashboardHeader(
        title = "SMExplorer"
      ),
      
      # Sidebar -----------------------------------------------------------
      dashboardSidebar(
        sidebarMenu(
          id = 'tabs',
          
          ## map_tab -----
          menuItem("Interactive Map", tabName = "map_tab", icon = icon("map")),
          conditionalPanel(
            condition = "input.tabs == 'map_tab'",
            div(
              style = "text-align: center; width: 100%; white-space: normal; 
                overflow-wrap: break-word; padding: 10px",
              HTML(
                "<p>Use the input panel on the right to choose a metric by either
                searching or using the dropdown. Push the 'Update Map' button to
                view a metric.</p>"
              )
            )
          ),
          
          ## graph_tab -----
          menuItem("Bivariate Graphs", tabName = "graph_tab", icon = icon("chart-simple")),
          conditionalPanel(
            condition = "input.tabs == 'graph_tab'",
            div(
              style = "text-align: center; width: 100%; white-space: normal; 
                overflow-wrap: break-word; padding: 10px",
              HTML(
                "<p>Use the input panel on the right to choose two sustainability
                metrics to compare. Once the graph is created, you can hover 
                over points to see values by county. You can also click on a 
                point to see more information on taht county in a new box</p>"
              )
            # div(
            #   style = "display: flex; justify-content: center; align-items: center; height: 50px;",
            #   selectInput("select_x_var", "Choose x variable")
            # )
            # div(
            #   style = "display: flex; justify-content: center; align-items: center; height: 50px;",
            #   actionButton("show_graph", "Create Graphs")
            # )
            )
          ),
          
          ## tree_tab -----
          # menuItem("Metrics Framework", tabName = "tree_tab", icon = icon("sitemap")),
          # conditionalPanel(
          #   condition = "input.tabs == 'tree_tab'",
          #   div(
          #     style = "text-align: center; width: 100%; white-space: normal;
          #       overflow-wrap: break-word; padding: 10px",
          #     HTML(
          #       "<p>I thought this might be fun, but I don't think it's really
          #       worth keeping. Click on each grouping to see subgroups.</p>"
          #     )
          #   )
          # ),
          
          ## table_tab -----
          menuItem("Metadata Table", tabName = "table_tab", icon = icon("table")),
          conditionalPanel(
            condition = "input.tabs == 'table_tab'",
            div(
              style = "text-align: center; width: 100%; white-space: normal; 
                overflow-wrap: break-word; padding: 10px",
              HTML(
                "<p>Explore metrics using this interactive table. You can 
                search by column or across all columns, reorder columns, and 
                scroll through pages.</p>
                <p>You can also hit the arrow on the left side of each row to 
                see more details, including a URL to the source of the data.</p>"
              )
            )
          )
        )
      ),
      
      # Body ---------------------------------------------------------------
      dashboardBody(
        # Formatting from my_theme.R CSS
        # my_theme,
        tabItems(
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
