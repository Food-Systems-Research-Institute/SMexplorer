#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import reactable
#' @import stringr
#' @import htmltools
mod_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = 'button-box',
      style = 'background: #fff !important;',
      uiOutput(ns('header')),
      with_spinner(
        reactable::reactableOutput(ns('metrics_table'))
      )
    )
  )
}
    
#' table Server Functions
#'
#' @noRd 
mod_table_server <- function(id, parent_input, global_data) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Lazy load ----
    table_data_loaded <- reactiveVal(FALSE)
    metadata <- reactiveVal(NULL)

    observe({
      req(parent_input$tabs == "table_tab")

      if (!table_data_loaded()) {
        metadata(readRDS('data/table_metadata.rds'))
        table_data_loaded(TRUE)
      }
    })

    
    # Reactable -----
    output$metrics_table <- reactable::renderReactable({
      req(metadata())

      reactable(
        metadata(),
        sortable = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        pagination = TRUE,
        bordered = TRUE,
        wrap = TRUE,
        rownames = FALSE,
        onClick = 'select',
        striped = TRUE,
        pageSizeOptions = c(5, 10, 25, 50, 100),
        defaultPageSize = 5,
        showPageSizeOptions = TRUE,
        highlight = TRUE,
        style = list(fontSize = "14px"),
        compact = TRUE,
        columns = list(
          Metric = colDef(
            minWidth = 200,
            sticky = 'left'
          ),
          Definition = colDef(
            minWidth = 250,
          ),
          # Units = colDef(minWidth = 50),
          Year = colDef(minWidth = 75),
          Source = colDef(minWidth = 250),
          Scope = colDef(show = FALSE),
          Resolution = colDef(show = FALSE),
          Url = colDef(
            minWidth = 300,
            show = FALSE
          )
        ),
        defaultColDef = colDef(minWidth = 100),
        # elementId = "metrics_table",
        details = function(index) {
          data <- metadata()
          div(
            style = "padding: 15px; border: 1px solid #ddd; margin: 10px 0;
             background-color: #E0EEEE; border-radius: 10px; border-color: black;
             box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);",

            tags$h4(
              strong("Details"),
            ),
            tags$p(
              strong('Metric Name: '),
              as.character(data[index, 'Metric']),
            ),
            tags$p(
              strong('Definition: '),
              as.character(data[index, 'Definition']),
            ),
            tags$p(
              strong('Source: '),
              as.character(data[index, 'Source'])
            ),
            tags$p(
              strong('Latest Year: '),
              as.character(data[index, 'Year'])
            ),
            tags$p(
              strong('URL: '),
              tags$a(
                href = as.character(data[index, 'URL']),
                target = '_blank',
                as.character(data[index, 'URL'])
              )
            )
          )
        }
      )
    })
    
    output$header <- renderUI(
      tagList(
        tags$h2(
          'Metadata', 
          style = 'text-align: left !important;'
        ),
        tags$p(
          'Use the filters at the top of the table to search through dimensions, metrics, data sources, or other metadata.',
          'Columns can be resized, and you can sort alphabetically by clicking a column header.',
          'Click the arrow on the left to see details about a particular metric.'
        )
      )
    )
    
  })
}
    
## To be copied in the UI
# mod_table_ui("table")
    
## To be copied in the server
# mod_table_server("table")
