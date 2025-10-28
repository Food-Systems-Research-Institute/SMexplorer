#' graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fluidRow column uiOutput selectInput selectizeInput conditionalPanel moduleServer reactiveVal observe req reactive renderUI updateSelectizeInput HTML tags div
#' @importFrom plotly renderPlotly plotlyOutput plot_ly layout event_register ggplotly event_data
#' @importFrom ggplot2 ggplot aes geom_point labs theme_classic geom_smooth sym
#' @importFrom dplyr inner_join filter pull select right_join left_join mutate across where
#' @importFrom tidyr pivot_wider unnest
#' @importFrom purrr map
#' @importFrom stringr str_subset
#' @importFrom snakecase to_title_case
#' @importFrom reactable reactableOutput reactable colDef
#' @importFrom broom tidy
#' @importFrom shinydashboard box
#' @importFrom shinyWidgets awesomeCheckbox actionBttn
#' @importFrom stats cor.test
#' @importFrom glue glue
mod_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      
      # Left Column -----
      column(
        width = 7,
        with_spinner(
          uiOutput(ns('graph_box'))
        ),
        uiOutput(ns('cor_box')),
        uiOutput(ns('click_box'))
      ),
      
      # Right Column -----
      column(
        width = 5,
        box(
          title = 'Select Metrics',
          width = 12,
          status = 'primary',
          solidHeader = TRUE,
          collapsible = TRUE,
          
          selectInput(
            inputId = ns('select_resolution'),
            label = 'Choose resolution:',
            # TODO: Fix issues with state so we can choose that also
            choices = c('County'),
            selected = NULL,
            width = '100%'
          ),
          
          selectizeInput(
            inputId = ns('search_x'),
            label = 'Metric one:',
            choices = NULL,
            selected = NULL,
            width = '100%'
          ),
         
          selectizeInput(
            inputId = ns('search_y'),
            label = 'Metric two:',
            choices = NULL,
            selected = NULL,
            width = '100%'
          ),
          
          conditionalPanel(
            condition = "input.select_resolution == 'County'",
            ns = ns,
            
            awesomeCheckbox(
              inputId = ns("cor_check"),
              label = "Add correlation",
              value = FALSE
            ),
            
            awesomeCheckbox(
              inputId = ns("loess"),
              label = "Add LOESS curve",
              value = FALSE
            )
          ),
          
          actionBttn(
            ns('update_graph'),
            'Compare Metrics',
            style = 'unite',
            icon = icon('arrows-rotate')
          )
          
        ), # End select metrics box
        
        ## Info box -----
        uiOutput(ns('info_box'))
      )
    ) # End of first fluid row
  ) # End tag list
} # End ui function
    
#' graph Server Functions
#'
#' @noRd 
mod_graph_server <- function(id, 
                             con,
                             parent_input, 
                             global_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Metric options -----
    observe({
      req(input$select_resolution)
      
      # Get metric options given resolution
      search_term <- paste0('RES_', input$select_resolution)
      metric_options <- global_data$metadata$Metric[
        global_data$metadata[[search_term]] == TRUE
      ]
      
      # Update search fields
      updateSelectizeInput(
        session, 
        'search_x', 
        choices = c('', metric_options),
        selected = NULL,
        server = TRUE
      )
      updateSelectizeInput(
        session, 
        'search_y', 
        choices = c('', metric_options),
        selected = NULL,
        server = TRUE
      )
    })


    # Query Data -----
    rval_data <- reactive({
      req(input$search_x, input$search_y)
      
      print(input$search_x)
      print(input$search_y)
      
      # Lookup table to get variable names (used in db) from metric names (inputs)
      xvar <- unique(metric_lookup$`Variable Name`[metric_lookup$Metric == input$search_x])
      yvar <- unique(metric_lookup$`Variable Name`[metric_lookup$Metric == input$search_y])
      
      # Query metrics
      table <- paste0('neast_', tolower(input$select_resolution), '_metrics')
      query <- glue::glue(
        "SELECT *
        FROM {table}
        WHERE variable_name IN ('{xvar}', '{yvar}')"
      )
      dat <- query_db(con, query)
      
      # Join with fips key to get county name and state name
      dat <- dat %>% 
        dplyr::left_join(
          select(global_data$fips_key, fips, county_name, state_name),
          by = 'fips'
        ) %>% 
        get_latest_year() %>% 
        pivot_wider(
          id_cols = c('fips', 'county_name', 'state_name'),
          names_from = 'variable_name',
          values_from = 'value'
        )
        
      # Return filtered data and variable names
      list(data = dat, xvar = xvar, yvar = yvar)
    })
    
    
    # Graph box -----
    output$graph_box <- renderUI({
      box(
        width = 12,
        title = 'Metric Comparison',
        solidHeader = TRUE,
        status = 'primary',
        collapsible = TRUE,
        
        plotlyOutput(ns('graph'))
      )
    })
    
    
    # Plotly Graph -----
    plot_to_render <- eventReactive(input$update_graph, {
      req(input$search_x, input$search_y)
      # TODO: check if we even need to do this
      plot_dat <- rval_data()$data
      xvar <- rval_data()$xvar
      yvar <- rval_data()$yvar

      # Generate x and y labels
      x_label <- unique(metric_lookup$`Axis Name`[metric_lookup$`Variable Name` == xvar])
      y_label <- unique(metric_lookup$`Axis Name`[metric_lookup$`Variable Name` == yvar])

      # Create the ggplot
      plot <- plot_dat %>%
        ggplot(aes(
          x = !!sym(xvar),
          y = !!sym(yvar),
          color = state_name,
          key = fips,
          # Popup - this sucks but it works. Can't use function because we
          # need to call it within aes()
          text = ifelse(
            # !is.na(county_name),
            'county_name' %in% names(plot_dat),
            paste0(
              '<b>', county_name, ', ', state_name, '</b>\n',
              x_label, ': ', format(round(!!sym(xvar), 3), big.mark = ','), '\n',
              y_label, ': ', format(round(!!sym(yvar), 3), big.mark = ',')
            ),
            paste0(
              '<b>', state_name, '</b>\n',
              x_label, ': ', format(round(!!sym(xvar), 3), big.mark = ','), '\n',
              y_label, ': ', format(round(!!sym(yvar), 3), big.mark = ',')
            )
          )
        )) +
        geom_point(size = 1.5, alpha = 0.7) +
        labs(
          x = x_label,
          y = y_label,
          title = paste0('"', y_label, '" by "', x_label, '"'),
          color = 'State'
        ) +
        theme_classic()

      # Optionally add LOESS smooth
      if (input$loess == TRUE) {
        plot <- plot + geom_smooth(aes(group = 1))
      }

      # Convert ggplot to plotly and return
      ggplotly(plot, tooltip = 'text') %>%
        layout(
          hoverlabel = list(
            bgcolor = "#154732",
            bordercolor = 'white',
            align = 'auto',
            font = list(
              size = 14,
              color = 'white'
            )
          )
        ) %>%
        event_register('plotly_click')
    })

    output$graph <- renderPlotly({
      # Show empty plot on initial load (before button is clicked)
      if (input$update_graph == 0) {
        plot_ly() %>%
          layout(
            title = list(
              text = "Select two metrics in the box on\nthe right to display a graph",
              x = 0.5,
              y = 0.6
            ),
            xaxis = list(
              showgrid = FALSE,
              showline = TRUE,
              range = c(0, 10)
            ),
            yaxis = list(
              showgrid = FALSE,
              showline = TRUE,
              range = c(0, 10)
            )
          ) %>%
          event_register('plotly_click')
      } else {
        # After button click, render the plot
        plot_to_render()
      }
    })
    
    
    # Output click_box -----
    output$click_box <- renderUI({
      req(input$search_x, input$search_y)
      point <- event_data(event = "plotly_click", priority = "event")
      
      box_title <- if (is.null(point)) {
        "Select a point to see details by county"
      } else {
        if (input$select_resolution == 'County') {
          location <- global_data$fips_key$county_name[
            global_data$fips_key$fips == point$key
          ]
        } else if (input$select_resolution == 'State') {
          location <- global_data$fips_key$state_name[
            global_data$fips_key$fips == point$key
          ]
        }
        paste("Details for", location)
      }
      
      box(
        title = box_title,       
        width = 12,              
        status = "primary",      
        solidHeader = TRUE,
        collapsible = TRUE,
        
        reactableOutput(ns("click_table"))
      )
    })
    
    ## Output click_table -----
    output$click_table <- renderReactable({
      point <- event_data(event = "plotly_click", priority = "event")
      req(point)
      
      # Get new DF, all info for selected county (or state)
      table <- glue::glue('neast_{tolower(input$select_resolution)}_metrics')
      query <- glue::glue(
        "SELECT 
          m.Metric,
          m.Definition,
          d.Year,
          m.Units,
          d.value
        FROM {table} d
        LEFT JOIN metadata m ON d.variable_name = m.\"Variable Name\"
        WHERE fips = {point$key}"
      )
      dat <- query_db(con, query)
      
      dat %>% 
        setNames(c(snakecase::to_title_case(names(.)))) %>% 
        reactable(
          sortable = TRUE,
          resizable = TRUE,
          filterable = TRUE,
          searchable = TRUE,
          pagination = TRUE,
          bordered = TRUE,
          wrap = TRUE,
          rownames = FALSE,
          striped = TRUE,
          pageSizeOptions = c(5, 10, 25, 50, 100),
          defaultPageSize = 5,
          showPageSizeOptions = TRUE,
          style = list(fontSize = "14px"),
          compact = TRUE
        )
    })
    
    # Output info_box -----
    output$info_box <- renderUI({
      box(
        title = 'Metric Details',
        width = 12,
        status = 'primary',
        solidHeader = TRUE,
        collapsible = TRUE,
        htmlOutput(ns('metric_info'))
      )
    })
    
    ## Output metric_info -----
    output$metric_info <- renderUI({

      # If no inputs, display message saying to select metric
      if (input$search_x == '' && input$search_y == '') {
        tags$p('Select metrics above to see details.')

      # Else if one or both are selected, display info
      } else if (input$search_x != '' || input$search_y != '') {

        tag_list <- tagList()

          # Use utility function to create metric info display
          if (input$search_x != '') {
            tag_list[[length(tag_list) + 1]] <- global_data$metadata %>%
              dplyr::filter(Metric == input$search_x) %>%
              get_metric_info()
          }
          if (input$search_y != '') {
            tag_list[[length(tag_list) + 1]] <- global_data$metadata %>%
              dplyr::filter(Metric == input$search_y) %>%
              get_metric_info()
          }
        
        tags$div(tag_list)

      } # end ifelse for HTML output
    }) # end metric_info renderUI
    
    # Output cor_box -----
    output$cor_box <- renderUI({
      req(
        input$cor_check == TRUE, 
        input$search_x, 
        input$search_y,
        input$select_resolution
      )
      
      box(
        title = 'Pearson Correlation',
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = 'primary',
        
        div(
          class = "centered-table",
          reactableOutput(ns('cor'))
        ),
        
        tags$head(
          tags$style(HTML("
            .full-width-table {
              width: 100%; border-width: 3px;
            }
            .reactable {
              width: 100%;  /* Make the table content-sized */
            }"
          ))
        )
        
      )
    })
    
    ## Output cor -----
    output$cor <- renderReactable({
      req(input$search_x, input$search_y)
      
      cor_out <- cor.test(
        rval_data()$data[[rval_data()$xvar]],
        rval_data()$data[[rval_data()$yvar]],
        method = 'pearson'
      )
      
      tidy(cor_out) %>% 
        select(
          -method, 
          -alternative, 
          df = parameter,
          t.stat = statistic
        ) %>% 
        mutate(
          across(!df, ~ format(round(., 3), nsmall = 3)),
          p.value = ifelse(p.value < 0.05, paste0(p.value, ' *'), p.value)
        ) %>% 
        reactable(
          sortable = FALSE,
          resizable = FALSE,
          filterable = FALSE,
          searchable = FALSE,
          pagination = FALSE,
          bordered = TRUE,
          wrap = FALSE,
          rownames = FALSE,
          striped = FALSE,
          style = list(fontSize = "14px"),
          compact = TRUE,
          defaultColDef = colDef(
            minWidth = 80,
            align = 'center'
          ),
          columns = list(
            p.value = colDef(
              style = function(value) {
                if (value < 0.05) {
                  font_weight <- 'bold'
                  color <- '#cdf6d9'
                } else if (value >= 0.05) {
                  font_weight <- NULL
                  color <- '#FFFFFF'
                }
                list(
                  fontWeight = font_weight,
                  background = color
                )
              }
            )
          )
        )
      
    })
  }) # end moduleServer
    
} # end server function
    
## To be copied in the UI
# mod_graph_ui("graph")
    
## To be copied in the server
# mod_graph_server("graph")
