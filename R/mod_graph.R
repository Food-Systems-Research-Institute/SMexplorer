#' graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import plotly
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import snakecase
#' @import reactable
#' @import broom
source('R/filter_metrics.R')
source('R/data_pipeline_functions.R')
source('R/filter_fips.R')
mod_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      
      # Left Column -----
      column(
        width = 7,
        
        ## plotlyOutput -----
        uiOutput(ns('graph_box')),
        
        ## Cor output -----
        uiOutput(ns('cor_box')),
        
        ## Click output -----
        uiOutput(ns('click_box'))
        
      ),
      
      # Right Column -----
      column(
        width = 5,
        
        ## Select Metrics Box -----
        box(
          title = 'Select Metrics',
          width = 12,
          status = 'primary',
          solidHeader = TRUE,
          collapsible = TRUE,
          
          ### Choose Scale -----
          selectInput(
            inputId = ns('choose_scale'),
            label = 'Choose a Scale:',
            choices = c('County', 'State'),
            selected = 'County',
            width = '100%',
            multiple = FALSE
          ),
          
          ### Search X -----
          selectizeInput(
            inputId = ns('search_x'),
            label = 'Select Metric for X-Axis:',
            choices = NULL,
            selected = NULL,
            width = '100%',
            multiple = FALSE
          ),
          
          # format search x button
          tags$head(
            tags$style(
              HTML(
                '
                .selectize-input {
                  word-wrap: break-word;
                  word-break: break-word;
                  max-width: 100%; 
                    overflow: hidden; 
                    text-overflow: ellipsis; 
                    white-space: nowrap; 
                }
                
                .selectize-dropdown {
                  word-wrap: break-word;
                  word-break: break-word;
                  max-width: 100px !important; 
                    overflow: hidden; 
                    text-overflow: ellipsis; 
                }
                '
              )
            )
          ),
          
          ### Search Y -----
          selectizeInput(
            inputId = ns('search_y'),
            label = 'Select Metric for Y-Axis:',
            choices = NULL,
            selected = NULL,
            width = '100%',
            multiple = FALSE
          ),
          
          # Format search y button
          tags$head(tags$style(
            HTML(
              '
                .selectize-input {
                  word-wrap: break-word;
                  word-break: break-word;
                  max-width: 100%; 
                    overflow: hidden; 
                    text-overflow: ellipsis; 
                    white-space: nowrap; 
                }
                
                .selectize-dropdown {
                  word-wrap: break-word;
                  word-break: break-word;
                  max-width: 100px !important; 
                    overflow: hidden; 
                    text-overflow: ellipsis; 
                }
              '
            )
          )),
          
          
          ### LOESS checkbox -----
          awesomeCheckbox(
            inputId = ns("loess"),
            label = "Add LOESS Curve", 
            value = FALSE,
            status = "primary"
          ),
          
          
          ### Correlation checkbox -----
          awesomeCheckbox(
            inputId = ns("cor_check"),
            label = "Add Correlation", 
            value = FALSE,
            status = "primary"
          ),
          
          tags$style(HTML(paste0(
            "#", ns("cor_check"), " { ",
            "background-color: #154734 !important; ",
            "color: white !important; ",
            "width: 50%; ",
            "margin-left: auto; ",
            "margin-right: auto; ",
            "display: block; ",
            "} "
          )))
          
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
mod_graph_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
     # Load data -----
    load('data/sm_data.rda')
    
    
    # Metric options -----
    observe({
      chosen_scale <- tolower(input$choose_scale)
      
      metric_options <- sm_data$metrics %>%
        inner_join(sm_data$metadata, by = 'variable_name') %>%
        filter(resolution == chosen_scale) %>%
        pull(metric) %>%
        unique() %>%
        sort()
      
      # Reorder metrics, put NAICS last
      metric_options <- c(
        sort(metric_options[!grepl("NAICS", metric_options)]),
        sort(metric_options[!grepl("NAICS", metric_options)]),
        sort(metric_options[grepl("NAICS", metric_options)])
      )
        
      # Update search fields
      updateSelectizeInput(
        session, 
        'search_x', 
        choices = metric_options,
        selected = character(0)
      )
      updateSelectizeInput(
        session, 
        'search_y', 
        choices = metric_options,
        selected = character(0)
      )
    })
    
    
    # Filter Data -----
    rval_data <- reactive({
      req(input$search_x, input$search_y)
      
      # Get resolution of inputs 
      res_x <- sm_data$metadata %>% 
        filter(metric == input$search_x) %>% 
        pull(resolution)
      res_y <- sm_data$metadata %>% 
        filter(metric == input$search_y) %>% 
        pull(resolution)
      
      # If they are not the same, throw an error
      # Otherwise, filter fips by scope/resolution (inconsistent)
      if (res_x != res_y) {
        stop(paste(
          'The metrics you have selected are at different spatial scales.',
          'It is probably the case that one variable is at the county level while the other is at the state level.',
          'Check the resolution field in the Metric Details box.'
        ))
      } else if (res_x == 'county' & res_y == 'county') {
        dat <- filter_fips(sm_data$metrics, scope = 'counties')
      } else if (res_x == 'state' & res_y == 'state') {
        dat <- filter_fips(sm_data$metrics, scope = 'states')
      }
      
      # Filter to selected variables
      # have to join with metadata to search by metric
      # Also join to fips to get county name
      # TODO: Do this ahead of time to avoid joining in reactive
      dat <- sm_data$metadata %>% 
        select(
          variable_name,
          metric,
          dimension,
          index,
          indicator,
          axis_name,
          units,
          all_years = year,
          definition,
          updates,
          source,
          url,
          citation
        ) %>% 
        right_join(dat, by = 'variable_name') %>% 
        filter(metric %in% c(input$search_x, input$search_y)) %>% 
        left_join(sm_data$fips_key, by = 'fips') %>% 
        filter(fips != '00') # removing US totals for now
      
      # Get the variable names
      xvar <- unique(dat$variable_name[dat$metric == input$search_x])
      yvar <- unique(dat$variable_name[dat$metric == input$search_y])
      
      # Process data for the latest year
      dat <- dat %>%
        get_latest_year() %>%
        mutate(
          variable_name = paste0(variable_name, '_', year),
          .keep = 'unused'
        ) %>%
        mutate(value = as.numeric(value)) %>% 
        pivot_wider(
          id_cols = c('fips', 'county_name', 'state_name'),
          names_from = 'variable_name',
          values_from = 'value'
        )
      
      # If there are lingering list columns, choose the second value This is the
      # jenkiest thing I've ever seen. 
      # NOTE: Have to get to source of problem - why are there some counties
      # showing up with two values for NAICS vars in the same year? Something
      # about the disclosures, but I haven't been able to fix it in sm-data yet.
      dat <- dat %>%
        mutate(across(where(~ is.list(.)), 
                      ~ map(., ~ if(length(.x) > 1) .x[2] else .x), 
                      .names = "{.col}")) %>% 
        unnest(cols = where(~ is.list(.)))
        
      # Reassign x and y variables after pasting year
      xvar <- str_subset(names(dat), xvar)
      yvar <- str_subset(names(dat), yvar)
      
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
    
    
    # Plotly -----
    output$graph <- renderPlotly({
      if (input$search_x == "" || input$search_y == "") {
        # Empty plot if no variables selected
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
          )
      } else {

        # Get the filtered data and variables from the reactive function
        plot_dat <- rval_data()$data
        xvar <- rval_data()$xvar
        yvar <- rval_data()$yvar
        
        # Generate x and y labels
        x_label <- snakecase::to_title_case(xvar)
        y_label <- snakecase::to_title_case(yvar)
        
        # Create the ggplot
        plot <- plot_dat %>%
          ggplot(aes(
            x = !!sym(xvar), 
            y = !!sym(yvar),
            color = state_name,
            key = county_name,
            text = paste0(
              '<b>', county_name, ', ', state_name, '</b>\n',
              x_label, ': ', format(round(!!sym(xvar), 3), big.mark = ','), '\n',
              y_label, ': ', format(round(!!sym(yvar), 3), big.mark = ',')
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
      }
    })
    
    
    # Output click_box -----
    output$click_box <- renderUI({
      req(input$search_x, input$search_y)
      point <- event_data(event = "plotly_click", priority = "event")
      
      box_title <- if (is.null(point)) {
        "Select a point to see details by county"
      } else {
        paste("Details for", point$key)
      }
      
      box(
        title = box_title,       
        width = 12,              
        status = "primary",      
        solidHeader = TRUE,
        collapsible = TRUE,
        
        uiOutput(ns('disclaimer')),
        reactableOutput(ns("click_table"))
      )
    })
    
    output$disclaimer <- renderUI({
      point <- event_data(event = "plotly_click", priority = "event")
      req(point)
      HTML(
        '<p>[Note to swap this out for a series of bar graphs highlighting 
        the county, or maybe compare it to state and national stats?]</p>'
      )
    })
    
    ## Output click_table -----
    output$click_table <- renderReactable({
      point <- event_data(event = "plotly_click", priority = "event")
      req(point)
      
      dat %>% 
        filter(county_name == point$key) %>% 
        select(
          metric,
          definition,
          year,
          units,
          value
        ) %>% 
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
        HTML('<p>Select metrics above to see details.</p>')
        
      # Else if one or both are selected, display info
      } else if (input$search_x != '' || input$search_y != '') {
        
        # Empty HTML output to add to
        html_output <- ''
        
        # Add x info if selected
        if (input$search_x != '') {
          meta_x <- sm_data$metadata %>% 
            filter(metric == input$search_x)
          html_output_x <- paste0(
            html_output,
            '<h4>X-Axis: ', input$search_x, '</h4>',
            '<p><b>Definition:</b> ', meta_x$definition, '<br>',
            '<b>Dimension:</b> ', meta_x$dimension, '<br>',
            '<b>Index:</b> ', meta_x$index, '<br>',
            '<b>Indicator:</b> ', meta_x$indicator, '<br>',
            '<b>Resolution:</b> ', meta_x$resolution, '<br>',
            '<b>Updates:</b> ', meta_x$updates, '<br>',
            '<b>Source: </b><a href="', meta_x$url, '">', meta_x$source, '</a><br>',
            '<b>Citation:</b> ', meta_x$citation, '</p>'
          )
        }
        
        # Add y info if selected
        if (input$search_y != '') {
          meta_y <- sm_data$metadata %>% 
            filter(metric == input$search_y)
          html_output_y <- paste0(
            html_output,
            '<br>',
            '<h4>Y-Axis: ', input$search_y, '</h4>',
            '<p><b>Definition:</b> ', meta_y$definition, '<br>',
            '<b>Dimension:</b> ', meta_y$dimension, '<br>',
            '<b>Index:</b> ', meta_y$index, '<br>',
            '<b>Indicator:</b> ', meta_y$indicator, '<br>',
            '<b>Resolution:</b> ', meta_y$resolution, '<br>',
            '<b>Updates:</b> ', meta_y$updates, '<br>',
            '<b>Source: </b><a href="', meta_y$url, '">', meta_y$source, '</a><br>',
            '<b>Citation:</b> ', meta_y$citation, '</p>'
          )
        }
        
        # Return HTML output
        # div(
        #   div(
        #     class = 'button-box',
        #     HTML(html_output_x),
        #   ),
        #   div(
        #     class = 'button-box',
        #     HTML(html_output_y),
        #   )
        # )
        HTML(html_output)
        
      } # end ifelse for HTML output
    }) # end metric_info renderUI
    
    # Output cor_box -----
    output$cor_box <- renderUI({
      req(input$cor_check == TRUE, input$search_x, input$search_y)
      
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
