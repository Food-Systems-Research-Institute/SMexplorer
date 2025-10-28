#' With Spinner
#'
#' @description Wrap a UI element in a formatted spinner
#'
#' @return The return value, if any, from executing the utility.
#' @importFrom shiny tags
#' @importFrom shinycssloaders withSpinner
#' @noRd
with_spinner <- function(ui_element,
                         type = 6,
                         color = color_palette['theme_green'],
                         caption = 'Loading...',
                         left_offset = NULL) {
  spinner <- shinycssloaders::withSpinner(
    ui_element = ui_element,
    type = type,
    color = color,
    caption = shiny::tags$p(caption)
  )

  if (!is.null(left_offset)) {
    shiny::tags$div(
      style = paste0('margin-left: ', left_offset, ';'),
      spinner
    )
  } else {
    spinner
  }
}

#' Modal Construction
#'
#' @returns
#' @export
#'
#' @examples
modal_construction <- function(tab) {
  showModal(modalDialog(
    tagList(
      tags$div(
        class = 'button-box',
        tags$p(
          "This page is under construction! Nothing works right yet.",
          "Head to the",
          tags$a(
            'FSRI GitHub Page',
            href = 'https://www.github.com/Food-Systems-Research-Institute/SMexplorer/issues',
            target = '_blank'
          ),
          "for issues and pull requests."
        )
      )
    ),
    title = "Under Construction",
    easyClose = TRUE
  ))

}

#' Create Metric Information Display
#'
#' @description Tags for metadata display. Juts feed it the metadata df.
#'
#' @param meta
#'
#' @return HTML div element with formatted metric information
#'
#' @importFrom shiny tags tagList
#'
#' @examples
#' @noRd
get_metric_info <- function(meta) {
  # TODO: Why are we getting doubles in the first place?
  meta <- meta[1, ]
  div(
    class = 'button-box',
    style = 'background-color: #fff !important;',
    tags$p(tags$strong('Metric:'), meta$Metric), tags$br(),
    tags$p(tags$strong('Definition:'), meta$Definition), tags$br(),
    tags$p(tags$strong('Units:'), meta$Units), tags$br(),
    tags$p(tags$strong('Dimension:'), meta$Dimension), tags$br(),
    tags$p(tags$strong('Indicator:'), meta$Indicator), tags$br(),
    tags$p(tags$strong('Resolution:'), meta$Resolution), tags$br(),
    tags$p(tags$strong('Source:'), tags$a(meta$Source)), tags$br(),
    tags$p(tags$strong('Citation:'), meta$Citation)
  )
}