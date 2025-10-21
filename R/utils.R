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
                         caption = 'Loading Map...') {
  shinycssloaders::withSpinner(
    ui_element = ui_element,
    type = type,
    color = color,
    caption = shiny::tags$p(caption)
  ) 
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
          "This page is under construction! Head to the",
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