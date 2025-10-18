#' With Spinner 
#'
#' @description A utils function
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