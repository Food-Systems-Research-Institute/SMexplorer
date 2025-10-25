#' Title
#'
#' @description
#' Convenience function for formatting four link blocks on welcome page
#' 
#' @param text 
#' @param image 
#' @importFrom shiny fluidRow column div span img
#' @returns
#' @export
#'
#' @examples
link_block <- function(text,
                       image,
                       id = NULL) {
  column(
    id = id,
    width = 6,
    div(
      # class = 'button-box',
      class = 'link-block',
      fluidRow(
        style = 'display: flex; align-items: stretch; min-height: 100px;',
        div(
          column(
            width = 6,
            style = 'display: flex; justify-content: center; align-items: center; text-align: center;',
            tags$span(
              style = 'display: inline-block; vertical-align: middle; line-height: normal;',
              text
            )
          ),
          column(
            width = 6,
            style = 'display: flex; justify-content: center; align-items: center;',
            tags$img(
              src = image,
              style = 'height: 100px; margin: 0px;',
              alt = ''
            )
          )
        )
      )
    )
  )

}