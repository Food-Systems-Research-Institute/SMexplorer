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
link_block <- function(title = NULL,
                       text = NULL,
                       image = NULL,
                       id = NULL) {
  column(
    id = id,
    width = 6,
    div(
      class = 'link-block',
      div(
        style = 'display: flex; align-items: center; justify-content: space-between; min-height: 100px;',
        div(
          style = 'flex: 1; display: flex; justify-content: center; align-items: center; text-align: center; padding: 10px;',
          tags$span(
            style = 'display: inline-block; vertical-align: middle; line-height: normal;',
            tags$h5(title),
            tags$p(text)
          )
        ),
        div(
          style = 'flex: 1; display: flex; justify-content: center; align-items: center; padding: 10px;',
          tags$img(
            src = image,
            style = 'height: 100px; margin: 0px;',
            alt = ''
          )
        )
      )
    )
  )

}