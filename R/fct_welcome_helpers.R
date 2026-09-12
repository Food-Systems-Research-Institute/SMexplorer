#' Title
#'
#' @description
#' Convenience function for formatting four link blocks on welcome page
#'
#' @param text
#' @param image
#' @param id Element id, expected to be of the form 'go_to_<tab_value>_tab'
#'   so the target tab can be derived for the anchor's href/data-value.
#' @importFrom shiny fluidRow column div span img
#' @returns
#' @export
#'
#' @examples
link_block <- function(title = NULL,
                       text = NULL,
                       image = NULL,
                       id = NULL) {
  tab_value <- sub("^go_to_", "", id)
  column(
    width = 6,
    div(
      class = "link-block",
      tags$a(
        id = id,
        href = paste0("#shiny-tab-", tab_value),
        `data-toggle` = "tab",
        `data-value` = tab_value,
        class = "link-block-btn",
        `aria-label` = title,
        div(
          class = "content",
          tags$span(
            tags$h5(title),
            tags$p(text)
          )
        ),
        div(
          class = "screenshot",
          tags$img(
            src = image,
            alt = glue("Screenshot of {title}")
          )
        )
      )
    )
  )
}
