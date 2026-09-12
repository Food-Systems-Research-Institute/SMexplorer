#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinycssloaders showPageSpinner
#' @noRd
#'
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shinydashboard::dashboardPage(
      skin = "green",
      mod_header_ui("header"),
      mod_sidebar_ui("sidebar"),
      mod_body_ui("body")
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
