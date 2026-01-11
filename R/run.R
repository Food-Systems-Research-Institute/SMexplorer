#' Run
#' 
#' @description
#' Shortcut to run `devtools::load_all()` and `SMexplorer::run_app()`
#' 
#' @returns
#' @export
#'
#' @examples
r <- function() {
  devtools::load_all()
  SMexplorer::run_app()
}
  