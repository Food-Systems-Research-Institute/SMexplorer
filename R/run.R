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
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package 'devtools' is required to use this dev shortcut. Install it with install.packages('devtools').")
  }
  devtools::load_all()
  SMexplorer::run_app()
}
