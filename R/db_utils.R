#' DuckDB Connection Utilities
#'
#' Functions for managing DuckDB connections in the Shiny app

#' Create DuckDB Connection
#'
#' Creates a read-only connection to the application's DuckDB database.
#' The connection should be created once at app startup and passed to modules.
#'
#' @param db_path Character string specifying the path to the DuckDB database
#'   file. Default is 'data/app.duckdb'.
#' @param read_only Logical indicating whether to open in read-only mode.
#'   Default is TRUE for safety in production.
#'
#' @return A DuckDB connection object
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom duckdb duckdb
#' @export
create_db_connection <- function(db_path = 'data/appdata.duckdb', read_only = TRUE) {
  if (!file.exists(db_path)) {
    stop(sprintf("DuckDB file not found at: %s", db_path))
  }

  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = read_only
  )
}

#' Close DuckDB Connection
#'
#' Safely closes a DuckDB connection. Should be called when the Shiny
#' session ends using session$onStop().
#'
#' @param con A DuckDB connection object
#'
#' @return NULL (invisibly)
#'
#' @importFrom DBI dbDisconnect
#'
#' @examples
#' \dontrun{
#' session$onStop(function() {
#'   close_db_connection(con)
#' })
#' }
#'
#' @export
close_db_connection <- function(con) {
  if (!is.null(con)) {
    tryCatch({
      DBI::dbDisconnect(con, shutdown = TRUE)
    }, error = function(e) {
      warning(sprintf("Error closing DuckDB connection: %s", e$message))
    })
  }
  invisible(NULL)
}

#' Query DuckDB Table
#'
#' Convenience wrapper for querying the DuckDB database with error handling.
#'
#' @param con A DuckDB connection object
#' @param query Character string containing the SQL query
#'
#' @return A data frame with query results
#'
#' @importFrom DBI dbGetQuery
#'
#' @examples
#' \dontrun{
#' # Get 2024 county metrics
#' data <- query_db(con, "
#'   SELECT * FROM county_metrics
#'   WHERE year = 2024
#' ")
#' }
#'
#' @export
query_db <- function(con, query) {
  tryCatch({
    DBI::dbGetQuery(con, query)
  }, error = function(e) {
    stop(sprintf("Database query failed: %s\nQuery: %s", e$message, query))
  })
}
