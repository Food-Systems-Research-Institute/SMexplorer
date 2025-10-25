#' Get Latest Year for Each Variable
#'
#' Filters a data frame so that each unique variable only has data from its
#' most recent year. Useful for working with longitudinal data where different
#' variables may have been collected in different years.
#'
#' @param df A data frame containing variables with year information
#' @param var_col Character string specifying the column name that contains
#'   variable names. Default is 'variable_name'.
#' @param year_col Character string specifying the column name that contains
#'   year information. Default is 'year'.
#'
#' @return A data frame containing only the most recent year of data for each
#'   unique variable in the var_col column.
#'
#' @export
#'
#' @importFrom dplyr mutate filter pull bind_rows %>%
#' @importFrom purrr map
#' @importFrom rlang .data
get_latest_year <- function(df, var_col = 'variable_name', year_col = 'year'){

  # Make sure year column is numeric
  df <- dplyr::mutate(df, {{ year_col }} := as.numeric(.data[[year_col]]))

  # Get unique vector of variables from variable_name column
  vars <- unique(df[[var_col]])

  # Get new df with only the latest year of each variable
  # Map over each variable_name
  filtered_df <- purrr::map(vars, \(var) {

    # Get all unique years
    unique_years <- df %>%
      dplyr::filter(.data[[var_col]] == var) %>%
      dplyr::pull({{ year_col }}) %>%
      unique()

    # Filter to the lastest unique year for each variable
    out <- df %>%
      dplyr::filter(.data[[var_col]] == var, .data[[year_col]] == max(unique_years))
    return(out)
  }) %>%

    # Put each variable back together
    dplyr::bind_rows()

  return(filtered_df)
}

