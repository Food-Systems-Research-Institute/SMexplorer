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
                         caption = 'Loading...',
                         left_offset = NULL) {
  spinner <- shinycssloaders::withSpinner(
    ui_element = ui_element,
    type = type,
    color = color,
    caption = shiny::tags$p(caption)
  )

  if (!is.null(left_offset)) {
    shiny::tags$div(
      style = paste0('margin-left: ', left_offset, ';'),
      spinner
    )
  } else {
    spinner
  }
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
          "This page is under construction! Nothing works right yet.",
          "Head to the",
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

#' Create Metric Information Display
#'
#' @description Tags for metadata display. Juts feed it the metadata df.
#'
#' @param meta
#'
#' @return HTML div element with formatted metric information
#'
#' @importFrom shiny tags tagList
#'
#' @examples
#' @noRd
get_metric_info <- function(meta) {
  # TODO: Why are we getting doubles in the first place?
  meta <- meta[1, ]
  div(
    class = 'button-box',
    style = 'background-color: #fff !important;',
    tags$p(tags$strong('Metric:'), meta$Metric), tags$br(),
    tags$p(tags$strong('Definition:'), meta$Definition), tags$br(),
    tags$p(tags$strong('Units:'), meta$Units), tags$br(),
    tags$p(tags$strong('Dimension:'), meta$Dimension), tags$br(),
    tags$p(tags$strong('Indicator:'), meta$Indicator), tags$br(),
    tags$p(tags$strong('Resolution:'), meta$Resolution), tags$br(),
    tags$p(tags$strong('Source:'), tags$a(meta$Source)), tags$br(),
    tags$p(tags$strong('Citation:'), meta$Citation)
  )
}


# Cascading Filter Utility Functions ==========================================

#' Get Available Years Range
#'
#' @description Get min and max years available in metadata, optionally filtered by resolution
#'
#' @param metadata The metadata data frame
#' @param resolution Optional resolution filter ('County', 'State', or NULL for all)
#'
#' @return Named vector with 'min' and 'max' year values
#' @importFrom dplyr filter pull
#' @importFrom stringr str_detect
#' @noRd
get_available_years_range <- function(metadata, resolution = NULL) {
  filtered_meta <- metadata

  if (!is.null(resolution)) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(stringr::str_detect(Resolution, resolution))
  }

  # Extract all years from Year Vector column (which is a list column)
  all_years <- unlist(filtered_meta$`Year Vector`)

  if (length(all_years) == 0) {
    return(c(min = 2000, max = 2024))  # Default fallback
  }

  c(min = min(all_years, na.rm = TRUE), max = max(all_years, na.rm = TRUE))
}


#' Filter Metadata by Year Range
#'
#' @description Filter metadata to only include metrics available within the year range
#'
#' @param metadata The metadata data frame
#' @param year_range Numeric vector of length 2 (min_year, max_year)
#'
#' @return Filtered metadata data frame
#' @importFrom purrr map_lgl
#' @noRd
filter_metadata_by_year <- function(metadata, year_range) {
  if (is.null(year_range) || length(year_range) != 2) {
    return(metadata)
  }

  # Check if any year in Year Vector falls within the range
  has_year_in_range <- purrr::map_lgl(metadata$`Year Vector`, function(years) {
    if (is.null(years) || length(years) == 0) return(FALSE)
    any(years >= year_range[1] & years <= year_range[2])
  })

  metadata[has_year_in_range, ]
}


#' Get Available Dimensions
#'
#' @description Get unique dimensions from metadata, optionally filtered by resolution and year range
#'
#' @param metadata The metadata data frame
#' @param resolution Optional resolution filter ('County', 'State', or NULL)
#' @param year_range Optional year range (numeric vector of length 2)
#'
#' @return Sorted character vector of unique dimensions
#' @importFrom dplyr filter pull
#' @importFrom stringr str_detect
#' @noRd
get_available_dimensions <- function(metadata, resolution = NULL, year_range = NULL) {
  filtered_meta <- metadata

  # Apply resolution filter
  if (!is.null(resolution)) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(
        stringr::str_detect(Resolution, resolution))
  }

  # Apply year range filter
  if (!is.null(year_range)) {
    filtered_meta <- filter_metadata_by_year(filtered_meta, year_range)
  }

  # Get unique dimensions
  filtered_meta %>%
    dplyr::pull(Dimension) %>%
    unique() %>%
    sort()
}


#' Get Available Indexes
#'
#' @description Get unique indexes from metadata, with hierarchical filtering
#'
#' @param metadata The metadata data frame
#' @param resolution Optional resolution filter
#' @param year_range Optional year range
#' @param dimensions Optional vector of dimensions to filter by
#'
#' @return Sorted character vector of unique indexes
#' @importFrom dplyr filter pull
#' @importFrom stringr str_detect
#' @noRd
get_available_indexes <- function(metadata, resolution = NULL, year_range = NULL, dimensions = NULL) {
  filtered_meta <- metadata

  # Apply resolution filter
  if (!is.null(resolution)) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(stringr::str_detect(Resolution, resolution))
  }

  # Apply year range filter
  if (!is.null(year_range)) {
    filtered_meta <- filter_metadata_by_year(filtered_meta, year_range)
  }

  # Apply dimension filter (multi-select support)
  if (!is.null(dimensions) && length(dimensions) > 0) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(Dimension %in% dimensions)
  }

  # Get unique indexes
  filtered_meta %>%
    dplyr::pull(Index) %>%
    unique() %>%
    sort()
}


#' Get Available Indicators
#'
#' @description Get unique indicators from metadata, with hierarchical filtering
#'
#' @param metadata The metadata data frame
#' @param resolution Optional resolution filter
#' @param year_range Optional year range
#' @param dimensions Optional vector of dimensions
#' @param indexes Optional vector of indexes
#'
#' @return Sorted character vector of unique indicators
#' @importFrom dplyr filter pull
#' @importFrom stringr str_detect
#' @noRd
get_available_indicators <- function(metadata, resolution = NULL, year_range = NULL,
                                    dimensions = NULL, indexes = NULL) {
  filtered_meta <- metadata

  # Apply resolution filter
  if (!is.null(resolution)) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(stringr::str_detect(Resolution, resolution))
  }

  # Apply year range filter
  if (!is.null(year_range)) {
    filtered_meta <- filter_metadata_by_year(filtered_meta, year_range)
  }

  # Apply dimension filter
  if (!is.null(dimensions) && length(dimensions) > 0) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(Dimension %in% dimensions)
  }

  # Apply index filter
  if (!is.null(indexes) && length(indexes) > 0) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(Index %in% indexes)
  }

  # Get unique indicators
  filtered_meta %>%
    dplyr::pull(Indicator) %>%
    unique() %>%
    sort()
}


#' Get Available Metrics
#'
#' @description Get unique metrics from metadata, with full hierarchical filtering
#'
#' @param metadata The metadata data frame
#' @param resolution Optional resolution filter
#' @param year_range Optional year range
#' @param dimensions Optional vector of dimensions
#' @param indexes Optional vector of indexes
#' @param indicators Optional vector of indicators
#'
#' @return Sorted character vector of unique metrics
#' @importFrom dplyr filter pull
#' @importFrom stringr str_detect
#' @noRd
get_available_metrics <- function(metadata, resolution = NULL, year_range = NULL,
                                 dimensions = NULL, indexes = NULL, indicators = NULL) {
  filtered_meta <- metadata

  # Apply resolution filter
  if (!is.null(resolution)) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(stringr::str_detect(Resolution, resolution))
  }

  # Apply year range filter
  if (!is.null(year_range)) {
    filtered_meta <- filter_metadata_by_year(filtered_meta, year_range)
  }

  # Apply dimension filter
  if (!is.null(dimensions) && length(dimensions) > 0) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(Dimension %in% dimensions)
  }

  # Apply index filter
  if (!is.null(indexes) && length(indexes) > 0) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(Index %in% indexes)
  }

  # Apply indicator filter
  if (!is.null(indicators) && length(indicators) > 0) {
    filtered_meta <- filtered_meta %>%
      dplyr::filter(Indicator %in% indicators)
  }

  # Get unique metrics
  filtered_meta %>%
    dplyr::pull(Metric) %>%
    unique() %>%
    sort()
}


#' Get Variable Names from Metrics
#'
#' @description Map metric display names to database variable names (vectorized)
#'
#' @param metadata The metadata data frame
#' @param metrics Character vector of metric names
#'
#' @return Character vector of variable names
#' @importFrom dplyr filter pull
#' @noRd
get_variable_names <- function(metadata, metrics) {
  if (is.null(metrics) || length(metrics) == 0) {
    return(character(0))
  }

  metadata %>%
    dplyr::filter(Metric %in% metrics) %>%
    dplyr::pull(`Variable Name`) %>%
    unique()
}


#' Get Available States with Data
#'
#' @description Query database to find which states have data
#'
#' @param con Database connection
#' @param resolution Resolution ('County' or 'State')
#' @param variable_names Optional character vector of variable names to filter by
#' @param year_range Numeric vector of length 2 (min_year, max_year)
#'
#' @return Character vector of state names (sorted)
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue glue_collapse
#' @noRd
get_available_states <- function(con, resolution, variable_names = NULL, year_range) {
  table <- paste0('neast_', tolower(resolution), '_metrics')

  # Build WHERE clause
  where_clauses <- c(
    glue::glue("year >= {year_range[1]} AND year <= {year_range[2]}")
  )

  # Add variable name filter if provided
  if (!is.null(variable_names) && length(variable_names) > 0) {
    var_sql <- paste0("'", variable_names, "'", collapse = ", ")
    where_clauses <- c(where_clauses, glue::glue("variable_name IN ({var_sql})"))
  }

  where_sql <- paste(where_clauses, collapse = " AND ")

  query <- glue::glue(
    "SELECT DISTINCT SUBSTR(fips, 1, 2) as state_fips
     FROM {table}
     WHERE {where_sql}
     ORDER BY state_fips"
  )

  result <- query_db(con, query)

  if (nrow(result) == 0) {
    return(character(0))
  }

  # Load fips_key to get state names
  fips_key <- readRDS('data/fips_key.rds')

  # Get unique state names from state FIPS codes
  state_fips_codes <- unique(result$state_fips)

  states <- fips_key %>%
    dplyr::filter(substr(fips, 1, 2) %in% state_fips_codes) %>%
    dplyr::pull(state_name) %>%
    unique() %>%
    sort()

  states
}


#' Get Available Counties with Data
#'
#' @description Query database to find which counties have data for selected states
#'
#' @param con Database connection
#' @param states Character vector of state names
#' @param variable_names Optional character vector of variable names to filter by
#' @param year_range Numeric vector of length 2
#'
#' @return Character vector of county names (sorted)
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr filter pull
#' @importFrom glue glue
#' @noRd
get_available_counties <- function(con, states, variable_names = NULL, year_range) {
  if (is.null(states) || length(states) == 0) {
    return(character(0))
  }

  # Load fips_key to get FIPS codes for selected states
  fips_key <- readRDS('data/fips_key.rds')

  state_fips <- fips_key %>%
    dplyr::filter(state_name %in% states) %>%
    dplyr::pull(fips) %>%
    substr(1, 2) %>%
    unique()

  if (length(state_fips) == 0) {
    return(character(0))
  }

  # Build WHERE clause
  state_sql <- paste0("'", state_fips, "'", collapse = ", ")
  where_clauses <- c(
    glue::glue("year >= {year_range[1]} AND year <= {year_range[2]}"),
    glue::glue("SUBSTR(fips, 1, 2) IN ({state_sql})")
  )

  # Add variable name filter if provided
  if (!is.null(variable_names) && length(variable_names) > 0) {
    var_sql <- paste0("'", variable_names, "'", collapse = ", ")
    where_clauses <- c(where_clauses, glue::glue("variable_name IN ({var_sql})"))
  }

  where_sql <- paste(where_clauses, collapse = " AND ")

  query <- glue::glue(
    "SELECT DISTINCT fips
     FROM neast_county_metrics
     WHERE {where_sql}
     ORDER BY fips"
  )

  result <- query_db(con, query)

  if (nrow(result) == 0) {
    return(character(0))
  }

  # Get county names
  counties <- fips_key %>%
    dplyr::filter(fips %in% result$fips) %>%
    dplyr::pull(county_name) %>%
    unique() %>%
    sort()

  counties
}


#' Query Metric Data in Bulk
#'
#' @description Query metrics from database with optional filtering
#'
#' @param con Database connection
#' @param variable_names Optional character vector of variable names (NULL = all metrics)
#' @param resolution Resolution ('County' or 'State')
#' @param year_range Numeric vector of length 2
#' @param states Optional character vector of state names
#' @param counties Optional character vector of county names
#'
#' @return Data frame with columns: fips, year, value, variable_name
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @noRd
query_metric_data_bulk <- function(con, 
                                   variable_names = NULL, 
                                   resolution, 
                                   year_range,
                                   states = NULL, 
                                   counties = NULL) {
  # Determine which table to query from
  # TODO: probably worth combining these into one?
  table <- paste0('neast_', tolower(resolution), '_metrics')

  # Build WHERE clauses - start with year filter
  where_clauses <- c(
    glue::glue("year >= {year_range[1]} AND year <= {year_range[2]}")
  )

  # Add variable name filter if provided
  if (!is.null(variable_names) && length(variable_names) > 0) {
    var_sql <- paste0("'", variable_names, "'", collapse = ", ")
    where_clauses <- c(where_clauses, glue::glue("variable_name IN ({var_sql})"))
  }

  # Add geography filter if provided
  if (!is.null(states) || !is.null(counties)) {
    fips_key <- readRDS('data/fips_key.rds')

    if (!is.null(counties) && length(counties) > 0) {
      # Filter by specific counties
      fips_codes <- fips_key %>%
        dplyr::filter(county_name %in% counties) %>%
        dplyr::pull(fips)

      if (length(fips_codes) > 0) {
        fips_sql <- paste0("'", fips_codes, "'", collapse = ", ")
        where_clauses <- c(where_clauses, glue::glue("fips IN ({fips_sql})"))
      }
    } else if (!is.null(states) && length(states) > 0) {
      # Filter by states
      state_fips <- fips_key %>%
        dplyr::filter(state_name %in% states) %>%
        dplyr::pull(fips) %>%
        substr(1, 2) %>%
        unique()

      if (length(state_fips) > 0) {
        state_sql <- paste0("'", state_fips, "'", collapse = ", ")
        where_clauses <- c(where_clauses, glue::glue("SUBSTR(fips, 1, 2) IN ({state_sql})"))
      }
    }
  }

  where_sql <- paste(where_clauses, collapse = " AND ")

  query <- glue::glue(
    "SELECT fips, year, value, variable_name
     FROM {table}
     WHERE {where_sql}
     ORDER BY variable_name, fips, year"
  )

  query_db(con, query)
}