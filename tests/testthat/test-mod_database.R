# Cascading Filters -------------------------------------------------------

test_that("mod_database_server filters dimensions by resolution and year", {
  mock_metadata <- get_mock_metadata()

  global_data <- list(
    metadata = mock_metadata,
    fips_key = data.frame(fips = "01001", state_name = "Test State", county_name = "Test County")
  )

  shiny::testServer(
    mod_database_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(),
      global_data = global_data
    ),
    {
      session$setInputs(resolution = "County", year_range = c(2015, 2023))
      session$flushReact()

      dimensions <- available_dimensions()

      expect_type(dimensions, "character")
      expect_true(length(dimensions) > 0)
      # Should include at least the 5 dimensions we sampled
      expect_true(any(c("Economics", "Production", "Social", "Health", "Environment") %in% dimensions))
    }
  )
})


test_that("mod_database_server filters indexes based on dimension selection", {
  mock_metadata <- get_mock_metadata()

  global_data <- list(
    metadata = mock_metadata,
    fips_key = data.frame(fips = "01001", state_name = "Test", county_name = "Test")
  )

  shiny::testServer(
    mod_database_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(),
      global_data = global_data
    ),
    {
      session$setInputs(
        resolution = "County",
        year_range = c(2015, 2023),
        dimension = c("Economics")  # Select only Economics
      )
      session$flushReact()

      indexes <- available_indexes()

      expect_type(indexes, "character")
      expect_true(length(indexes) > 0)
      # All returned indexes should be from Economics dimension
      econ_indexes <- mock_metadata |>
        dplyr::filter(Dimension == "Economics") |>
        dplyr::pull(Index) |>
        unique()
      expect_true(all(indexes %in% econ_indexes))
    }
  )
})


test_that("mod_database_server filters indicators based on index selection", {
  mock_metadata <- get_mock_metadata()

  # Get a valid index from Economics dimension
  econ_index <- mock_metadata |>
    dplyr::filter(Dimension == "Economics") |>
    dplyr::pull(Index) |>
    unique() |>
    head(1)

  global_data <- list(
    metadata = mock_metadata,
    fips_key = data.frame(fips = "01001", state_name = "Test", county_name = "Test")
  )

  shiny::testServer(
    mod_database_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(),
      global_data = global_data
    ),
    {
      session$setInputs(
        resolution = "County",
        year_range = c(2015, 2023),
        dimension = "Economics",
        index = econ_index
      )
      session$flushReact()

      indicators <- available_indicators()

      expect_type(indicators, "character")
      expect_true(length(indicators) > 0)
      # All indicators should come from selected dimension and index
      expected_indicators <- mock_metadata |>
        dplyr::filter(Dimension == "Economics", Index == econ_index) |>
        dplyr::pull(Indicator) |>
        unique()
      expect_true(all(indicators %in% expected_indicators))
    }
  )
})


test_that("mod_database_server filters metrics through full cascade", {
  mock_metadata <- get_mock_metadata()

  # Get valid cascade values from Economics that have County resolution
  econ_data <- mock_metadata |>
    dplyr::filter(
      Dimension == "Economics",
      stringr::str_detect(Resolution, "County")
    )

  # Get year range from the data
  all_years <- unlist(econ_data$`Year Vector`)
  test_year_range <- c(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE))

  test_index <- econ_data |> dplyr::pull(Index) |> unique() |> head(1)
  test_indicator <- econ_data |>
    dplyr::filter(Index == test_index) |>
    dplyr::pull(Indicator) |>
    unique() |>
    head(1)

  global_data <- list(
    metadata = mock_metadata,
    fips_key = data.frame(fips = "01001", state_name = "Test", county_name = "Test")
  )

  shiny::testServer(
    mod_database_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(),
      global_data = global_data
    ),
    {
      session$setInputs(
        resolution = "County",
        year_range = test_year_range,
        dimension = "Economics",
        index = test_index,
        indicator = test_indicator
      )
      session$flushReact()

      metrics <- available_metrics()

      expect_type(metrics, "character")
    }
  )
})


test_that("mod_database_server extracts variable names from metrics", {
  mock_metadata <- get_mock_metadata()

  # Get some actual metrics from the metadata
  test_metrics <- mock_metadata |> dplyr::pull(Metric) |> head(2)
  expected_vars <- mock_metadata |>
    dplyr::filter(Metric %in% test_metrics) |>
    dplyr::pull(`Variable Name`)

  global_data <- list(
    metadata = mock_metadata,
    fips_key = data.frame(fips = "01001", state_name = "Test", county_name = "Test")
  )

  shiny::testServer(
    mod_database_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(),
      global_data = global_data
    ),
    {
      session$setInputs(metric = test_metrics)
      session$flushReact()

      var_names <- selected_variable_names()

      expect_type(var_names, "character")
      expect_length(var_names, length(expected_vars))
      expect_setequal(var_names, expected_vars)
    }
  )
})


test_that("mod_database_server handles multiple dimension selection", {
  mock_metadata <- get_mock_metadata()

  global_data <- list(
    metadata = mock_metadata,
    fips_key = data.frame(fips = "01001", state_name = "Test", county_name = "Test")
  )

  shiny::testServer(
    mod_database_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(),
      global_data = global_data
    ),
    {
      session$setInputs(
        resolution = "County",
        year_range = c(2015, 2023),
        dimension = c("Economics", "Health")  # Multiple selections
      )
      session$flushReact()

      indexes <- available_indexes()

      expect_type(indexes, "character")
      expect_true(length(indexes) > 0)
      # All indexes should come from either Economics or Health
      expected_indexes <- mock_metadata |>
        dplyr::filter(Dimension %in% c("Economics", "Health")) |>
        dplyr::pull(Index) |>
        unique()
      expect_true(all(indexes %in% expected_indexes))
    }
  )
})


test_that("mod_database_server returns empty when no metrics match filters", {
  mock_metadata <- data.frame(
    Metric = c("M1", "M2"),
    Dimension = c("Economics", "Environment"),
    Index = c("I1", "I2"),
    Indicator = c("Ind1", "Ind2"),
    Resolution = rep("County", 2),
    `Variable Name` = c("var1", "var2"),
    `Year Vector` = I(list(2015:2020, 2015:2020)),
    Units = rep("units", 2),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  global_data <- list(
    metadata = mock_metadata,
    fips_key = data.frame(fips = "01001", state_name = "Test", county_name = "Test")
  )

  shiny::testServer(
    mod_database_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(),
      global_data = global_data
    ),
    {
      session$setInputs(
        resolution = "County",
        year_range = c(2015, 2020),
        dimension = "Health"  # No metrics with this dimension
      )
      session$flushReact()

      metrics <- available_metrics()

      expect_length(metrics, 0)
    }
  )
})


test_that("mod_database_server handles State resolution filtering", {
  mock_metadata <- data.frame(
    Metric = c("County Only", "State Only", "Both"),
    Dimension = rep("Economics", 3),
    Index = rep("I1", 3),
    Indicator = rep("Ind1", 3),
    Resolution = c("County", "State", "County, State"),
    `Variable Name` = c("var1", "var2", "var3"),
    `Year Vector` = I(list(2015:2020, 2015:2020, 2015:2020)),
    Units = rep("units", 3),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  global_data <- list(
    metadata = mock_metadata,
    fips_key = data.frame(fips = "01", state_name = "Test State", county_name = NA)
  )

  shiny::testServer(
    mod_database_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(),
      global_data = global_data
    ),
    {
      session$setInputs(
        resolution = "State",
        year_range = c(2015, 2020)
      )
      session$flushReact()

      metrics <- available_metrics()

      expect_length(metrics, 2)
      expect_false("County Only" %in% metrics)
      expect_true("State Only" %in% metrics)
      expect_true("Both" %in% metrics)
    }
  )
})

