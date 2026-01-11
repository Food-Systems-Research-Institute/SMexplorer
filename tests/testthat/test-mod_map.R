# Tests for mod_map server logic

test_that("mod_map_server updates metric choices based on resolution and dimension", {
  # Create mock global_data
  mock_metadata <- data.frame(
    Metric = c("Metric A", "Metric B", "Metric C", "Metric D"),
    Dimension = c("Economics", "Economics", "Economics", "Health"),
    Resolution = c("County", "State", "County, State", "County"),
    `Variable Name` = c("var_a", "var_b", "var_c", "var_d"),
    `Year Vector` = I(list(2020:2022, 2020:2023, 2019:2022, 2021:2023)),
    Definition = rep("Test definition", 4),
    Units = rep("units", 4),
    Indicator = rep("indicator", 4),
    Source = rep("source", 4),
    Citation = rep("citation", 4),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  global_data <- list(metadata = mock_metadata)

  # Mock database connection
  mock_con <- NULL

  shiny::testServer(
    mod_map_server,
    args = list(
      con = mock_con,
      parent_input = shiny::reactiveValues(tabs = "map_tab"),
      global_data = global_data
    ),
    {
      # Skip initial data loading (would require actual qs files)
      session$setInputs(resolution = "County", dimension = "Economics")

      # Manually set map_data_loaded to TRUE to bypass data loading
      map_data_loaded(TRUE)

      # Test that available_metrics filters correctly
      session$flushReact()
      metrics <- available_metrics()

      expect_type(metrics, "character")
      expect_true("Metric A" %in% metrics)  # County + Economics
      expect_true("Metric C" %in% metrics)  # County, State + Economics (matches both)
      expect_false("Metric B" %in% metrics) # State only (no County match)
      expect_false("Metric D" %in% metrics) # Health dimension (wrong dimension)
    }
  )
})


test_that("mod_map_server filters metrics by dimension", {
  mock_metadata <- data.frame(
    Metric = c("Econ Metric", "Env Metric", "Health Metric"),
    Dimension = c("Economics", "Environment", "Health"),
    Resolution = rep("County", 3),
    `Variable Name` = c("econ_var", "env_var", "health_var"),
    `Year Vector` = I(list(2020:2022, 2020:2022, 2020:2022)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  global_data <- list(metadata = mock_metadata)

  shiny::testServer(
    mod_map_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(tabs = "map_tab"),
      global_data = global_data
    ),
    {
      session$setInputs(resolution = "County", dimension = "Environment")
      map_data_loaded(TRUE)
      session$flushReact()

      metrics <- available_metrics()

      expect_length(metrics, 1)
      expect_equal(metrics, "Env Metric")
    }
  )
})


test_that("mod_map_server extracts available years for selected metric", {
  mock_metadata <- data.frame(
    Metric = c("Metric A", "Metric B"),
    Dimension = rep("Economics", 2),
    Resolution = rep("County", 2),
    `Variable Name` = c("var_a", "var_b"),
    `Year Vector` = I(list(c(2015, 2018, 2020), c(2019, 2021, 2022, 2023))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  global_data <- list(metadata = mock_metadata)

  shiny::testServer(
    mod_map_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(tabs = "map_tab"),
      global_data = global_data
    ),
    {
      session$setInputs(metric = "Metric B")
      map_data_loaded(TRUE)
      session$flushReact()

      years <- available_years()

      expect_equal(years, c(2019, 2021, 2022, 2023))
    }
  )
})


test_that("mod_map_server extracts correct variable name", {
  mock_metadata <- data.frame(
    Metric = c("Test Metric"),
    Dimension = "Economics",
    Resolution = "County",
    `Variable Name` = "expected_variable_name",
    `Year Vector` = I(list(2020:2022)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  global_data <- list(metadata = mock_metadata)

  shiny::testServer(
    mod_map_server,
    args = list(
      con = NULL,
      parent_input = shiny::reactiveValues(tabs = "map_tab"),
      global_data = global_data
    ),
    {
      session$setInputs(metric = "Test Metric")
      map_data_loaded(TRUE)
      session$flushReact()

      var_name <- selected_variable()

      expect_equal(var_name, "expected_variable_name")
    }
  )
})


# Note: Testing map_data() reactive would require:
# 1. Mock database connection with query_db() function
# 2. Mock spatial data (neast_county_spatial_2024, etc.)
# 3. More complex setup
# Consider this if database logic needs testing

# Note: Testing observeEvent(input$update_map) would require:
# 1. All of the above
# 2. Mocking leafletProxy
# 3. Testing that validation runs correctly
# This might be better suited for integration tests with shinytest2
