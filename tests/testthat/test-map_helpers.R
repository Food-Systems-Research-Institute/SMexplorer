# create_base_map ---------------------------------------------------------

test_that("create_base_map creates valid leaflet map object", {
  map <- create_base_map(neast_county_spatial_2024, resolution = 'County')

  # Test object classes
  expect_s3_class(map, 'leaflet')
  expect_s3_class(map, 'htmlwidget')

  # Test that map has expected components
  expect_true(!is.null(map$x))  # Map has configuration
  expect_true(!is.null(map$x$calls))  # Map has layer calls

  # Check that provider tiles were added (should have multiple addProviderTiles calls)
  tile_calls <- sapply(map$x$calls, function(x) x$method == "addProviderTiles")
  expect_true(sum(tile_calls) == 2)  # OpenStreetMap, USGS

  # Check that polygons were added
  polygon_calls <- sapply(map$x$calls, function(x) x$method == "addPolygons")
  expect_true(sum(polygon_calls) >= 1)

  # Check that layer control was added
  control_calls <- sapply(map$x$calls, function(x) x$method == "addLayersControl")
  expect_true(sum(control_calls) >= 1)
})

test_that("create_base_map works with County resolution", {
  map <- create_base_map(neast_county_spatial_2024, resolution = 'County')

  expect_s3_class(map, 'leaflet')
  expect_true(!is.null(map$x$calls))
})

test_that("create_base_map works with State resolution", {
  map <- create_base_map(neast_county_spatial_2024, resolution = 'State')

  expect_s3_class(map, 'leaflet')
  expect_true(!is.null(map$x$calls))
})


# custom_popup tests ----

test_that("custom_popup formats county-level data correctly", {
  result <- custom_popup(
    county_name = "Test County",
    state_name = "Test State",
    variable_name = "Test Variable",
    value = 123.456,
    metric = "Test Metric"
  )

  expect_type(result, "character")
  expect_match(result, "Test County")
  expect_match(result, "Test Metric")
  expect_match(result, "123.46")  # Rounded to 2 decimals
})

test_that("custom_popup handles state-level data (NA county)", {
  result <- custom_popup(
    county_name = NA,
    state_name = "Test State",
    variable_name = "Test Variable",
    value = 789.123,
    metric = "Test Metric"
  )

  expect_type(result, "character")
  expect_match(result, "Test State")
  expect_match(result, "Test Metric")
  expect_match(result, "789.12")
})

test_that("custom_popup rounds values correctly", {
  result <- custom_popup(
    county_name = "County",
    state_name = "State",
    variable_name = "Variable",
    value = 0.00123,
    metric = "Metric"
  )

  expect_match(result, "0")  # Rounds to 2 decimals
})


# get_map_formulas --------------------------------------------------------

test_that("get_map_formulas returns correct structure for County", {
  formulas <- get_map_formulas('County', 'Test Metric')

  expect_true(is.list(formulas))
  expect_named(formulas, c("popup", "label"))
  expect_s3_class(formulas$popup, "formula")
  expect_s3_class(formulas$label, "formula")
})

test_that("get_map_formulas returns correct structure for State", {
  formulas <- get_map_formulas('State', 'Test Metric')

  expect_true(is.list(formulas))
  expect_named(formulas, c("popup", "label"))
  expect_s3_class(formulas$popup, "formula")
  expect_s3_class(formulas$label, "formula")
})

test_that("get_map_formulas County uses county_name", {
  formulas <- get_map_formulas('County', 'Test Metric')

  # Label should reference county_name
  label_text <- as.character(formulas$label)
  expect_match(label_text[1], "~")
  expect_match(label_text[2], "county_name")
})

test_that("get_map_formulas State uses state_name", {
  formulas <- get_map_formulas('State', 'Test Metric')

  # Label should reference state_name
  label_text <- as.character(formulas$label)
  expect_match(label_text[1], "~")
  expect_match(label_text[2], "state_name")
})


# validate_map_data -------------------------------------------------------

test_that("validate_map_data returns valid for good data", {
  test_data <- create_mock_map_data(values = c(1, 2, 3, 4, 5))

  result <- validate_map_data(
    test_data,
    metric_name = "Test Metric",
    year = 2024,
    resolution = "County"
  )

  expect_true(result$valid)
  expect_null(result$message)
  expect_null(result$type)
})

test_that("validate_map_data errors when value column missing", {
  test_data <- create_mock_map_data(values = c(1, 2, 3))
  # Remove value column
  test_data$value <- NULL

  result <- validate_map_data(
    test_data,
    metric_name = "Test Metric",
    year = 2024,
    resolution = "County"
  )

  expect_false(result$valid)
  expect_match(result$message, "No data found")
  expect_equal(result$type, "error")
})

test_that("validate_map_data errors when all values are NA", {
  test_data <- create_mock_map_data(values = c(NA, NA, NA, NA))

  result <- validate_map_data(
    test_data,
    metric_name = "Test Metric",
    year = 2024,
    resolution = "County"
  )

  expect_false(result$valid)
  expect_match(result$message, "No valid data available")
  expect_match(result$message, "All values are missing")
  expect_equal(result$type, "error")
})

test_that("validate_map_data warns when many values are NA", {
  test_data <- create_mock_map_data(
    values = c(1, NA, NA, NA, NA, NA, NA, NA, NA, NA)  # 10% valid
  )

  result <- validate_map_data(
    test_data,
    metric_name = "Test Metric",
    year = 2024,
    resolution = "County",
    warn_threshold = 0.2
  )

  expect_true(result$valid)
  expect_match(result$message, "Warning")
  expect_match(result$message, "90% of values are missing")
  expect_equal(result$type, "warning")
})

test_that("validate_map_data passes with exactly threshold valid data", {
  test_data <- create_mock_map_data(
    values = c(1, 2, NA, NA, NA)  # 40% valid (above 20% threshold)
  )

  result <- validate_map_data(
    test_data,
    metric_name = "Test Metric",
    year = 2024,
    resolution = "County",
    warn_threshold = 0.2
  )

  expect_true(result$valid)
  expect_null(result$message)
  expect_null(result$type)
})

test_that("validate_map_data respects custom warn_threshold", {
  test_data <- create_mock_map_data(
    values = c(1, 2, 3, NA, NA, NA, NA, NA, NA, NA)  # 30% valid
  )

  result <- validate_map_data(
    test_data,
    metric_name = "Test Metric",
    year = 2024,
    resolution = "County",
    warn_threshold = 0.5  # Require 50% valid data
  )

  expect_true(result$valid)
  expect_match(result$message, "Warning")
  expect_equal(result$type, "warning")
})
