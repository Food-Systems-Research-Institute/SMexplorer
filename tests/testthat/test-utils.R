test_that("get_metric_info works", {
  meta <- get_mock_metadata()
  div <- get_metric_info(meta)
  
  expect_s3_class(div, 'shiny.tag')
  expect_match(div$name, 'div')
  expect_match(div$attribs$class, 'button-box')
})

test_that("get_available_years_range for County", {
  meta <- get_mock_metadata()
  range <- get_available_years_range(meta, resolution = 'County')
  
  expect_type(range, 'integer')
  expect_true(length(range) == 2)
})

test_that("get_available_years_range for State", {
  meta <- get_mock_metadata()
  range <- get_available_years_range(meta, resolution = 'State')
  
  expect_type(range, 'integer')
  expect_true(length(range) == 2)
})

test_that("get_available_years_range for bad data", {
  meta <- tibble(
    Resolution = c(rep('County', 3), rep('State', 3)),
    `Year Vector` = rep(NA, 6)
  )
  
  expect_error(
    get_available_years_range(meta),
    'Year Vector column was not a list'
  )
})

test_that("get_available_indicators works", {
  meta <- get_mock_metadata()
  indicators <- get_available_indicators(
    metadata = meta,
    resolution = 'County',
    year_range = c(2000, 2025),
    dimensions = c('Economics', 'Environment', 'Production')
  )
  
  expect_type(indicators, 'character')
  expect_true(length(indicators) > 1)
  expect_true('Biodiversity' %in% indicators)
})

test_that("get_available_indexes works", {
  meta <- get_mock_metadata()
  indexes <- get_available_indexes(
    metadata = meta,
    resolution = 'County',
    year_range = c(2000, 2025),
    dimensions = c('Economics', 'Environment', 'Production', 'Health')
  )
  
  expect_type(indexes, 'character')
  expect_true('Water' %in% indexes)
  expect_true(length(indexes) > 1)
})
