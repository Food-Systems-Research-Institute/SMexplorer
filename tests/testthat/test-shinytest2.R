library(shinytest2)

test_that("{shinytest2} recording: launch", {
  skip("Integration test - run manually with testthat::test_file()")

  local_app_support(test_path("../.."))
  app <- AppDriver$new(test_path("../.."), variant = platform_variant(), name = "SMexplorer",
      height = 854, width = 1529)
  app$set_inputs(`details-select_state` = "Vermont")
  app$set_inputs(`details-search_metric` = "Overall food insecurity rate")
  app$set_inputs(`details-search_county` = "Chittenden County")
  app$set_window_size(width = 1163, height = 824)
  app$expect_screenshot()
})

test_that("{shinytest2} recording: mod_map", {
  skip("Integration test - run manually with testthat::test_file()")

  local_app_support(test_path("../.."))
  app <- AppDriver$new(test_path("../.."), name = "mod_map", height = 702, width = 1163)
  app$set_inputs(`details-select_state` = "Vermont")
  app$set_inputs(`details-search_metric` = "Overall food insecurity rate")
  app$set_inputs(`details-search_county` = "Chittenden County")
  app$set_window_size(width = 1153, height = 702)
  app$set_inputs(`map_plot-metric` = "Broadband access")
  app$set_inputs(`map_plot-year` = "2025")
  app$expect_values()
})

