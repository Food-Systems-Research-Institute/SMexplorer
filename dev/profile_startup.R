# Profile app startup to identify bottlenecks

pacman::p_load(tictoc)

# Package loading
cat("1. Package loading time:\n")
tic("Total pkgload::load_all()")
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
toc()

cat("\nIndividual package imports\n")
heavy_packages <- c("sf", "leaflet", "plotly", "shiny", "dplyr")
for (pkg in heavy_packages) {
  if (pkg %in% loadedNamespaces()) {
    cat(sprintf("  %s: already loaded\n", pkg))
  } else {
    tic(sprintf("  %s", pkg))
    library(pkg, character.only = TRUE)
    toc()
  }
}

cat("\nData file loading time:\n")

# Test .rda loading
tic("  js.rda")
load('data/js.rda')
toc()

tic("  metadata.rda")
load('data/metadata.rda')
toc()

tic("  neast_county_metrics.rda")
load('data/neast_county_metrics.rda')
toc()

tic("  neast_counties_2024.rda")
load('data/neast_counties_2024.rda')
toc()

tic("  sm_data.rda")
load('data/sm_data.rda')
toc()
