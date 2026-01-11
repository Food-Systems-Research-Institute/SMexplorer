<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/Food-Systems-Research-Institute/SMexplorer/graph/badge.svg)](https://app.codecov.io/gh/Food-Systems-Research-Institute/SMexplorer)
<!-- badges: end -->

# Sustainability Metrics Explorer

## About
 
The Sustainability Metrics project is a collaborative effort to measure food system sustainability in the Northeast led by the University of Vermont Food Systems Research Institute and the USDA ARS in Burlington, Vermont.

This repository houses an R Shiny Dashboard app that allows for interactive exploration of metrics and spatial data. It follows the [golem](https://github.com/ThinkR-open/golem/tree/master) framework for Shiny app development. You can find the app deployed [here](https://fsrc.w3.uvm.edu/SMexplorer/).

## Cloning

```{bash}
git clone https://github.com/Food-Systems-Research-Institute/SMexplorer.git
cd SMexplorer
```

```[r}
renv::restore()
```

## File Structure
- `R/` :
    - Contains modules (`mod_*`) which are currently set up like page views. Each corresponds to a tab in the sidebar. 
    - Also contains functions (`fct_*`) that generally correspond to modules. For example, (`fct_map_helpers.R`) contains functions for the map module.
    - There are also some loose utils and other functions strewn about here currently. Should clean this up eventually.
- `man/`: Function and module documentation
- `DESCRIPTION`: Package info and dependencies
- `tests/`: Test suite with `testthat` and `shinytest2`

## Workflow
- Load package with `devtools::load_all()` (`ctrl+shift+l`)
- Run app with `SMexplorer::run_app()` 
  - A shortcut for `load_all()` and `run_app()` is `r()`. This is convenient when making and checking changes.
- Run tests with `ctrl+shift+t`
- Render documentation with `ctrl+shift+d`
- Run check with `ctrl+shift+e`

## Database
- DuckDB database file saved in `data/`. This is not checked into git.
- Script to create database from `SMdocs` package is `dev/create_duckdb.R`. Run this after pulling to update local database.
- `R/db_utils.R` contains functions for connecting and querying database. 
- `create_db_connection()` is called in the `R/app_server.R` script and the `con` is passed to all servers from there. 
