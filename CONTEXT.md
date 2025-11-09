## File Structure
- `R/` 
    - Contains modules (`mod_*`) which are currently set up like page views. Each corresponds to a tab in the sidebar. 
    - Also contains functions (`fct_*`) that generally correspond to modules. For example, (`fct_map_helpers.R`) contains functions for the map module.
    - There are also some loose utils and other functions strewn about here currently. Should clean this up eventually.

## Operations
- Run app locally with `r()`. This will load the project using `devtools::load_all()` and then run the app with `Shiny::run_app()`.

## Database
- DuckDB database file saved in `data/`. This is not checked into git.
- Script to create database from `SMdocs` package is `dev/create_duckdb.R`. Run this after pulling to update local database.
- `R/db_utils.R` contains functions for connecting and querying database. 
- `create_db_connection()` is called in the `R/app_server.R` script and the `con` is passed to all servers from there. 

## Styling
- Modules should call only the functions required, not entire packages. For example, `@importFrom dplyr select`. 
