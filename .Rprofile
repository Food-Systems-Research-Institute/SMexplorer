options(
  cli.ignore_unknown_rstudio_theme = TRUE,
  shiny.autoreload = TRUE # this doesn't work though
)

# Activate renv
source("renv/activate.R")

# Shortcut to vim configuration
# try(vim <- function() rstudiovim::rsvim_exec_file(rstudiovim::rsvim_default_path()))

# Load package
devtools::load_all()

# Shortcut run function
try(
    r <- function() {
      devtools::load_all()
      SMexplorer::run_app()
    }
)

# To help remember
theme_green <- '#154734'
