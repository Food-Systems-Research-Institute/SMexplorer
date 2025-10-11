options(
  cli.ignore_unknown_rstudio_theme = TRUE,
  shiny.autoreload = TRUE
)

# Activate renv
source("renv/activate.R")

# Shortcut to vim configuration
try(vim <- function() rstudiovim::rsvim_exec_file(rstudiovim::rsvim_default_path()))

devtools::load_all()
