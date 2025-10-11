pacman::p_load(fresh)
fresh_theme <- fresh::create_theme(
  adminlte_color(
    green = '#243f3f'
  ),
  adminlte_sidebar(
    dark_bg = '#333333'
  ),
  output_file = 'inst/app/www/fresh_theme.css'
)
