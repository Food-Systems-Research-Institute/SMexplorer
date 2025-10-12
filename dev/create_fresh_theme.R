pacman::p_load(fresh)
fresh_theme <- fresh::create_theme(
  # adminlte_color(
  #   green = '#154734' # Set green skin to our theme green
  # ),
  adminlte_sidebar(
    dark_bg = 'white',
    dark_hover_bg = '#222222',
    dark_hover_color = 'red',
    dark_submenu_bg = 'blue',
    dark_submenu_color = 'green',

    light_hover_bg = 'purple',
    light_submenu_bg = 'orange',
    light_submenu_color = 'pink'
  ),
  output_file = 'inst/app/www/fresh_theme.css'
)
