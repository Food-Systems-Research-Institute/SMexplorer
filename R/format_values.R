#' format values
#' 2024-11-07
#'
#' takes the value column and formats based on units to print nicely

pacman::p_load(
  dplyr,
  stringr
)

format_values <- function(df,
                          units_col = "units",
                          value_col = "value") {
  # out <- df %>%
  #   mutate(
  #     !!sym(units_col) := case_when(
  #       !!sym(units_col) %in% c('count', 'acres', 'usd') & as.numeric(!!sym(value_col)) > 1e6 ~ paste(!!sym(units_col), '(x1000)'),
  #       .default = !!sym(units_col)
  #     ),
  #     value = case_when(
  #       str_detect(!!sym(units_col), 'x1000') ~ as.character(round(as.numeric(!!sym(value_col)) / 1000), 0),
  #       str_detect(!!sym(units_col), 'count|acres|usd') ~ format(round(as.numeric(!!sym(value_col)), 0), big.mark = ','),
  #       # str_detect(!!sym(units_col), 'usd') ~ paste0('\\$', !!sym(value_col)),
  #       str_detect(!!sym(units_col), 'usd') ~ paste0('$', format(as.numeric(!!sym(value_col)), big.mark = ',')),
  #       !!sym(units_col) == 'binary' ~ ifelse(!!sym(value_col) == 1, 'True', 'False'),
  #       !!sym(units_col) == 'age' ~ paste(!!sym(value_col), 'years'),
  #       !!sym(units_col) == 'percentage' ~ paste0('%', !!sym(value_col)),
  #       .default = !!sym(value_col)
  #     ) %>%
  #       format(justify = 'left') %>%
  #       str_trim()
  #   )

  out <- df %>%
    mutate(
      !!sym(units_col) := case_when(
        !!sym(units_col) %in% c("count", "acres", "usd") & as.numeric(!!sym(value_col)) > 1e6 ~ paste(!!sym(units_col), "x1000"),
        .default = !!sym(units_col)
      ),
      value = case_when(
        str_detect(!!sym(units_col), "x1000") ~ format(round(as.numeric(!!sym(value_col)) / 1000), scientific = FALSE),
        str_detect(!!sym(units_col), "count|acres|usd") ~ format(round(as.numeric(!!sym(value_col)), 0), big.mark = ",", scientific = FALSE),
        str_detect(!!sym(units_col), "usd") ~ paste0("\\$", format(round(as.numeric(!!sym(value_col)), 0), big.mark = ",", scientific = FALSE)),
        !!sym(units_col) == "binary" ~ ifelse(!!sym(value_col) == 1, "True", "False"),
        !!sym(units_col) == "age" ~ paste(!!sym(value_col), "years"),
        !!sym(units_col) == "percentage" ~ paste0(!!sym(value_col), "%"),
        .default = as.character(!!sym(value_col))
      ) %>%
        str_trim()
    )

  return(out)


  return(out)
}
