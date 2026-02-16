# Fancy stats dictionary

fancy_stats <- function(dat, sort = TRUE){

  dat <- dat %>%
    mutate(term = str_replace(term, "treatment", ""),
           term = str_replace(term, "\\(Intercept\\)", "Intercept"),
           term = str_replace(term, "forb", "F"),
           term = str_replace(term, "gram", "G"),
           term = str_replace(term, "bryo", "B"),
           term = str_replace(term, ".functional_group", "FG"),
           term = str_replace(term, "temperature_scaled", "T"),
           term = str_replace(term, "precipitation_scaled", "P"))

           # sort
           if(sort == TRUE){
             dat <- dat |>
               mutate(term = factor(term, levels = c("Intercept",
                                                     "FG", "T", "P", "FG:T",
                                                     "FG:P", "T:P", "FG:T:P",
                                                     "F", "G", "B", "GF", "FB", "GB", "FGB")))
           }

  return(dat)

}


### Stats tables
round_numbers_anova <- function(table){
  table |>
    mutate(sumsq = round(sumsq, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 3),
           p.value = if_else(p.value < 0.001, "<0.001", as.character(p.value))) |>
    select(-names)
}


round_numbers_tidy <- function(table){
  table |>
    mutate(p_raw = p.value,
           estimate = round(estimate, 2),
           std.error = round(std.error, 2),
           statistic = round(statistic, 2),
           df = round(df, 2),
           p.value = round(p.value, 3),
           p.value = if_else(p.value < 0.001, "<0.001", as.character(p.value)))
}


table_style <- function(gt_table, font_size){

  gt_table |>
    tab_options(
      table.font.size = font_size,
      data_row.padding = gt::px(1)
    ) |>
    cols_align(
      align = c("left"),
      columns = term
    ) |>
    cols_align(
      align = c("right"),
      columns = contains("p.value")
    )

}
