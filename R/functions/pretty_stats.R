# Fancy stats dictionary

fancy_stats <- function(dat, sort = TRUE){

  dat <- dat %>%
    mutate(
      term = str_replace(term, "treatment", ""),
      term = str_replace(term, "\\(Intercept\\)", "Intercept"),
      term = str_replace(term, "forb", "F"),
      term = str_replace(term, "gram", "G"),
      term = str_replace(term, "bryo", "B"),
      # treatment model: .functional_groupGF -> GF, etc.
      term = str_replace(term, "\\.functional_group", ""),
      term = str_replace(term, "temperature_scaled", "T"),
      term = str_replace(term, "precipitation_scaled", "P")
    )

  if (sort == TRUE) {
    term_levels <- c(
      "Intercept",
      "T", "P", "GF", "FB", "GB", "FGB",
      "T:P",
      "GF:T", "FB:T", "GB:T", "FGB:T",
      "GF:P", "FB:P", "GB:P", "FGB:P",
      "GF:T:P", "FB:T:P", "GB:T:P", "FGB:T:P",
      # factorial model (forb, gram, bryo)
      "FG", "FG:T", "FG:P", "FG:T:P",
      "F", "G", "B"
    )
    all_terms <- unique(dat$term)
    ordered_terms <- c(intersect(term_levels, all_terms), setdiff(all_terms, term_levels))
    dat <- dat |>
      mutate(term = factor(term, levels = ordered_terms))
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
