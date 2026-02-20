# Pretty figures

#' Heatmap of model estimates coloured by sign (blue/red); bold numbers = p <= 0.01
#'
#' @param model_data Data frame with columns: response, term, estimate, p.value (numeric)
#' @param group_lookup Data frame with columns: response, group (for sorting responses)
#' @param group_order Character vector: order of groups for y-axis (default: microenvironment, nutrient cycling, carbon cycling, nematodes, microbes)
#' @param fancy_terms Apply fancy_stats term formatting (default TRUE)
plot_model_heatmap <- function(model_data,
                               group_lookup,
                               group_order = c("microenvironment", "nutrient cycling", "carbon cycling", "nematodes", "microbes"),
                               fancy_terms = TRUE) {

  # Join group and define explicit response order
  group_lookup_distinct <- group_lookup |> distinct(response, group)
  dat <- model_data |>
    left_join(group_lookup_distinct, by = "response")

  # Explicit response order: by group_order, then alphabetically within group
  group_order_df <- tibble(group = group_order, group_order = seq_along(group_order))
  response_order <- dat |>
    distinct(response, group) |>
    left_join(group_order_df, by = "group") |>
    mutate(group_order = replace_na(group_order, 999L)) |>
    arrange(group_order, response) |>
    pull(response)

  dat <- dat |>
    mutate(response = factor(response, levels = response_order))

  # Apply fancy term names if requested
  if (fancy_terms) {
    dat <- dat |> fancy_stats(sort = FALSE)
  }

  # Significance: p <= 0.01 = bold
  dat <- dat |>
    mutate(p_raw = as.numeric(p.value)) |>
    mutate(p_raw = if_else(is.na(p_raw), 1, p_raw)) |>
    mutate(significant = p_raw <= 0.01)

  # If fancy_terms was FALSE, we may not have formatted terms
  if (!fancy_terms) {
    dat <- dat |>
      mutate(term = str_replace(term, "\\.functional_group", ""),
             term = str_replace(term, "temperature_scaled", "T"),
             term = str_replace(term, "precipitation_scaled", "P"),
             term = str_replace(term, "\\(Intercept\\)", "Intercept"))
  }

  # Order terms
  term_levels <- c(
    "Intercept", "GF", "FB", "GB", "FGB", "T", "P",
    "T:P", "GF:T", "FB:T", "GB:T", "FGB:T",
    "GF:P", "FB:P", "GB:P", "FGB:P",
    "GF:T:P", "FB:T:P", "GB:T:P", "FGB:T:P"
  )
  dat <- dat |>
    mutate(term = factor(term, levels = intersect(term_levels, unique(term))))

  # Colours for each group (for strip and legend)
  group_colours <- c(
    "microenvironment" = "#E41A1C",
    "nutrient cycling" = "#377EB8",
    "carbon cycling" = "#4DAF4A",
    "nematodes" = "#984EA3",
    "microbes" = "#FF7F00",
    "primary producers" = "#A65628",
    "higher trophic level" = "#F781BF"
  )

  # Strip data: one row per response with group colour (same response order as heatmap)
  strip_data <- tibble(response = levels(dat$response)) |>
    left_join(dat |> distinct(response, group), by = "response") |>
    mutate(
      response = factor(response, levels = levels(dat$response)),
      group_char = as.character(group),
      fill = if_else(group_char %in% names(group_colours), group_colours[group_char], "grey40")
    )

  # Heatmap
  p_heatmap <- ggplot(dat, aes(x = term, y = response, fill = estimate)) +
    geom_tile(colour = "grey90", linewidth = 0.3) +
    geom_text(
      aes(label = round(estimate, 2), fontface = ifelse(significant, "bold", "plain")),
      size = 3.5, colour = "grey20"
    ) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0, name = "Estimate"
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    scale_y_discrete(limits = rev(levels(dat$response))) +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      legend.position = "right",
      panel.grid = element_blank()
    )

  # Strip: thin bar on left with group colours
  p_strip <- ggplot(strip_data, aes(x = 1, y = response, fill = fill)) +
    geom_tile() +
    scale_y_discrete(limits = rev(levels(dat$response)), position = "right") +
    scale_fill_identity() +
    labs(x = NULL, y = NULL) +
    theme_void() +
    theme(
      axis.text.y = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )

  # Combine: strip left, heatmap right
  patchwork::wrap_plots(p_strip, p_heatmap, nrow = 1, widths = c(0.02, 0.98), guides = "collect")
}
