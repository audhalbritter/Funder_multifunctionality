### PCA for available nutrients

# make pca for available nutrients except N and P

make_nutrient_pca <- function(available_nutrients_raw, meta) {
  # prep data
  nutrients <- available_nutrients_raw |>
    mutate(year = year(retrieval_date)) |>
    select(-burial_length, -detection_limit, -burial_date, -retrieval_date, -notes) |>
    left_join(meta, by = c("siteID", "blockID", "treatment", "plotID"))

  # wide table
  nutri_wide <- nutrients |>
    pivot_wider(names_from = elements, values_from = value, values_fill = 0)

  # make pca
  pca_output <- nutri_wide |>
    select(-(siteID:precipitation_scaled)) |>
    rda(scale = TRUE, center = TRUE)

  # vegan 2.7 scores(tidy = TRUE) replaces fortify(); tidy always returns every score type
  pca_scores <- scores(pca_output, choices = 1:6, tidy = TRUE)

  pca_sites <- bind_cols(
    nutri_wide |>
      select(siteID:precipitation_scaled),
    pca_scores |>
      filter(score == "sites") |>
      select(starts_with("PC"))
  )

  pca_traits <- pca_scores |>
    filter(score == "species") |>
    rename(elements = label)

  outputList <- list(pca_sites, pca_traits, pca_output)
}


plot_nutrient_pca <- function(other_available_nutrients) {
  pct <- eigenvals(other_available_nutrients[[3]]) / sum(eigenvals(other_available_nutrients[[3]])) * 100
  n_pcs <- length(pct)

  scree <- tibble(
    PC = factor(paste0("PC", seq_len(n_pcs)), levels = paste0("PC", seq_len(n_pcs))),
    variance = as.numeric(pct)
  ) |>
    ggplot(aes(x = PC, y = variance)) +
    geom_col() +
    geom_text(aes(label = round(variance, 1)), vjust = -0.4, size = 2.6) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = NULL, y = "Variance explained (%)") +
    theme_bw()

  loadings <- other_available_nutrients[[2]] |>
    mutate(
      label_x = PC1 * 1.12,
      label_y = PC2 * 1.12
    )

  pca <- other_available_nutrients[[1]] |>
    ggplot(aes(x = PC1, y = PC2, colour = temperature_degree, shape = precipitation_name)) +
    geom_hline(yintercept = 0, linewidth = 0.2, colour = "grey80") +
    geom_vline(xintercept = 0, linewidth = 0.2, colour = "grey80") +
    geom_segment(
      data = loadings,
      aes(x = 0, y = 0, xend = PC1, yend = PC2),
      colour = "grey40",
      arrow = arrow(length = unit(0.18, "cm")),
      inherit.aes = FALSE
    ) +
    geom_text(
      data = loadings,
      aes(x = label_x, y = label_y, label = elements),
      size = 3,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    geom_point(size = 2) +
    coord_equal() +
    scale_colour_viridis_c(end = 0.8, option = "inferno", name = "Summer temperature (°C)") +
    scale_shape_manual(values = c(15, 16, 17, 1), name = "Precipitation") +
    labs(
      x = glue("PC1 ({round(pct[1], 1)}%)"),
      y = glue("PC2 ({round(pct[2], 1)}%)")
    ) +
    theme_bw() +
    theme(aspect.ratio = 1)

  scree + pca +
    plot_layout(widths = c(1.05, 1)) +
    plot_annotation(tag_levels = "a")
}
