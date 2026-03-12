### PCA for available nutrients

# make pca for available nutrients except N and P

make_nutrient_pca <- function(available_nutrients_raw, meta){

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

  pca_sites <- bind_cols(
    nutri_wide |>
      select(siteID:precipitation_scaled),
    fortify(pca_output, display = "sites")
  )

  pca_traits <- fortify(pca_output, display = "species") |>
    rename(elements = label)

  outputList <- list(pca_sites, pca_traits, pca_output)

}


plot_nutrient_pca <- function(other_available_nutrients){


  e_B <- eigenvals(other_available_nutrients[[3]])/sum(eigenvals(other_available_nutrients[[3]]))

  evals <- eigenvals(other_available_nutrients[[3]])
  n_pcs <- length(evals)
  scree <- evals |>
    as_tibble() |>
    rename(eigenvalue = x) |>
    mutate(PC = paste0("PC", seq_len(n_pcs)),
           PC = factor(PC, levels = paste0("PC", seq_len(n_pcs)))) |>
    ggplot(aes(y = eigenvalue, x = PC)) +
    geom_col() +
    labs(tag = "a)") +
    theme_bw()


  pca <- other_available_nutrients[[1]] |>
    ggplot(aes(x = PC1, y = PC2, colour = temperature_degree, shape = precipitation_name)) +
    geom_segment(data = other_available_nutrients[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 colour = "grey70",
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE) +
    geom_text(data = other_available_nutrients[[2]],
              aes(x = PC1 + 0.1, y = PC2, label = elements),
              size = 2.5,
              inherit.aes = FALSE,
              show.legend = FALSE, parse = TRUE) +
    geom_point() +
    coord_equal() +
    scale_colour_viridis_c(end = 0.8, option = "inferno", name = "Summer temperature") +
    scale_shape_manual(values = c(15, 16, 17, 1), name = "Precipitation in mm") +
    labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)"),
         tag = "b)") +
    theme_bw() +
    theme(aspect.ratio = 1,
          plot.tag.position = c(0, 0.9),
          plot.tag = element_text(vjust = -1.5, hjust = -0.5, size = 10))

  scree + pca

}

