# make pca

make_pca <- function(big_data){

  # prep data
  big_data_wide <- big_data |>
    select(siteID, blockID, plotID, treatment, group, response, value, value_std, temperature_degree, habitat, precipitation_name, precipitation_mm, temperature_scaled, precipitation_scaled) |>
    pivot_wider(names_from = response, values_from = value, values_fill = 0)

  # make pca
  pca_output <- big_data_wide |>
    select(-(siteID:precipitation_mm)) |>
    rda(scale = TRUE, center = TRUE)

  pca_sites <- bind_cols(
    big_data_wide |>
      select(siteID:precipitation_mm),
    fortify(pca_output, display = "sites")
  )

  pca_traits <- fortify(pca_output, display = "species") |>
    rename(response = label)

  outputList <- list(pca_sites, pca_traits, pca_output)

}


plot_pca <- function(all_functions_pca){


  e_B <- eigenvals(all_functions_pca[[3]])/sum(eigenvals(all_functions_pca[[3]]))

  evals <- eigenvals(all_functions_pca[[3]])
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


  pca <- all_functions_pca[[1]] |>
    ggplot(aes(x = PC1, y = PC2, colour = temperature_degree, shape = precipitation_name)) +
    geom_segment(data = all_functions_pca[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 colour = "grey70",
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE) +
    geom_text(data = all_functions_pca[[2]],
              aes(x = PC1 + 0.1, y = PC2, label = response),
              size = 2.5,
              inherit.aes = FALSE,
              show.legend = FALSE) +
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
