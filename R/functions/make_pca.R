# make pca

#' Run PCA on ecosystem function data, optionally with PERMANOVA
#'
#' @param big_data Long-format data with siteID, blockID, plotID, treatment,
#'   response, value_std, plus optional env vars
#' @param partition Optional character: variable to partial out (e.g. "siteID").
#'   If provided, runs partial PCA on residuals after removing that effect via vegan::rda(..., Condition()).
#' @param formula_terms Optional character vector for PERMANOVA, e.g. c("treatment", "siteID").
#'   If provided, runs vegan::adonis2 and adds result as 4th list element.
#' @param permanova_method Distance method for adonis2 (default "euclidean")
#' @param permanova_permutations Number of permutations (default 999)
make_pca <- function(big_data, partition = NULL, formula_terms = NULL, permanova_method = "euclidean", permanova_permutations = 999) {

  meta_cols <- c("siteID", "blockID", "plotID", "treatment", "temperature_degree",
                 "habitat", "precipitation_name", "precipitation_mm",
                 "temperature_scaled", "precipitation_scaled")
  meta_cols <- intersect(meta_cols, names(big_data))

  # prep data: one row per plot
  big_data_wide <- big_data |>
    select(all_of(meta_cols), response, value_std) |>
    pivot_wider(names_from = response, values_from = value_std, values_fill = 0)

  response_matrix <- big_data_wide |> select(-all_of(meta_cols))
  metadata <- big_data_wide |> select(all_of(meta_cols))

  # make pca: normal or partial (partition out one variable)
  if (is.null(partition)) {
    pca_output <- response_matrix |> rda(scale = TRUE, center = TRUE)
  } else {
    if (!partition %in% names(metadata)) {
      stop("partition variable '", partition, "' not found in metadata")
    }
    form <- as.formula(paste("response_matrix ~ Condition(", partition, ")"))
    pca_output <- rda(form, data = metadata, scale = TRUE)
  }

  pca_sites <- bind_cols(
    metadata,
    fortify(pca_output, display = "sites")
  )

  # Map response to group for colouring loadings
  response_to_group <- big_data |>
    distinct(response, group)

  pca_traits <- fortify(pca_output, display = "species") |>
    rename(response = label) |>
    left_join(response_to_group, by = "response")

  # optional PERMANOVA
  permanova <- NULL
  if (!is.null(formula_terms)) {
    formula_obj <- as.formula(paste("response_matrix ~", paste(formula_terms, collapse = " + ")))
    permanova <- vegan::adonis2(formula_obj, data = metadata, method = permanova_method, permutations = permanova_permutations)
  }

  list(pca_sites, pca_traits, pca_output, permanova)
}

make_scree_plot <- function(pca_output){
  # pca_output is list(sites, traits, rda) from make_pca; extract rda object
  rda_obj <- if (inherits(pca_output, "rda")) pca_output else pca_output[[3]]
  evals <- eigenvals(rda_obj)
  e_B <- evals / sum(evals)
  n_pcs <- length(evals)
  if (n_pcs == 0) return(ggplot() + theme_void() + labs(title = "No eigenvalues"))
  scree <- evals |>
    as_tibble() |>
    rename(eigenvalue = x) |>
    mutate(PC = paste0("PC", seq_len(n_pcs)),
           PC = factor(PC, levels = paste0("PC", seq_len(n_pcs)))) |>
    ggplot(aes(y = eigenvalue, x = PC)) +
    geom_col() +
    theme_bw()
  scree
}

plot_pca <- function(pca_output){

  e_B <- eigenvals(pca_output[[3]]) / sum(eigenvals(pca_output[[3]]))
  sites <- pca_output[[1]]
  traits <- pca_output[[2]]

  # Biplot: points and arrows in one figure
  pca_plot <- ggplot() +
    # Arrows (loadings) first, coloured by group
    geom_segment(
      data = traits,
      aes(x = 0, y = 0, xend = PC1, yend = PC2, colour = group),
      arrow = arrow(length = unit(0.2, "cm"))
    ) +
    geom_text(
      data = traits,
      aes(x = PC1 + 0.05, y = PC2, label = response, colour = group),
      size = 3.5, hjust = 0, show.legend = FALSE
    ) +
    scale_colour_brewer(palette = "Dark2", name = "Group (loadings)") +
    ggnewscale::new_scale_color() +
    # Points on top, coloured by temperature
    geom_point(
      data = sites,
      aes(x = PC1, y = PC2, colour = temperature_degree, shape = precipitation_name),
      size = 4
    ) +
    coord_equal() +
    scale_colour_viridis_c(end = 0.8, option = "inferno", name = "Summer temperature") +
    scale_shape_manual(values = c(15, 16, 17, 1), name = "Precipitation in mm") +
    labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)")) +
    theme_bw() +
    theme(aspect.ratio = 1)

  pca_plot
}

#' @param display "climate" (colour by temperature, shape by precipitation) or "treatment" (colour by treatment)
#' @param facet_var Optional string: column name in `sites` data used for `facet_wrap(~ facet_var)`. Default NULL (no faceting).
plot_pca_separate <- function(pca_output, display = c("climate", "treatment"), facet_var = NULL) {

  display <- match.arg(display)
  e_B <- eigenvals(pca_output[[3]]) / sum(eigenvals(pca_output[[3]]))
  sites <- pca_output[[1]]
  traits <- pca_output[[2]]

  axis_labs <- c(
    glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
    glue("PCA2 ({round(e_B[2] * 100, 1)}%)")
  )

  # Plot 1: arrows (loadings) only
  p_loadings <- ggplot() +
    geom_segment(
      data = traits,
      aes(x = 0, y = 0, xend = PC1, yend = PC2, colour = group),
      arrow = arrow(length = unit(0.2, "cm"))
    ) +
    geom_text(
      data = traits,
      aes(x = PC1 + 0.05, y = PC2, label = response, colour = group),
      size = 3.5, hjust = 0, show.legend = FALSE
    ) +
    scale_colour_brewer(palette = "Dark2", name = "Group (loadings)") +
    coord_equal() +
    labs(x = axis_labs[1], y = axis_labs[2]) +
    theme_bw() +
    theme(aspect.ratio = 1)

  # Plot 2: points (sites) only
  if (display == "climate") {
    p_sites <- ggplot() +
      geom_point(
        data = sites,
        aes(x = PC1, y = PC2, colour = temperature_degree, shape = precipitation_name),
        size = 4
      ) +
      coord_equal() +
      scale_colour_viridis_c(end = 0.8, option = "inferno", name = "Summer temperature") +
      scale_shape_manual(values = c(15, 16, 17, 1), name = "Precipitation in mm") +
      labs(x = axis_labs[1], y = axis_labs[2]) +
      theme_bw() +
      theme(aspect.ratio = 1)
  } else {
    p_sites <- ggplot() +
      geom_point(
        data = sites,
        aes(x = PC1, y = PC2, colour = treatment),
        size = 4
      ) +
      coord_equal() +
      scale_colour_brewer(palette = "Set1", name = "Treatment") +
      labs(x = axis_labs[1], y = axis_labs[2]) +
      theme_bw() +
      theme(aspect.ratio = 1)
  }

  # Optional faceting by a column in `sites`
  if (!is.null(facet_var)) {
    p_sites <- p_sites + facet_wrap(as.formula(paste("~", facet_var)))
  }

  # Stack plots vertically: points on top, arrows below
  p_sites / p_loadings
}
