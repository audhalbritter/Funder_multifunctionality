# make pca

#' Run PCA on ecosystem function data, optionally with PERMANOVA
#'
#' @param big_data Long-format data with siteID, blockID, plotID, treatment,
#'   response, value_std, plus optional env vars
#' @param partition Optional character: variable to partial out (e.g. "siteID").
#'   If provided, runs partial PCA on residuals after removing that effect via vegan::rda(..., Condition()).
#'   PERMANOVA (if requested) is then run on the same residualized response matrix.
#' @param formula_terms Optional: either a single formula string (e.g. "temperature_scaled * precipitation_scaled")
#'   or a named list of formula strings for multiple PERMANOVA models (e.g. list(climate = "...", fg_biomass = "...")).
#'   If provided, runs vegan::adonis2 and adds result(s) as 4th list element (single result or named list of results).
#' @param permanova_method Distance method for adonis2 (default "euclidean")
#' @param permanova_permutations Number of permutations (default 999)
#' @param seed Integer seed passed to set.seed() before adonis2 for reproducible permutation p-values (default 42)
#' @param permanova_by Passed to vegan::adonis2(by = ...). Default "terms" to return R2 per model term.
make_pca <- function(big_data, partition = NULL, formula_terms = NULL, permanova_method = "euclidean", permanova_permutations = 999, seed = 42, permanova_by = "terms") {
  # include year so each year × plot is a separate observation in the PCA
  meta_cols <- c(
    "year", "siteID", "blockID", "plotID", "treatment", "temperature_degree",
    "habitat", "fg_remaining", "precipitation_name", "precipitation_mm",
    "temperature_scaled", "precipitation_scaled",
    "total_biomass", "total_biomass_scaled",
    "biomass_scaled_G", "biomass_scaled_F", "biomass_scaled_B"
  )
  meta_cols <- intersect(meta_cols, names(big_data))

  # prep data: one row per meta combo, one column per response
  big_data_wide <- big_data |>
    select(all_of(meta_cols), response, value_std) |>
    tidyr::pivot_wider(
      names_from  = response,
      values_from = value_std,
      values_fill = 0
    )

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

  # vegan 2.7 scores(tidy = TRUE) replaces fortify(); tidy always returns every score type
  pca_scores <- scores(pca_output, choices = 1:6, tidy = TRUE)

  pca_sites <- bind_cols(
    metadata,
    pca_scores |>
      filter(score == "sites") |>
      select(starts_with("PC"))
  )

  # Map response to group for colouring loadings
  response_to_group <- big_data |>
    distinct(response, group)

  pca_traits <- pca_scores |>
    filter(score == "species") |>
    rename(response = label) |>
    left_join(response_to_group, by = "response") |>
    mutate(response = pretty_response_label(response))

  # optional PERMANOVA: one model or multiple (seed ensures reproducible permutation p-values)
  # if partitioned PCA is requested, use residualized responses for PERMANOVA as well
  permanova <- NULL
  if (!is.null(formula_terms)) {
    permanova_response <- if (is.null(partition)) {
      response_matrix
    } else {
      as.data.frame(residuals(pca_output))
    }
    run_one <- function(ft) {
      form_str <- if (length(ft) > 1L) paste(ft, collapse = " + ") else ft
      as.formula(paste("permanova_response ~", form_str))
    }
    if (is.list(formula_terms) && !inherits(formula_terms, "formula")) {
      permanova <- lapply(formula_terms, function(ft) {
        set.seed(seed)
        formula_obj <- run_one(ft)
        vegan::adonis2(
          formula_obj,
          data = metadata,
          method = permanova_method,
          permutations = permanova_permutations,
          by = permanova_by
        )
      })
    } else {
      set.seed(seed)
      formula_obj <- run_one(formula_terms)
      permanova <- vegan::adonis2(
        formula_obj,
        data = metadata,
        method = permanova_method,
        permutations = permanova_permutations,
        by = permanova_by
      )
    }
  }

  list(pca_sites, pca_traits, pca_output, permanova)
}

make_scree_plot <- function(pca_output, n_axes = 10) {
  # pca_output is list(sites, traits, rda) from make_pca; extract rda object
  rda_obj <- if (inherits(pca_output, "rda")) pca_output else pca_output[[3]]
  evals <- eigenvals(rda_obj)
  if (length(evals) == 0) {
    return(ggplot() +
      theme_void() +
      labs(title = "No eigenvalues"))
  }
  pct <- evals / sum(evals) * 100
  n_show <- min(n_axes, length(pct))
  tibble(
    PC = factor(paste0("PC", seq_len(n_show)), levels = paste0("PC", seq_len(n_show))),
    variance = as.numeric(pct[seq_len(n_show)])
  ) |>
    ggplot(aes(x = PC, y = variance)) +
    geom_col() +
    geom_text(aes(label = round(variance, 1)), vjust = -0.4, size = 2.6) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = NULL, y = "Variance explained (%)") +
    theme_bw()
}

plot_pca <- function(pca_output) {
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
    labs(
      x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
      y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)")
    ) +
    theme_bw() +
    theme(aspect.ratio = 1)

  pca_plot
}

#' @param display "climate" (colour by temperature, shape by precipitation) or "treatment" (colour by treatment)
#' @param facet_var Optional string: column name in `sites` data used for `facet_wrap(~ facet_var)`. Default NULL (no faceting).
#' @param facet_ncol Optional number of facet columns.
#' @param facet_nrow Optional number of facet rows.
#' @param point_size_var Optional string: column name in `sites` used for point size (e.g. "total_biomass"). Default NULL (fixed point size).
#' @param point_alpha Numeric transparency for points in [0, 1]. Default 1 (opaque).
#' @param show_loadings_legend Logical. Keep the group legend on the loadings panel.
#' @param flip_axes Logical. Multiply PC1 and PC2 by -1 so the ordination can be matched to another plot. The sign of a PCA axis is arbitrary.
#' @param components Logical. If TRUE, return a list with `sites` and `loadings` instead of a stacked patchwork.
plot_pca_separate <- function(pca_output, display = c("climate", "treatment"), facet_var = NULL, facet_ncol = NULL, facet_nrow = NULL, point_size_var = NULL, point_alpha = 1, show_loadings_legend = TRUE, flip_axes = FALSE, components = FALSE) {
  display <- match.arg(display)
  e_B <- eigenvals(pca_output[[3]]) / sum(eigenvals(pca_output[[3]]))
  sites <- pca_output[[1]]
  traits <- pca_output[[2]]

  axis_labs <- c(
    glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
    glue("PCA2 ({round(e_B[2] * 100, 1)}%)")
  )

  if (flip_axes) {
    sites <- sites |> mutate(PC1 = -PC1, PC2 = -PC2)
    traits <- traits |> mutate(PC1 = -PC1, PC2 = -PC2)
  }

  if (!is.null(facet_var)) {
    if (!facet_var %in% names(sites)) {
      stop("facet_var '", facet_var, "' not found in PCA site metadata")
    }
    sites[[facet_var]] <- droplevels(as.factor(sites[[facet_var]]))
  }
  if (!is.null(point_size_var) && !point_size_var %in% names(sites)) {
    stop("point_size_var '", point_size_var, "' not found in PCA site metadata")
  }
  if (!is.numeric(point_alpha) || length(point_alpha) != 1L || point_alpha < 0 || point_alpha > 1) {
    stop("point_alpha must be a single numeric value between 0 and 1")
  }

  # Label arrows longer than this loading radius. Shorter arrows stay
  # on the plot, but their names are omitted so the long arrows can fill
  # the panel. Names are nudged off the tip; a line is drawn only when
  # the name cannot sit next to its arrow.
  loading_min_radius <- 0.6
  loading_label <- c(
    "Bacteria:fungal feeder" = "B:F feeder",
    "Bacterivore feeder" = "Bacterivore",
    "Fungivore feeder" = "Fungivore",
    "Predator feeder" = "Predator",
    "Plant feeder" = "Pl. feeder",
    "Plant parasite index" = "Pl. parasite",
    "Graminoid decomp." = "Gram. decomp.",
    "Root associated" = "Root assoc.",
    "Maturity index" = "Maturity",
    "Non-melanised" = "Non-melan.",
    "Bacteria:Fungi" = "Bact.:Fungi",
    "Saprotroph" = "Sapro."
  )
  label_xlim <- c(min(traits$PC1) - 0.22, max(traits$PC1) + 0.22)
  label_ylim <- c(min(traits$PC2) - 0.28, max(traits$PC2) + 0.24)
  traits_lab <- traits |>
    mutate(
      loading_radius = sqrt(PC1^2 + PC2^2),
      angle = atan2(PC2, PC1),
      response = recode(response, !!!loading_label)
    ) |>
    filter(loading_radius >= loading_min_radius) |>
    arrange(angle) |>
    mutate(
      side = if_else(row_number() %% 2L == 1L, 1, -1),
      right_bundle = PC1 > 1.15 & abs(PC2) < 0.45,
      nudge_x = cos(angle) * 0.05 + (-sin(angle)) * side * 0.14,
      nudge_y = sin(angle) * 0.05 + cos(angle) * side * 0.14
    )
  # The right-hand arrows share one direction, so their names are stacked
  # beside the tips and grow back into the panel. Every other name is
  # placed by repel, which draws a line only if it has to move the text.
  traits_right <- traits_lab |>
    filter(right_bundle) |>
    arrange(desc(PC2)) |>
    mutate(
      label_x = max(PC1) + 0.04,
      label_y = ((n() + 1) / 2 - row_number()) * 0.34
    )
  traits_repel <- traits_lab |>
    filter(!right_bundle)

  # Plot 1: arrows (loadings) only
  p_loadings <- ggplot() +
    geom_segment(
      data = traits,
      aes(x = 0, y = 0, xend = PC1, yend = PC2, colour = group),
      arrow = arrow(length = unit(0.2, "cm"))
    ) +
    geom_segment(
      data = traits_right,
      aes(x = PC1, y = PC2, xend = label_x, yend = label_y),
      colour = "grey70",
      linewidth = 0.25
    ) +
    geom_text(
      data = traits_right,
      aes(x = label_x, y = label_y, label = response, colour = group),
      size = 2.8,
      hjust = 1,
      vjust = 0.5,
      show.legend = FALSE
    ) +
    ggrepel::geom_text_repel(
      data = traits_repel,
      aes(x = PC1, y = PC2, label = response, colour = group),
      size = 2.8,
      show.legend = FALSE,
      min.segment.length = 0.3,
      segment.colour = "grey60",
      segment.size = 0.25,
      box.padding = 0.25,
      point.padding = 0.08,
      force = 1.6,
      force_pull = 0.7,
      max.overlaps = Inf,
      nudge_x = traits_repel$nudge_x,
      nudge_y = traits_repel$nudge_y,
      xlim = label_xlim,
      ylim = label_ylim,
      seed = 1
    ) +
    scale_colour_brewer(palette = "Dark2", name = "Group (loadings)") +
    coord_equal(
      xlim = label_xlim,
      ylim = label_ylim,
      expand = FALSE
    ) +
    labs(x = axis_labs[1], y = axis_labs[2]) +
    theme_bw() +
    theme(
      aspect.ratio = 1,
      legend.position = if (show_loadings_legend) "right" else "none",
      plot.margin = margin(6, 8, 4, 6)
    )

  # Plot 2: points (sites) only
  if (display == "climate") {
    point_layer <- if (is.null(point_size_var)) {
      geom_point(
        data = sites,
        aes(x = PC1, y = PC2, colour = temperature_degree, shape = precipitation_name),
        size = 3,
        alpha = point_alpha
      )
    } else {
      geom_point(
        data = sites,
        aes(x = PC1, y = PC2, colour = temperature_degree, shape = precipitation_name, size = .data[[point_size_var]]),
        alpha = point_alpha
      )
    }

    p_sites <- ggplot() +
      point_layer +
      coord_equal() +
      scale_colour_viridis_c(end = 0.8, option = "inferno", name = "Summer temperature") +
      scale_shape_manual(values = c(15, 16, 17, 1), name = "Precipitation in mm") +
      labs(x = axis_labs[1], y = axis_labs[2]) +
      theme_bw() +
      theme(aspect.ratio = 1)
  } else {
    point_layer <- if (is.null(point_size_var)) {
      geom_point(
        data = sites,
        aes(x = PC1, y = PC2, colour = fg_remaining),
        size = 2,
        alpha = point_alpha
      )
    } else {
      geom_point(
        data = sites,
        aes(x = PC1, y = PC2, colour = fg_remaining, size = .data[[point_size_var]]),
        alpha = point_alpha
      )
    }

    p_sites <- ggplot() +
      point_layer +
      coord_equal() +
      scale_colour_brewer(palette = "Set1", name = "FG remaining") +
      labs(x = axis_labs[1], y = axis_labs[2]) +
      theme_bw() +
      theme(aspect.ratio = 1)
  }

  if (!is.null(point_size_var)) {
    size_name <- if (point_size_var == "total_biomass") "Total biomass" else point_size_var
    p_sites <- p_sites + scale_size_continuous(name = size_name)
  }

  # Optional faceting by a column in `sites`
  if (!is.null(facet_var)) {
    p_sites <- p_sites + facet_wrap(
      as.formula(paste("~", facet_var)),
      ncol = facet_ncol,
      nrow = facet_nrow
    )
  }

  if (components) {
    return(list(sites = p_sites, loadings = p_loadings))
  }

  # Stack plots vertically: points on top, arrows below
  p_sites / p_loadings
}

permanova_term_label <- function(x) {
  x |>
    str_replace_all("biomass_scaled_G", "Graminoid biomass") |>
    str_replace_all("biomass_scaled_F", "Forb biomass") |>
    str_replace_all("biomass_scaled_B", "Bryophyte biomass") |>
    str_replace_all("total_biomass_scaled", "Total biomass") |>
    str_replace_all("temperature_scaled", "Temperature") |>
    str_replace_all("precipitation_scaled", "Precipitation") |>
    str_replace_all("treatment", "Treatment") |>
    str_replace_all(":", " × ")
}

permanova_terms <- function(ad, model_name) {
  ad |>
    as.data.frame() |>
    tibble::rownames_to_column("term") |>
    filter(term != "Total") |>
    transmute(
      model = model_name,
      term = permanova_term_label(term),
      r2 = .data[["R2"]] * 100,
      p = .data[["Pr(>F)"]]
    )
}

#' Compare sequential R² from the full and site-partitioned PERMANOVA models
#'
#' @param pca_full Output of make_pca() without partitioning
#' @param pca_partitioned Output of make_pca() with site partitioned out
#' @return A gt table. Significant R² values (p ≤ 0.05) are bold. Residual rows are included.
make_permanova_comparison_table <- function(pca_full, pca_partitioned) {
  model_labels <- c(
    climate = "Climate only",
    fg_climate_biomass = "Treatment + climate + total biomass",
    biomass_fg_climate = "Functional group biomass + climate"
  )

  extract <- function(pca) {
    permanova <- pca[[4]]
    missing <- setdiff(names(model_labels), names(permanova))
    if (length(missing) > 0) {
      stop("PERMANOVA models missing: ", paste(missing, collapse = ", "))
    }
    purrr::map(names(model_labels), function(nm) {
      permanova_terms(permanova[[nm]], unname(model_labels[[nm]]))
    }) |>
      list_rbind()
  }

  extract(pca_full) |>
    inner_join(
      extract(pca_partitioned),
      by = c("model", "term"),
      suffix = c("_full", "_partitioned")
    ) |>
    mutate(
      model = factor(model, levels = unname(model_labels)),
      # Climate is removed with the site mean, so climate terms are not tested.
      # The climate-only model then has nothing left to test, including its residual.
      untested_partitioned = term %in% c(
        "Temperature", "Precipitation", "Temperature × Precipitation"
      ) | model == "Climate only",
      r2_partitioned = if_else(untested_partitioned, NA_real_, r2_partitioned),
      p_partitioned = if_else(untested_partitioned, NA_real_, p_partitioned),
      across(c(r2_full, r2_partitioned), ~ round(.x, 1))
    ) |>
    select(-untested_partitioned) |>
    gt(groupname_col = "model") |>
    cols_hide(columns = c(p_full, p_partitioned)) |>
    fmt_number(
      columns = c(r2_full, r2_partitioned),
      decimals = 1,
      pattern = "{x}%"
    ) |>
    cols_label(
      term = "Term",
      r2_full = "Full PCA",
      r2_partitioned = "Site partitioned"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = r2_full,
        rows = !is.na(p_full) & p_full <= 0.05
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = r2_partitioned,
        rows = !is.na(p_partitioned) & p_partitioned <= 0.05
      )
    )
}
