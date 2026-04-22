# make figures

figure_plan <- list(
  # function figure (spaghetti plot)
  tar_target(
    name = function_figure,
    command = big_data |>
      filter(!response %in% c("nema_bacterivores_density", "nema_fungivores_density", "nema_herbivores_density", "nema_omnivores_density", "nema_predators_density", "collembola_fungivorous_density", "mite_fungivorous_density", "mite_nematophagous_density", "mite_predaceous_density", "collembola_predaceous_density", "specific_root_length_m_per_g", "root_tissue_density_g_per_m3", "root_dry_matter_content")) |>
      mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm"))) |>
      ggplot(aes(
        x = fg_richness, y = value_std, colour = response,
        shape = group, linetype = group
      )) +
      geom_point() +
      geom_smooth(method = "lm", mapping = aes(fill = response), alpha = 0.15) +
      scale_x_continuous(breaks = c(0, 1, 2, 3)) +
      scale_colour_viridis_d(option = "inferno", end = 0.85) +
      scale_fill_viridis_d(option = "inferno", end = 0.85) +
      labs(
        x = "Number of functional groups present",
        y = "Standardized function"
      ) +
      guides(fill = "none") +
      facet_grid(habitat ~ precipitation_name) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.box = "vertical"
      )
  ),

  # multifunctionality figure
  tar_target(
    name = treatment_patterns,
    command = list(
      "peru",
      "mediumseagreen",
      "plum3",
      "darkorange1",
      pattern(
        rectGrob(
          x = c(0.25, 0.25, .75, .75),
          y = c(0.25, 0.75, 0.25, 0.75),
          width = .5, height = .5
        ),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("mediumseagreen", "plum3", "plum3", "mediumseagreen"))
      ),
      pattern(
        rectGrob(
          x = c(0.25, 0.25, .75, .75),
          y = c(0.25, 0.75, 0.25, 0.75),
          width = .5, height = .5
        ),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("mediumseagreen", "darkorange1", "darkorange1", "mediumseagreen"))
      ),
      pattern(
        rectGrob(
          x = c(0.25, 0.25, .75, .75),
          y = c(0.25, 0.75, 0.25, 0.75),
          width = .5, height = .5
        ),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("darkorange1", "plum3", "plum3", "darkorange1"))
      ),
      # pattern with 3 colours
      pattern(
        rectGrob(
          x = c(0.25, 0.25, .75, .75),
          y = c(0.25, 0.75, 0.25, 0.75),
          width = .5, height = .5
        ),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("mediumseagreen", "plum3", "darkorange1"))
      )
    )
  ),

  # multifunctionality by treatment
  tar_target(
    name = multifunctionality_figure2,
    command = multifunctionality |>
      filter(!is.na(habitat)) |>
      mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm"))) |>
      ggplot(aes(x = fg_remaining, y = multifuntionality, fill = fg_remaining)) +
      geom_boxplot() +
      scale_fill_manual(values = treatment_patterns) +
      # scale_x_discrete(labels = c(0, "", 1, "", "", 2, "", 3), name = "") +
      # scale_x_discrete(labels = c("A", "B"), breaks = c(1, 3, 6, 8)) +
      # geom_signif(comparisons = list(c("B", "All"),
      #                                c("F", "All"),
      #                                c("None", "All")),
      #             map_signif_level = TRUE,
      #             y_position = c(4.3, 4.6, 4.9)) +
      labs(
        x = "Functional groups present",
        y = "Average multifunctionality"
      ) +
      # facet_grid(habitat ~ precipitation_name) +
      theme_bw() +
      theme(legend.position = "none")
  ),

  ### Model comparisons (R2 and estimates)
  tar_target(
    name = model_single_function_comparison_plot,
    command = plot_model_comparison_diagnostic(
      model_tbl = model_single_function,
      response_group_lookup = big_data |> distinct(response, group)
    )
  ),
  tar_target(
    name = model_interaction_vs_main_comparison_plot,
    command = plot_interaction_vs_main_diagnostic(
      model_int = model_single_function,
      model_main = model_single_function_main,
      response_group_lookup = big_data |> distinct(response, group)
    )
  ),

  # Heatmaps: one per model (same group_lookup; unnest different result_*)
  tar_target(
    name = model_single_function_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function |>
        unnest(result) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      r2_tbl <- model_single_function |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
    }
  ),
  tar_target(
    name = model_single_function_biomass_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function |>
        unnest(result_biomass) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      r2_tbl <- model_single_function |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2_biomass,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
    }
  ),
  tar_target(
    name = model_single_function_FG_biomass_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function |>
        unnest(result_FG_biomass) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      r2_tbl <- model_single_function |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2_FG_biomass,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
    }
  ),

  ### Main-effects models (FG/biomass + climate mains + T:P only)
  tar_target(
    name = model_single_function_main_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function_main |>
        unnest(result_main) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      r2_tbl <- model_single_function_main |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2_main,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
    }
  ),
  tar_target(
    name = model_single_function_main_biomass_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function_main |>
        unnest(result_main_biomass) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      r2_tbl <- model_single_function_main |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2_main_biomass,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
    }
  ),
  tar_target(
    name = model_single_function_main_FG_biomass_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function_main |>
        unnest(result_main_FG_biomass) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      r2_tbl <- model_single_function_main |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2_main_FG_biomass,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
    }
  )
)
