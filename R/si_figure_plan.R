### Supplementary Figures

si_figure_plan <- list(

  # plot ordination for other available nutrients
  tar_target(
    name = nutrient_pca,
    command = plot_pca(other_available_nutrients)
  ),

  # Normalize functions
  tar_target(
    name = normalize_functions_plot,
    command = big_data_raw |>
      # log transform some functions (careful this code is duplicated, also in mf plan)
      mutate(value_trans = if_else(response %in% c("biomass", "root biomass", "root turnover", "organic matter", "microarthropod density", "nitrogen", "phosphate"), log(value), value)) |>
      ggplot(aes(x = value_trans)) +
      geom_histogram() +
      labs(x = "Normalized functions") +
      facet_wrap(~ response, scales = "free")
  ),

  # standardize functions
  tar_target(
    name = standardize_functions_plot,
    command = big_data |>
      ggplot(aes(x = value_std)) +
      geom_histogram() +
      labs(x = "Standardized value") +
      facet_wrap(~ response, scales = "free")
  ),

  # root data
  tar_target(
    name = root_data_plot,
    command = big_data |>
      filter(response %in% c("root biomass", "root productivity", "root turnover")) |>
      ggplot(aes(y = value_std, x = fg_richness, colour = response)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(y = "Standardized value") +
      facet_grid(habitat ~ precipitation_name, scales = "free") +
      theme_bw()
  ),

  # correlation matrix
  tar_target(
    name = correlation_plot,
    command = {

      function_table <- big_data |>
        ungroup() |>
        select(-year, -value_trans, -value_std, -unit, -duration, -litter_type, -temperature_degree, -habitat, -temperature_scaled, -precipitation_mm, -precipitation_name, -precipitation_scaled, -fg_richness, -fg_remaining, -data_type, -group) |>
        pivot_wider(names_from = response, values_from = value, values_fill = 0) |>
        select(`biomass`:`micro nutrients`)

      corr <- round(cor(function_table), 1)
      p.mat <- cor_pmat(function_table)

      ggcorrplot(corr, hc.order = TRUE, type = "lower",
                 colors = c("#6D9EC1", "white", "#E46726"),
                 lab = TRUE)

    }
  ),


  # group figures
  tar_target(
    name = pp_plot,
    command = make_group_figure(big_data, group = "primary producers")
  ),

  tar_target(
    name = htl_plot,
    command = make_group_figure(big_data, group = "higher trophic level")
  ),

  tar_target(
    name = cc_plot,
    command = make_group_figure(big_data |>
                                  filter(!response %in% c("decomposition forbs", "Reco")),
                                group = "carbon cycling")
  ),

  tar_target(
    name = nc_plot,
    command = make_group_figure(big_data, group = "nutrient cycling")
  )



)
