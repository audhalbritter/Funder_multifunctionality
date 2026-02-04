### Supplementary Figures

si_figure_plan <- list(

  # plot ordination for other available nutrients
  tar_target(
    name = nutrient_pca,
    command = plot_pca(other_available_nutrients)
  ),

  # Raw data
  tar_target(
    name = raw_functions_plot,
    command = big_data_raw |>
      ggplot(aes(x = value)) +
      geom_histogram() +
      labs(x = "Raw functions") +
      facet_wrap(~ response, scales = "free")
  ),

  # Normalize functions
  tar_target(
    name = normalize_functions_plot,
    command = big_data_raw |> 
      # log transform some functions (careful this code is duplicated, also in mf plan)
      mutate(value_trans = if_else(response %in% c("biomass", "root biomass", "microarthropod density", "nematode density", "carbon", "nitrogen", "phosphate", "gpp", "nema_bacterivores_density", "nema_fungivores_density", "nema_herbivores_density", "nema_omnivores_density", "nema_predators_density", "micro_fungivorous_density", "micro_nematophagous_density", "micro_predaceous_density"), log(value), value)) |>
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

  # decomposition data
  tar_target(
    name = decomposition_data_plot,
    command = {

      dat <- big_data |>
        filter(response %in% c("decomposition graminoids", "decomposition forbs"))

      mean <- dat |>
        group_by(siteID, blockID, plotID, fg_richness, treatment, habitat, precipitation_name) |>
        summarise(value_std = mean(value_std)) |>
        mutate(data_type = "function",
               group = "carbon cycling",
               response = "decomposition mean")

      bind_rows(dat, mean) |>
        ggplot(aes(y = value_std, x = fg_richness, colour = response)) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(y = "Standardized value") +
        facet_grid(habitat ~ precipitation_name, scales = "free") +
        theme_bw()
    }

  ),

  # correlation matrix
  tar_target(
    name = correlation_plot,
    command = {

      function_table <- big_data |>
        select(-year, -value_trans, -value_std, -unit, -temperature_degree, -habitat, -temperature_scaled, -precipitation_mm, -precipitation_name, -precipitation_scaled, -fg_richness, -fg_remaining, -forb, -gram, -bryo) |>
        pivot_wider(names_from = response, values_from = value, values_fill = 0) |>
        select(`biomass`:`micro nutrients`)

      # pairwise.complete.obs avoids NA in cor matrix (root_biomass, root_traits have a few NAs)
      corr <- round(cor(function_table, use = "pairwise.complete.obs"), 1)
      p.mat <- ggcorrplot::cor_pmat(function_table)

      ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower",
                 colors = c("#6D9EC1", "white", "#E46726"),
                 lab = TRUE,
                 lab_size = 2.5,
                 tl.cex = 8,
                 tl.srt = 45) +
        theme(axis.text = element_text(size = 7))

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
