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
      # shift values to make all positive (for responses with negative values)
      group_by(response) |>
      mutate(value = case_when(
        response %in% c("micro nutrients") ~ value + abs(min(value, na.rm = TRUE)) + 1,
        TRUE ~ value
      )) |>
      ungroup() |>
      # log transform some functions (careful this code is duplicated, also in mf plan)
      mutate(value_trans = case_when(
        # log for responses with only positive values
        response %in% c("biomass", "root biomass", "plant richness", "microarthropod density", "nematode density", "carbon", "nitrogen", "phosphorous", "micro nutrients", "gpp") ~ log(value),
        # no transformation for others
        TRUE ~ value
      )) |>
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
                 lab_size = 8,
                 tl.cex = 14,
                 tl.srt = 45) +
        theme(axis.text = element_text(size = 8))

    }
  ),


  # group figures
  tar_target(
    name = pp_plot,
    command = make_group_figure(big_data, 
    group = "primary producers")
  ),

  tar_target(
    name = htl_plot,
    command = make_group_figure(big_data,
    group = "higher trophic level")
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
