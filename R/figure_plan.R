# make figures

figure_plan <- list(

  # biodiversity figure
  tar_target(
    name = bd_figure,
    command = big_data |>
      filter(!is.na(habitat),
             data_type == "biodiversity") |>
      mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm"))) |>
      ggplot(aes(x = fg_richness, y = value_std, colour = response,
                 shape = trophic_level, linetype = trophic_level)) +
      geom_point() +
      geom_smooth(method = "lm", mapping = aes(fill = response)) +
      scale_x_continuous(breaks = c(0, 1, 2, 3)) +
      scale_colour_manual(values = c("plum3", "limegreen")) +
      scale_fill_manual(values = c("plum3", "limegreen")) +
      labs(x = "Number of functional groups present",
           y ="Standardized biodiversity") +
      facet_grid(habitat ~ precipitation_name) +
      theme_bw()
  ),

  # function figure
  tar_target(
    name = function_figure,
    command = big_data |>
      filter(!is.na(habitat),
             data_type == "function") |>
      mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm"))) |>
      ggplot(aes(x = fg_richness, y = value_std, colour = response,
                 shape = trophic_level, linetype = trophic_level)) +
      geom_point() +
      geom_smooth(method = "lm", mapping = aes(fill = response), alpha = 0.15) +
      scale_x_continuous(breaks = c(0, 1, 2, 3)) +
      scale_colour_viridis_d(option = "inferno", end = 0.85) +
      scale_fill_viridis_d(option = "inferno", end = 0.85) +
      labs(x = "Number of functional groups present",
           y ="Absolute standardized function") +
      guides(fill = "none") +
      facet_grid(habitat ~ precipitation_name) +
      theme_bw()
  ),


  #multifunctionality figure
  tar_target(
    name = treatment_patterns,
    command = list(
      "chocolate4",
      "limegreen",
      "plum3",
      "orange2",
      pattern(
        rectGrob(x = c(0.25, 0.25, .75, .75),
                 y = c(0.25, 0.75, 0.25, 0.75),
                 width = .5, height = .5),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("limegreen", "plum3", "plum3", "limegreen"))),
      pattern(
        rectGrob(x = c(0.25, 0.25, .75, .75),
                 y = c(0.25, 0.75, 0.25, 0.75),
                 width = .5, height = .5),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("limegreen", "orange2", "orange2", "limegreen"))),
      pattern(
        rectGrob(x = c(0.25, 0.25, .75, .75),
                 y = c(0.25, 0.75, 0.25, 0.75),
                 width = .5, height = .5),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("orange2", "plum3", "plum3", "orange2"))),
      # pattern with 3 colours
      pattern(
        rectGrob(x = c(0.25, 0.25, .75, .75),
                 y = c(0.25, 0.75, 0.25, 0.75),
                 width = .5, height = .5),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("limegreen", "plum3", "orange2"))
      )
    )

  ),


  # multifunctionality figure
  tar_target(
    name = multifunctionality_figure,
    command = multifunctionality |>
      filter(!is.na(habitat),
             data_type == "function") |>
      mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm"))) |>
      ggplot(aes(x = fg_remaining, y = multifuntionality, fill = fg_remaining)) +
      geom_boxplot() +
      scale_fill_manual(values = treatment_patterns) +
      scale_x_discrete(labels = c(0, "", 1, "", "", 2, "", 3)) +
      #scale_x_discrete(labels = c("A", "B"), breaks = c(1, 3, 6, 8)) +
      labs(x = "Number of functional groups present",
           y ="Average multifunctionality") +
      facet_grid(habitat ~ precipitation_name) +
      theme_bw()
  ),

  # correlation matrix
  tar_target(
    name = correlation_plot,
    command = {

        function_table <- big_data %>%
          filter(!is.na(value)) |>
          pivot_wider(names_from = response, values_from = value, values_fill = 0) |>
          select(biomass:Reco)

        corr <- round(cor(function_table), 1)
        p.mat <- cor_pmat(function_table)

        ggcorrplot(corr, hc.order = TRUE, type = "lower",
                   colors = c("#6D9EC1", "white", "#E46726"),
                   lab = TRUE)

    }
  )

)

