# make figures

figure_plan <- list(

  # palettes
  # shape for prec starting with continental - oceanic
  tar_target(
    name = prec_shape,
    command = c(6, 0, 1, 2)
      ),

  # function figure (spaghetti plot)
  tar_target(
    name = function_figure,
    command = big_data |>
      filter(!is.na(habitat)) |>
      mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm"))) |>
      ggplot(aes(x = fg_richness, y = value_std, colour = response,
                 shape = group, linetype = group)) +
      geom_point() +
      geom_smooth(method = "lm", mapping = aes(fill = response), alpha = 0.15) +
      scale_x_continuous(breaks = c(0, 1, 2, 3)) +
      scale_colour_viridis_d(option = "inferno", end = 0.85) +
      scale_fill_viridis_d(option = "inferno", end = 0.85) +
      labs(x = "Number of functional groups present",
           y ="Standardized function") +
      guides(fill = "none") +
      facet_grid(habitat ~ precipitation_name) +
      theme_bw()
  ),

  # multifunctionality figure
  ### For now only showing facet by temp, because precip is not important!!!
  tar_target(
    name = multifunctionality_figure,
    command = multifunctionality |>
      filter(!is.na(habitat),
             data_type == "function") |>
      mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm"))) |>
      ggplot(aes(x = fg_richness, y = multifuntionality,
                 colour = precipitation_name, shape = precipitation_name, fill = precipitation_name)) +
      geom_jitter(width = 0.1, alpha = 0.7) +
      geom_smooth(method = "lm", alpha = 0.4) +
      scale_colour_viridis_d(option = "mako", direction = -1, end = 0.7) +
      scale_shape_manual(values = prec_shape) +
      scale_fill_viridis_d(option = "mako", direction = -1, end = 0.7) +
      labs(x = "Number of functional groups present",
           y ="Average multifunctionality") +
      facet_wrap( ~ habitat) +
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

  # multifunctionality by treatment
  tar_target(
    name = multifunctionality_figure2,
    command = multifunctionality |>
      filter(!is.na(habitat),
             data_type == "function") |>
      mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm"))) |>
      ggplot(aes(x = fg_remaining, y = multifuntionality, fill = fg_remaining)) +
      geom_boxplot() +
      scale_fill_manual(values = treatment_patterns) +
      #scale_x_discrete(labels = c(0, "", 1, "", "", 2, "", 3), name = "") +
      #scale_x_discrete(labels = c("A", "B"), breaks = c(1, 3, 6, 8)) +
      geom_signif(comparisons = list(c("B", "All"),
                                     c("F", "All"),
                                     c("None", "All")),
                  map_signif_level = TRUE,
                  y_position = c(4.3, 4.6, 4.9)) +
      labs(x = "Functional groups present",
           y ="Average multifunctionality") +
      #facet_grid(habitat ~ precipitation_name) +
      theme_bw() +
      theme(legend.position = "none")
  ),

  ### VARIANCE PARTITIONING ####

  tar_target(
    name = vp_multifun_figure,
    command = vp_multifun |>
      mutate(group = "a") |>
      ggplot(aes(x = group, y = var_explained, fill = variable)) +
      geom_col(position="stack") +
      scale_fill_viridis_d(name = "Variance partitioning") +
      labs(x = "", y = "Variance explained (%)") +
      theme_bw()
  )

)

