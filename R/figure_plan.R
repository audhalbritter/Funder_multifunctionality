# make figures

figure_plan <- list(

  # palettes
  # shape for prec starting with continental - oceanic
  tar_target(
    name = prec_shape,
    command = c(6, 0, 1, 2)
      ),

  # shape for prec starting with continental - oceanic
  tar_target(
    name = temp_colour,
    command = c("#EF9A9AFF", "#E53935FF", "#B71C1CFF")
  ),

  # function figure (spaghetti plot)
  tar_target(
    name = function_figure,
    command = big_data |> 
      filter(!response %in% c("nema_bacterivores_density", "nema_fungivores_density", "nema_herbivores_density", "nema_omnivores_density", "nema_predators_density", "collembola_fungivorous_density", "mite_fungivorous_density", "mite_nematophagous_density", "mite_predaceous_density", "collembola_predaceous_density", "specific_root_length_m_per_g", "root_tissue_density_g_per_m3", "root_dry_matter_content")) |>
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
      theme_bw() +
      theme(legend.position = "bottom",
            legend.box = "vertical")
  ),

  # multifunctionality figure
  ### For now only showing facet by temp, because precip is not important!!!
  tar_target(
    name = multifunctionality_figure,
    command = {
      # one point per (.functional_group, habitat) so geom_line draws one straight line per habitat
      pred_line <- multi_nr_pred |>
        group_by(.functional_group, habitat) |>
        summarise(fitted = mean(fitted, na.rm = TRUE), .groups = "drop")

      multi_nr_pred |>
        ggplot(aes(x = .functional_group, y = .response,
                   colour = habitat, shape = habitat, fill = habitat)) +
        geom_jitter(width = 0.2, alpha = 0.5) +
        geom_line(data = pred_line,
                  aes(y = fitted, group = habitat),
                  linewidth = 1.2,
                  inherit.aes = TRUE) +
        scale_colour_manual(values = temp_colour) +
        scale_shape_manual(values = c(2, 1, 6)) +
        labs(x = "Number of functional groups present",
             y = "Average multifunctionality") +
        theme_bw() +
        theme(text = element_text(size = 15))
    }


      # multifunctionality |>
      # filter(!is.na(habitat),
      #        data_type == "function") |>
      # mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm"))) |>
      # ggplot(aes(x = fg_richness, y = multifuntionality,
      #            colour = precipitation_name, shape = precipitation_name, fill = precipitation_name)) +
      # geom_jitter(width = 0.1, alpha = 0.7) +
      # geom_smooth(method = "lm", alpha = 0.4) +
      # scale_colour_viridis_d(option = "mako", direction = -1, end = 0.7) +
      # scale_shape_manual(values = prec_shape) +
      # scale_fill_viridis_d(option = "mako", direction = -1, end = 0.7) +
      # labs(x = "Number of functional groups present",
      #      y ="Average multifunctionality") +
      # facet_wrap( ~ habitat) +
      # theme_bw()
  ),


  #multifunctionality figure
  tar_target(
    name = treatment_patterns,
    command = list(
      "peru",
      "mediumseagreen",
      "plum3",
      "darkorange1",
      pattern(
        rectGrob(x = c(0.25, 0.25, .75, .75),
                 y = c(0.25, 0.75, 0.25, 0.75),
                 width = .5, height = .5),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("mediumseagreen", "plum3", "plum3", "mediumseagreen"))),
      pattern(
        rectGrob(x = c(0.25, 0.25, .75, .75),
                 y = c(0.25, 0.75, 0.25, 0.75),
                 width = .5, height = .5),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("mediumseagreen", "darkorange1", "darkorange1", "mediumseagreen"))),
      pattern(
        rectGrob(x = c(0.25, 0.25, .75, .75),
                 y = c(0.25, 0.75, 0.25, 0.75),
                 width = .5, height = .5),
        width = unit(5, "mm"),
        height = unit(5, "mm"),
        extend = "repeat",
        gp = gpar(fill = c("darkorange1", "plum3", "plum3", "darkorange1"))),
      # pattern with 3 colours
      pattern(
        rectGrob(x = c(0.25, 0.25, .75, .75),
                 y = c(0.25, 0.75, 0.25, 0.75),
                 width = .5, height = .5),
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
  )

  ### VARIANCE PARTITIONING ####

  # vp figure for resonses
  # tar_target(
  #   name = vp_response_figure,
  #   command = bind_cols(vp_response |>
  #   rename(var_explained = `I.perc(%)`),
  #   tibble(variable = rep(c("Random", "Functional group", "Climate", "Context"), 16))) |>
  #     mutate(variable = factor(variable, levels = c("Context", "Climate", "Functional group", "Random"))) |>
  #     filter(!response %in% c("organic matter", "inorganic carbon")) |>
  #          #response = factor(response, c("biomass", "root", "nematode", "microart", "decomposition", "gpp", "nee", "som", "p", "micro"))
  #   ggplot(aes(y = response, x = var_explained, fill = variable)) +
  #   geom_col(position="stack") +
  #   scale_fill_manual(values = c("#CC79A7", "#0072B2", "#009E73", "#999999"), name = "") +
  #   theme_bw()
  #   ),

  # vp figure for multifunctionality
  # tar_target(
  #   name = vp_multifun_figure,
  #   command = vp_multifun |>
  #     mutate(group = "a",
  #            variable = c("Random", "Functional group", "Climate", "Context")) |>
  #     rename(var_explained = `I.perc(%)`) |>
  #     ggplot(aes(x = group, y = var_explained, fill = variable)) +
  #     geom_col(position="stack") +
  #     scale_fill_viridis_d(name = "Variance partitioning") +
  #     labs(x = "", y = "Variance explained (%)") +
  #     scale_fill_manual(values = c("#CC79A7", "#0072B2", "#009E73", "#999999"), name = "") +
  #     theme_bw()
  # )

)

