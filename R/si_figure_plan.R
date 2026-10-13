### Supplementary Figures

si_figure_plan <- list(

  # palettes
  # shape for prec starting with continental - oceanic
  tar_target(
    name = prec_shape,
    command = c(6, 0, 1, 2)
      ),

  # colour for temperature (boreal to alpine)
  tar_target(
    name = temp_colour,
    command = c("#EF9A9AFF", "#E53935FF", "#B71C1CFF")
  ),

  # colour for precipitation (700 to 2800 mm, continental to oceanic)
  tar_target(
    name = prec_colour,
    command = c("#C6DBEF", "#6BAED6", "#2171B5", "#084594")
  ),
  
  # linetype for precipitation (continental to oceanic)
  tar_target(
    name = prec_linetype,
    command = c("solid", "dashed", "dotted", "dotdash")
  ),

  # Available micronutrients: scree plot and PC1–PC2 biplot
  tar_target(
    name = micronutrient_pca_plot,
    command = plot_nutrient_pca(micronutrients_pca)
  ),

  # Three columns: raw, transformed, and standardized, each with its own axis.
  # Standardized panels share one ruler so a tick means the same thing on every row.
  tar_target(
    name = transformation_distributions_plot,
    command = {
      group_order <- c(
        "microenvironment", "nutrient cycling", "carbon cycling",
        "nematodes", "microbes"
      )
      group_label <- c(
        microenvironment = "Environment",
        "nutrient cycling" = "Nutrient cycling",
        "carbon cycling" = "Carbon cycling",
        nematodes = "Nematodes",
        microbes = "Microbes"
      )
      fill_colours <- c(none = "grey65", shift = "#C47B2B", log = "#2C7FB8", logit = "#7A0177")
      # One ruler for every standardized panel. Limits cover the data; ticks are identical.
      std_limits <- c(-8, 7)
      std_breaks <- c(-6, 0, 6)
      col_widths <- c(1.55, 1, 1, 1)

      base <- big_data_full |>
        select(
          year, siteID, blockID, plotID, treatment, group, response,
          transformed = value_trans,
          standardized = value_std
        ) |>
        left_join(
          big_data_raw |>
            select(year, siteID, blockID, plotID, treatment, response, raw = value),
          by = c("year", "siteID", "blockID", "plotID", "treatment", "response")
        ) |>
        mutate(
          transformation = response_transformation(response),
          response_label = pretty_response_label(response),
          group = factor(group, levels = group_order, labels = group_label)
        )

      nested <- base |>
        summarise(
          raw = list(raw[is.finite(raw)]),
          transformed = list(transformed[is.finite(transformed)]),
          standardized = list(standardized[is.finite(standardized)]),
          .by = c(group, response, response_label, transformation)
        ) |>
        arrange(group, response_label)

      density_panel <- function(x, fill, limits = NULL, breaks = NULL, zero_line = FALSE) {
        p <- ggplot(tibble(value = x), aes(value)) +
          geom_density(fill = fill, colour = NA) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
          theme_bw(base_size = 8) +
          theme(
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 6.5, colour = "grey20"),
            axis.ticks.x = element_line(colour = "grey20"),
            axis.ticks.length.x = unit(2, "pt"),
            panel.grid = element_blank(),
            plot.margin = margin(1, 4, 0, 2)
          )
        if (zero_line) {
          p <- p + geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.25)
        }
        if (is.null(limits)) {
          p + scale_x_continuous(
            breaks = scales::breaks_pretty(n = 3),
            labels = scales::label_number(scale_cut = scales::cut_short_scale()),
            expand = expansion(mult = 0.06)
          )
        } else {
          p +
            scale_x_continuous(breaks = breaks, expand = expansion(mult = 0.02)) +
            coord_cartesian(xlim = limits)
        }
      }

      name_cell <- function(label) {
        wrap_elements(grid::textGrob(
          label,
          x = unit(1, "npc") - unit(3, "pt"),
          hjust = 1,
          gp = grid::gpar(fontsize = 8, col = "grey15")
        ))
      }

      column_header <- function(label) {
        wrap_elements(grid::textGrob(
          label,
          gp = grid::gpar(fontsize = 12, fontface = "bold", col = "grey10")
        ))
      }

      group_banner <- function(label) {
        wrap_elements(full = grid::textGrob(
          label,
          x = unit(4, "pt"),
          y = unit(0.5, "npc"),
          hjust = 0,
          vjust = 0.5,
          gp = grid::gpar(fontsize = 12, fontface = "bold", col = "grey10")
        ))
      }

      make_row <- function(response_label, transformation, raw, transformed, standardized) {
        fill <- unname(fill_colours[[transformation]])
        name_cell(response_label) +
          density_panel(raw, "grey70") +
          density_panel(transformed, fill) +
          density_panel(
            standardized,
            fill,
            limits = std_limits,
            breaks = std_breaks,
            zero_line = TRUE
          ) +
          plot_layout(widths = col_widths)
      }

      header <- plot_spacer() +
        column_header("Raw") +
        column_header("Transformed") +
        column_header("Standardized") +
        plot_layout(widths = col_widths)

      pieces <- list(header)
      heights <- 1
      for (group_name in levels(nested$group)) {
        dat_g <- nested |>
          filter(group == group_name)
        pieces <- c(pieces, list(group_banner(group_name)))
        heights <- c(heights, 0.85)
        rows <- dat_g |>
          select(response_label, transformation, raw, transformed, standardized) |>
          purrr::pmap(make_row)
        pieces <- c(pieces, rows)
        heights <- c(heights, rep(1, length(rows)))
      }

      legend_plot <- ggplot(
        tibble(transformation = factor(
          c("log", "logit", "shift", "none"),
          levels = c("log", "logit", "shift", "none")
        )),
        aes(x = 1, y = 1, fill = transformation)
      ) +
        geom_col() +
        scale_fill_manual(
          values = fill_colours,
          labels = c("Log", "Logit", "Shift", "None"),
          name = "Transformation"
        ) +
        theme_void() +
        theme(legend.position = "bottom")
      legend_grob <- ggplotGrob(legend_plot)
      legend_only <- legend_grob$grobs[[which(vapply(
        legend_grob$grobs,
        function(x) x$name,
        character(1)
      ) == "guide-box")]]

      pieces <- c(pieces, list(wrap_elements(full = legend_only)))
      heights <- c(heights, 1.15)

      reduce(pieces, function(a, b) a / b) +
        plot_layout(heights = heights)
    }
  ),

  # correlation matrix
  tar_target(
    name = correlation_plot,
    command = {

      function_table <- big_data |>
        select(-year,-value_trans, -value_std, -unit, -temperature_degree, -habitat,
               -temperature_scaled, -precipitation_mm, -precipitation_name,
               -precipitation_scaled, -fg_richness, -fg_remaining, -forb, -gram, -bryo, -total_biomass_scaled, -biomass_scaled_B, -biomass_scaled_F, -biomass_scaled_G) |>
        pivot_wider(names_from = response, values_from = value, values_fill = 0) |>
        select(-c(siteID:group))

      corr <- round(cor(function_table, use = "pairwise.complete.obs"), 1)
      p.mat <- ggcorrplot::cor_pmat(function_table)

      ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower",
                 colors = c("#6D9EC1", "white", "#E46726"),
                 lab = TRUE,
                 lab_size = 4,
                 tl.cex = 14,
                 tl.srt = 45) +
        theme(axis.text = element_text(size = 8))

    }
  ),

  # Biomass plot
  tar_target(
    name = biomass_plot,
    command = standing_biomass |>
      tidylog::left_join(
        meta |>
          select(siteID, blockID, plotID, treatment, fg_remaining, temperature_degree, precipitation_mm, habitat),
        by = c("siteID", "blockID", "plotID", "treatment")
      ) |>
      mutate(no_treatment = case_when(
        is.na(no_treatment) ~ "remaining",
        no_treatment == "leftover" ~ "removed",
        TRUE ~ no_treatment
      )) |>
      ggplot(aes(x = fg_remaining, y = biomass, fill = fg_id)) +
      geom_col() +
      scale_fill_manual(values = c("orange", "purple", "green")) +
      labs(
        x = "FG remaining",
        y = "Standing biomass (g m^-2)",
        fill = "FG"
      ) +
      facet_grid(no_treatment ~ habitat) +
      theme_bw()
  ),

  # Biomass variation
  tar_target(
    name = biomass_variation_plot,
    command = standing_biomass |>
      mutate(no_treatment = case_when(
        is.na(no_treatment) ~ "remaining",
        no_treatment == "leftover" ~ "removed",
        TRUE ~ no_treatment
      )) |>
      ggplot(aes(x = log(biomass), fill = fg_id)) +
      geom_histogram() +
      scale_fill_manual(values = c("orange", "purple", "green")) +
      labs(x = "log(Biomass) (g m^-2)", fill = "FG") +
      facet_grid(no_treatment ~ fg_id) +
      theme_bw()
  )

)
