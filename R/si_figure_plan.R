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

  # Raw/transformed shapes beside standardized values, groups stacked
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

      # scale_x = TRUE puts every curve on 0–1 so raw and transformed shapes can share a row.
      density_curve <- function(x, scale_x) {
        x <- x[is.finite(x)]
        if (length(unique(x)) < 2L) {
          return(tibble(x = c(0, 1), dens = c(0, 0)))
        }
        d <- stats::density(x, n = 256)
        span <- diff(range(d$x))
        tibble(
          x = if (!scale_x) {
            d$x
          } else if (span == 0) {
            rep(0.5, length(d$x))
          } else {
            (d$x - min(d$x)) / span
          },
          dens = d$y / max(d$y)
        )
      }

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

      # Unique y positions so each group facet keeps only its own labels.
      # A gap stops a neighbouring group's label from falling inside the expanded scale.
      response_pos <- base |>
        distinct(group, response_label) |>
        arrange(group, response_label) |>
        mutate(y_local = n() - row_number() + 1L, n_group = n(), .by = group)
      group_offset <- response_pos |>
        distinct(group, n_group) |>
        arrange(group) |>
        mutate(offset = cumsum(n_group + 2L) - n_group - 2L)
      response_pos <- response_pos |>
        left_join(group_offset, by = c("group", "n_group")) |>
        transmute(group, response_label, y0 = y_local + offset, label_y = y0 + 0.28)

      curve_layer <- function(dat, value_col, scale_x) {
        dat[[".value"]] <- dat[[value_col]]
        scale_x_flag <- scale_x
        dat |>
          summarise(
            curve = list(density_curve(.value, scale_x = scale_x_flag)),
            .by = c(group, response_label, transformation)
          ) |>
          tidyr::unnest(curve) |>
          left_join(response_pos, by = c("group", "response_label"))
      }

      # One curve when nothing was transformed. Otherwise grey raw under the coloured curve.
      raw_main <- base |>
        filter(transformation == "none") |>
        curve_layer("raw", scale_x = TRUE) |>
        mutate(layer = "main")
      raw_under <- base |>
        filter(transformation != "none") |>
        curve_layer("raw", scale_x = TRUE) |>
        mutate(layer = "raw")
      transformed_main <- base |>
        filter(transformation != "none") |>
        curve_layer("transformed", scale_x = TRUE) |>
        mutate(layer = "main")
      shape_dat <- bind_rows(raw_main, raw_under, transformed_main) |>
        mutate(transformation = factor(transformation, levels = c("log", "logit", "shift", "none")))

      std_dat <- base |>
        curve_layer("standardized", scale_x = FALSE) |>
        mutate(
          layer = "main",
          transformation = factor(transformation, levels = c("log", "logit", "shift", "none"))
        )

      y_breaks <- response_pos$label_y
      y_labels <- response_pos$response_label

      distribution_plot <- function(dat, title, x_label, show_labels, main_alpha) {
        dat |>
          ggplot(aes(x = x)) +
          geom_ribbon(
            data = function(d) dplyr::filter(d, layer == "raw"),
            aes(ymin = y0, ymax = y0 + 0.8 * dens, group = response_label),
            fill = "grey75",
            alpha = 0.4,
            colour = NA
          ) +
          geom_ribbon(
            data = function(d) dplyr::filter(d, layer == "main"),
            aes(ymin = y0, ymax = y0 + 0.8 * dens, group = response_label, fill = transformation),
            alpha = main_alpha,
            colour = NA
          ) +
          facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
          scale_fill_manual(
            values = fill_colours,
            breaks = c("log", "logit", "shift", "none"),
            labels = c("Log", "Logit", "Shift", "None"),
            name = "Transformation",
            drop = FALSE
          ) +
          scale_y_continuous(
            breaks = y_breaks,
            labels = if (show_labels) y_labels else rep("", length(y_labels)),
            expand = expansion(add = 0.2)
          ) +
          labs(title = title, x = x_label, y = NULL) +
          theme_bw(base_size = 8) +
          theme(
            axis.text.y = element_text(size = 7, hjust = 1),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 7),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing.y = unit(0.35, "lines"),
            strip.placement = "outside",
            panel.spacing = unit(0.12, "lines"),
            plot.title = element_text(face = "bold", size = 10, hjust = 0),
            legend.position = "none"
          )
      }

      p_shape <- distribution_plot(
        shape_dat,
        title = "Raw and transformed",
        x_label = NULL,
        show_labels = TRUE,
        main_alpha = 0.55
      ) +
        scale_x_continuous(breaks = NULL, expand = expansion(mult = 0.02)) +
        theme(
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(colour = NA, size = 7),
          axis.title.x = element_text(colour = NA, size = 8)
        )

      p_std <- distribution_plot(
        std_dat,
        title = "Standardized",
        x_label = "Standardized value",
        show_labels = FALSE,
        main_alpha = 0.9
      ) +
        theme(
          strip.text.y.left = element_blank(),
          strip.background.y = element_blank(),
          axis.text.y = element_blank()
        )

      legend_plot <- ggplot(
        tibble(transformation = factor(c("log", "logit", "shift", "none"), levels = c("log", "logit", "shift", "none"))),
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
      legend_only <- legend_grob$grobs[[which(vapply(legend_grob$grobs, function(x) x$name, character(1)) == "guide-box")]]

      (p_shape + p_std + plot_layout(widths = c(1.25, 1))) /
        wrap_elements(full = legend_only) +
        plot_layout(heights = c(1, 0.045))
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
