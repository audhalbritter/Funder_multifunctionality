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

  # Raw data
  tar_target(
    name = raw_functions_plot,
    command = big_data_raw |>
      rename_response_pretty(response) |>
      ggplot(aes(x = value)) +
      geom_histogram() +
      labs(x = "Raw functions") +
      facet_wrap(~ response, scales = "free")
  ),

  # Normalize functions
  tar_target(
    name = normalize_functions_plot,
    command = big_data_raw |> 
      rename_response_pretty(response) |>
      # shift values to make all positive (for responses with negative values)
      group_by(response) |>
      mutate(value = case_when(
        response %in% c("avialable_micronutrients") ~ value + abs(min(value, na.rm = TRUE)) + 1,
        TRUE ~ value
      )) |> 
      ungroup() |> 
      # normalize data
      # if zeros in data (nematodes and microarthropods), then there will be NAs here
      tidylog::mutate(value_trans = case_when(
        # logit for ratio variables (proportions in 0-1) and fungal guilds; exclude "respiration" (contains "ratio")
        str_detect(response, "ratio") & !str_detect(response, "respiration") ~ qlogis(pmin(pmax(value, 0.001), 0.999)),
        response %in% c("root_associated_fungi", "saprotroph", "yeast_dimorphic", "ectomycorrhizal") ~ qlogis(pmin(pmax(value, 0.001), 0.999)),
        # log for responses with only positive values
        response %in% c("available_nitrogen", "available_phosphorus", "bacteria density", "bacterivore feeder density", "bacterivore density", "carbon stock", "fungi density", "fungivore feeder density", "nematode density", "nitrogen stock", "phosphorus stock", "plant_feeder feeder density", "predator feeder density", "soil organic matter") ~ log(value + 1),
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
      rename_response_pretty(response) |>
      ggplot(aes(x = value_std)) +
      geom_histogram() +
      labs(x = "Standardized value") +
      facet_wrap(~ response, scales = "free")
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

      # pairwise.complete.obs avoids NA in cor matrix (root_biomass, root_traits have a few NAs)
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
      ggplot(aes(x = log(biomass), fill = fg_id)) +
      geom_histogram() +
      scale_fill_manual(values = c("orange", "purple", "green")) +
      labs(x = "log(Biomass) (g m^-2)", fill = "FG") +
      facet_grid(no_treatment ~ fg_id) +
      theme_bw()
  )

)
