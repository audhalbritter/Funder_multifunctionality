# Multifunctionality plan

multifunctionality_plan <- list(

  # merge datasets with different functions
  tar_target(
    name = big_data_raw,
    command = bind_rows(
      # primary producers
      plant_biomass,
      root_biomass,
      root_productivity,
      root_turnover,

      # higher trophic levels
      nematode_density,
      microarthropod_density,

      # carbon cycle
      decomposition_forbs,
      decomposition_gram,
      organic_matter,
      gpp,
      nee,
      reco,

      # nutrient cycle
      available_nutrients

    ) |>
      # add number of functional groups
      mutate(fg_richness = case_when(treatment == "C" ~ 3,
                                             treatment %in% c("F", "G", "B") ~ 2,
                                             treatment %in% c("FB", "GF", "GB") ~ 1,
                                             treatment == "FGB" ~ 0)) |>
      # add metadata
      left_join(meta, by = c("siteID", "blockID", "plotID", "treatment"))
  ),

  # transformation: normaliue (log) and standardize between 0 and 1
  tar_target(
    name = big_data,
    command = big_data_raw |>
      # normalize data
        ### WARNING WHY???  !!!
      mutate(value_trans = if_else(response %in% c("biomass", "root biomass", "root turnover", "microarthropod density", "organic matter", "nitrogen", "phosphate"), log(value), value)) |>
      # scale variables between 0 and 1
      group_by(data_type, group, response) |>
      mutate(value_std = scale(value_trans)[, 1]) |>
      # trasnf
      mutate(forb = if_else(str_detect(treatment, "F"), 0, 1),
             gram = if_else(str_detect(treatment, "G"), 0, 1),
             bryo = if_else(str_detect(treatment, "B"), 0, 1)) |>
    # make treatment factor and sort
    mutate(treatment = factor(treatment, levels = c("C", "F", "G", "B", "GF", "FB", "GB", "FGB"))) |>
      ungroup()
      #mutate(value_std = rescale(value_trans))

      # get max value for each function (different approach, remove?)
      ### CHECK IF ALL VALUES ARE POSITIVE -> LARGER VALUES IS MORE FUNCTION
      # mutate(max_value = max(value, na.rm = TRUE), .by = c("data_type", "group", "response")) |>
      # mutate(value_std = abs(value / max_value))

  ),

  # average multifunctionality
  tar_target(
    name = multifunctionality,
    command = big_data |>
      # SHOULD NOT NEED TO FILTER THIS!
      filter(!response %in% c("decomposition forbs", "Reco", "root productivity", "root turnover")) |>
      group_by(year, siteID, blockID, plotID, treatment, habitat, temperature_degree, precipitation_mm, precipitation_name, temperature_scaled, precipitation_scaled, data_type, group, fg_richness, fg_remaining, forb, gram, bryo) |>
      summarise(multifuntionality = mean(value_std, na.rm = TRUE),
                se = sd(value_std, na.rm = TRUE)/sqrt(n())) |>
      # global level (useful for group_by map)
      mutate(level = "global") |>
      ungroup()

  )
)

