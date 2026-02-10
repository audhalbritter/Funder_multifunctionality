# Multifunctionality plan

multifunctionality_plan <- list(

  # merge datasets with different functions
  tar_target(
    name = big_data_raw,
    command = bind_rows(
      # primary producers
      plant_biomass,
      root_biomass,
      #root_traits,
      plant_richness,

      # higher trophic levels
      nematode_density,
      microarthropod_density,
      nematode_ecosytem_condition,
      #TBA fungi and bacteria density

      # carbon cycle
      decomposition_forbs,
      decomposition_gram,
      # carbon and nitrogen stocks
      cn_stocks |> filter(response != "CN"),
      gpp,
      nee,
      reco,

      # nutrient cycle
      # nitrogen stocks added above
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
      # shift values to make all positive (for responses with negative values)
      group_by(response) |>
      mutate(value = case_when(
        response %in% c("micro nutrients") ~ value + abs(min(value, na.rm = TRUE)) + 1,
        TRUE ~ value
      )) |>
      ungroup() |> 
      # normalize data
      # if zeros in data (nematodes and microarthropods), then there will be NAs here
      tidylog::mutate(value_trans = case_when(
        # log for responses with only positive values
        response %in% c("biomass", "root biomass", "microarthropod density", "nematode density", "carbon", "nitrogen", "phosphate", "gpp", "micro nutrients") ~ log(value),
        # no transformation for others
        TRUE ~ value
      )) |>
      # scale variables between 0 and 1
      group_by(data_type, group, response) |>
      mutate(value_std = scale(value_trans)[, 1]) |>
      # transform
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
      filter(!response %in% c("decomposition forbs", "reco")) |> 
      group_by(
        siteID, blockID, plotID, treatment, habitat,
        temperature_degree, precipitation_mm, precipitation_name,
        temperature_scaled, precipitation_scaled,
        fg_richness, fg_remaining, forb, gram, bryo
      ) |>
      summarise(
        multifuntionality = mean(value_std, na.rm = TRUE),
        se = sd(value_std, na.rm = TRUE) / sqrt(n()),
        #data_type = dplyr::first(data_type),
        #group     = dplyr::first(group),
        .groups = "drop"
      ) |> 
      # global level (useful for group_by map)
      mutate(level = "global")

  )
)

