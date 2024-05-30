# Multifunctionality plan

multifunctionality_plan <- list(

  # merge datasets
  tar_target(
    name = big_data_raw,
    command = bind_rows(
      plant_biomass,
      root_productivity,
      plant_richness,
      #plant_litter,
      nematode_density,
      microarthropod_density,
      decomposition_forbs,
      decomposition_gram,
      organic_matter,
      available_nutrients,
      gpp,
      reco
    ) |>
      # add number of functional groups
      mutate(fg_richness = case_when(treatment == "C" ~ 3,
                                             treatment %in% c("F", "G", "B") ~ 2,
                                             treatment %in% c("FB", "GF", "GB") ~ 1,
                                             treatment == "FGB" ~ 0)) |>
      # add metadata
      left_join(meta, by = c("siteID", "blockID", "plotID", "treatment"))
  ),

  # transformation: standardize between 0 and 1
  tar_target(
    name = big_data,
    command = big_data_raw |>
      # get max value for each function
      ### CHECK IF ALL VALUES ARE POSITIVE -> LARGER VALUES IS MORE FUNCTION
      mutate(max_value = max(value, na.rm = TRUE), .by = c("data_type", "trophic_level", "response")) |>
      mutate(value_std = abs(value / max_value))
             #threshold = 0.3,
             #above = if_else(value_std >= threshold, 1, 0))


  ),

  # average multifunctionality
  tar_target(
    name = multifunctionality,
    command = big_data |>
      group_by(year, siteID, blockID, plotID, treatment, habitat, precipitation_mm, precipitation_name, data_type, trophic_level, fg_richness, fg_remaining) |>
      summarise(multifuntionality = mean(value_std, na.rm = TRUE),
                se = sd(value, na.rm = TRUE)/sqrt(n()))
                #above = sum(above))



  )
)
