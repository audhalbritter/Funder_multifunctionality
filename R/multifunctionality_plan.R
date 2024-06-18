# Multifunctionality plan

multifunctionality_plan <- list(

  # merge datasets
  tar_target(
    name = big_data_raw,
    command = bind_rows(
      plant_biomass,
      root_productivity,
      #plant_richness,
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
    command = {

      big <- big_data_raw |>
      # normalize data
      mutate(value_trans = if_else(response %in% c("biomass", "organic matter", "ammonium", "microarthropod density", "nitrate", "phosphate"), log(value), value)) |>
      # scale variables between 0 and 1
      group_by(data_type, group, response) |>
      mutate(value_std = rescale(value_trans),
             temperature_degree_std = rescale(temperature_degree),
             precipitation_mm_std = rescale(precipitation_mm))

      # temp <- scale(big$temperature_degree) |>
      #   as.tibble() |>
      #   rename(temperature_degree_std = V1)
      #
      # prec <- scale(big$precipitation_mm) |>
      #   as.tibble() |>
      #   rename(precipitation_mm_std = V1)
      #
      # big <- big |>
      #   bind_cols(temp, prec)



      # get max value for each function
      ### CHECK IF ALL VALUES ARE POSITIVE -> LARGER VALUES IS MORE FUNCTION
      # mutate(max_value = max(value, na.rm = TRUE), .by = c("data_type", "group", "response")) |>
      # mutate(value_std = abs(value / max_value))

      }


  ),

  # average multifunctionality
  tar_target(
    name = multifunctionality,
    command = big_data |>
      # SHOULD NOT NEED TO FILTER THIS!
      filter(!response %in% c("decomposition forbs", "decomposition graminoids", "root productivity")) |>
      group_by(year, siteID, blockID, plotID, treatment, habitat, temperature_degree, precipitation_mm, precipitation_name, data_type, group, fg_richness, fg_remaining) |>
      summarise(multifuntionality = mean(value_std, na.rm = TRUE),
                se = sd(value, na.rm = TRUE)/sqrt(n()))

  )
)

