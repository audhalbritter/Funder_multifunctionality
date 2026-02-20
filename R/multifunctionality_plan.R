# Multifunctionality plan
multifunctionality_plan <- list(

  # merge datasets with different functions
  tar_target(
    name = big_data_raw,
    command = bind_rows(
      # primary producers
      # plant_biomass,
      # root_biomass,
      # plant_llitter,

      # nematodes
      # nematode_density,
      nematode_feeding_group_density,
      nematode_fungal_bacterial_feeder_ratio,
      nematode_indices,

      # fungi and bacteria
      microbial_density,
      microbial_ratio,
      fungal_fg_density,
      fungal_necromass,

      # carbon cycle
      decomposition_forbs,
      decomposition_gram,
      cn_stocks |> filter(response != "CN"), # includes nitrogen stocks
      #som,
      gpp,
      #nee,
      reco,

      # nutrient cycle
      phosphorus_stock,
      available_nutrients,

      # microenvironment
      microclimate |> 
        filter(!response %in% c("mean soil moisture", "average min ground temperature", "average max ground temperature"))
  

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
        response %in% c("avialable_micronutrients") ~ value + abs(min(value, na.rm = TRUE)) + 1,
        TRUE ~ value
      )) |> 
      ungroup() |> 
      # normalize data
      # if zeros in data (nematodes and microarthropods), then there will be NAs here
      tidylog::mutate(value_trans = case_when(
        # logit for ratio variables (proportions in 0-1) and fungal guilds; exclude "respiration" (contains "ratio")
        str_detect(response, "ratio") & !str_detect(response, "respiration") ~ qlogis(pmin(pmax(value, 0.001), 0.999)),
        response %in% c("root_asscociated_sapro", "other", "sapro", "yeast_dimorphic", "animal_parasite", "mycorrhiza", "lichenized", "plant_pathogen") ~ qlogis(pmin(pmax(value, 0.001), 0.999)),
        # log for responses with only positive values
        response %in% c("available_nitrogen", "available_phosphorus", "bacteria density", "bacterivore feeder density", "bacterivore density", "carbon stock", "fungi density", "fungivore feeder density", "nematode density", "nitrogen stock", "phosphorus stock", "plant_feeder feeder density", "predator feeder density", "soil organic matter") ~ log(value + 1),
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
      mutate(treatment = factor(treatment, levels = c("C", "F", "G", "B", "GF", "FB", "GB", "FGB"))) |>
      ungroup()
  ),

  # contrast to bare ground (FGB): difference treatment - FGB per block and response
  # Pivot by (siteID, blockID, response) so we get all 8 treatment values in one row;
  # including plotID would create one row per plot with only one treatment filled -> NAs
  tar_target(
    name = big_data_contrast,
    command = {
      contrast_wide <- big_data |>
        select(siteID, blockID, treatment, response, value_std) |>
        pivot_wider(names_from = treatment, values_from = value_std) |>
        mutate(across(any_of(c("C", "F", "G", "B", "GF", "FB", "GB")), ~ .x - FGB))

      contrast_long <- contrast_wide |>
        pivot_longer(cols = any_of(c("C", "F", "G", "B", "GF", "FB", "GB")), names_to = "treatment", values_to = "value_contrast") |>
        filter(!is.na(value_contrast)) |>
        mutate(treatment = factor(treatment, levels = c("C", "F", "G", "B", "GF", "FB", "GB")))
      contrast_long |>
        left_join(
          big_data |> select(-plotID, -value, -value_trans, -value_std),
          by = c("siteID", "blockID", "treatment", "response")
        )
    }
  ),

  # average multifunctionality
  tar_target(
    name = multifunctionality,
    command = big_data |>
      # SHOULD NOT NEED TO FILTER THIS!
      filter(!response %in% c("decomposition forbs", "ecosystem respiration", "macronutrients", "root_tissue_density_g_per_m3", "root_dry_matter_content")) |> 
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

  ),

    # average multifunctionality by group
  tar_target(
    name = multifunctionality_group,
    command = big_data |>
      # SHOULD NOT NEED TO FILTER THIS!
      filter(!response %in% c("decomposition forbs", "ecosystem respiration", "macronutrients", "root_tissue_density_g_per_m3", "root_dry_matter_content")) |> 
      group_by(
        siteID, blockID, plotID, treatment, habitat,
        temperature_degree, precipitation_mm, precipitation_name,
        temperature_scaled, precipitation_scaled,
        fg_richness, fg_remaining, forb, gram, bryo, group
      ) |>
      summarise(
        multifuntionality = mean(value_std, na.rm = TRUE),
        se = sd(value_std, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) |> 
      # group level (useful for group_by map)
      mutate(level = group)

  )
)

