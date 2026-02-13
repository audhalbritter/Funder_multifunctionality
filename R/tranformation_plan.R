# data curation plan
transformation_plan <- list(
  # metadata
  tar_target(
    name = meta,
    command = create_funder_meta_data() %>%
      funcabization(dat = ., convert_to = "FunCaB") |>
      mutate(
        temperature_degree = case_when(
          siteID %in% c("Skjelingahaugen", "Gudmedalen", "Lavisdalen", "Ulvehaugen") ~ 6.5,
          siteID %in% c("Veskre", "Rambera", "Hogsete", "Alrust") ~ 8.5,
          siteID %in% c("Ovstedalen", "Arhelleren", "Vikesland", "Fauske") ~ 10.5
        ),
        habitat = case_when(
          temperature_degree == 6.5 ~ "alpine",
          temperature_degree == 8.5 ~ "sub-alpine",
          temperature_degree == 10.5 ~ "boreal"
        ),
        habitat = factor(habitat, levels = c("alpine", "sub-alpine", "boreal")),
        precipitation_mm = case_when(
          siteID %in% c("Fauske", "Alrust", "Ulvehaugen") ~ 700,
          siteID %in% c("Vikesland", "Hogsete", "Lavisdalen") ~ 1400,
          siteID %in% c("Arhelleren", "Rambera", "Gudmedalen") ~ 2100,
          siteID %in% c("Ovstedalen", "Veskre", "Skjelingahaugen") ~ 2800
        ),
        precipitation_name = paste0(precipitation_mm, " ", "mm"),
        precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm")),
        fg_remaining = case_when(
          treatment == "FGB" ~ "None",
          treatment == "FB" ~ "G",
          treatment == "GB" ~ "F",
          treatment == "GF" ~ "B",
          treatment == "B" ~ "GF",
          treatment == "F" ~ "GB",
          treatment == "G" ~ "FB",
          treatment == "C" ~ "All"
        ),
        fg_remaining = factor(fg_remaining, levels = c("None", "G", "F", "B", "GF", "GB", "FB", "All"))
      ) |>
      # scale temperature and precipitation variable
      mutate(
        temperature_scaled = scale(temperature_degree)[, 1],
        precipitation_scaled = scale(precipitation_mm)[, 1]
      )
  ),


  # prep biomass
  # 2015 - 2021 (maybe remove)
  tar_target(
    name = biomass,
    command = biomass_raw |>
      # remove extra plots in 2016, select last year
      filter(
        treatment != "XC",
        year == 2021
      ) |>
      # sum biomass from different rounds
      group_by(year, siteID, blockID, plotID, treatment, removed_fg) |>
      summarise(value = sum(biomass)) |>
      # convert to g per m2
      mutate(value = value * 10000 / 625) |>
      ungroup() |>
      mutate(
        data_type = "function",
        group = "primary producers",
        response = "biomass",
        unit = "g m\u207B\u00B2"
      ) |>
      select(year:removed_fg, data_type, group, response, value, unit)
  ),

  # 2022
  tar_target(
    name = plant_biomass,
    command = biomass_22_raw |>
      # sum biomass from different rounds and functional group (add removed_fg to group_by to keep them)
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = sum(biomass), .groups = "drop") |>
      # convert to g per m2
      mutate(value = value * 10000 / 625) |>
      mutate(
        data_type = "function",
        group = "primary producers",
        response = "aboveground biomass",
        unit = "g m\u207B\u00B2"
      ) |>
      select(year:treatment, data_type, group, response, value, unit)
  ),

  # root productivity and traits
  # Ram2 GB is missing (5GB)
  tar_target(
    name = root_biomass,
    command = root_biomass_raw |>
      mutate(year = 2021) |>
      select(year, siteID, blockID, plotID, treatment, value = root_biomass) |>
      tidylog::filter(!is.na(value)) |>
      # convert to g per 625 cm2
      mutate(
        ric_radius = 3 / 2, # ric is 3 cm in diameter
        ric_area = pi * ric_radius^2,
        value = value * 10000 / ric_area
      ) |>
      mutate(
        data_type = "function",
        group = "primary producers",
        response = "root biomass",
        unit = "g m\u207B\u00B2"
      ) |>
      select(-ric_radius, -ric_area)
  ),

  # root traits
  tar_target(
    name = root_traits,
    command = root_traits_raw |>
      select(year:treatment, response = "trait", value) |>
      filter(!is.na(value)) |>
      filter(response %in% c("specific_root_length_m_per_g", "root_tissue_density_g_per_m3", "root_dry_matter_content")) |>
      mutate(
        data_type = "function",
        group = "primary producers",
        unit = case_when(
          response == "specific_root_length_m_per_g" ~ "m g\u207B\u00B9",
          response == "root_tissue_density_g_per_m3" ~ "g m\u207B\u00B3",
          response == "root_dry_matter_content" ~ "unitless",
          TRUE ~ NA_character_
        )
      )
  ),

  # community
  tar_target(
    name = community,
    command = community_raw |>
      # remove extra plots in 2016, select last year
      filter(
        treatment != "XC",
        year == 2022
      )
  ),

  # plant richness (vascular and bryophytes)
  tar_target(
    name = plant_richness,
    command = community |>
      # remove where species are NA, treatments with only bryophytes etc. Is only needed if intersted in moss hieght, litter cover etc.
      filter(!is.na(species)) |>
      bind_rows(bryophyte_richness) |>
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = n()) |>
      mutate(
        data_type = "biodiversity",
        group = "primary producers",
        response = "plant richness",
        unit = "count per 625 cm\u00B2"
      )
  ),

  # bryophyte richness
  tar_target(
    name = bryophyte_richness,
    command = bryophyte_raw %>%
      dataDocumentation::funcabization(dat = ., convert_to = "FunCaB") |>
      mutate(year = 2022)
    # group_by(year, siteID, blockID, plotID, treatment) |>
    # summarise(value = n()) |>
    # mutate(data_type = "biodiversity",
    #        group = "primary producers",
    #        response = "plant richness",
    #        unit = "count")
  ),

  # prep microarthropod
  tar_target(
    name = microarthropod,
    command = microarthropod_raw |>
      mutate(
        siteID = case_when(
          siteID == "Gud" ~ "Gudmedalen",
          siteID == "Lav" ~ "Lavisdalen",
          siteID == "Ram" ~ "Rambera",
          siteID == "Ulv" ~ "Ulvehaugen",
          siteID == "Skj" ~ "Skjelingahaugen",
          siteID == "Alr" ~ "Alrust",
          siteID == "Arh" ~ "Arhelleren",
          siteID == "Fau" ~ "Fauske",
          siteID == "Hog" ~ "Hogsete",
          siteID == "Ovs" ~ "Ovstedalen",
          siteID == "Vik" ~ "Vikesland",
          siteID == "Ves" ~ "Veskre",
          TRUE ~ siteID
        )
      ) |>
      select(year, siteID, blockID, plotID, treatment, functional_group, abundance, sample_weight_g) |>
      # remove missing data (1 plot, Fau2FB)
      filter(!is.na(abundance)) |>
      mutate(abundance_per_g_dry_soil = abundance / sample_weight_g)
  ),
  tar_target(
    name = microarthropod_density,
    command = microarthropod |>
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = sum(abundance_per_g_dry_soil)) |>
      mutate(
        data_type = "function",
        group = "higher trophic level",
        response = "microarthropod density",
        unit = "count g\u207B\u00B9"
      )
  ),

  # prep nematodes
  tar_target(
    name = nematode,
    command = nematode_raw |>
      select(year:treatment, family, functional_group, value = total_nematode_abundance_per_g_dry_soil, per_family_abundance_per_g_dry_soil) |>
      filter(value != 0)
    # filter(!family %in% c("unknown", "unknown_bacterial_feeder", "unknown_plant_feeder")) |>
  ),
  tar_target(
    name = nematode_density,
    command = nematode |>
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = sum(value)) |>
      mutate(
        data_type = "function",
        group = "higher trophic level",
        response = "nematode density",
        unit = "count g\u207B\u00B9"
      )
  ),
  tar_target(
    name = nematode_ecosytem_condition,
    command = nematode |>
      filter(!family %in% c("unknown", "unknown_bacterial_feeder", "unknown_plant_feeder")) |>
      # Tripylidae does not join because it is filtered out
      # Aphelenchidae is in the family table but not in the data
      tidylog::left_join(
        nematode_cp |>
          mutate(Family = tolower(Family)) |>
          rename(family = Family),
        by = "family"
      ) |>
      # remove observations with no cp_group (26% removed)
      tidylog::filter(!is.na(cp_group)) |>
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(
        value = weighted.mean(as.numeric(cp_group), per_family_abundance_per_g_dry_soil),
        .groups = "drop"
      ) |>
      mutate(
        data_type = "function",
        group = "higher trophic level",
        response = "nematode ecosystem condition",
        unit = "unitless"
      )
  ),

  # microbial fungal:bacterial ratio (fungal density / bacterial density per plot)
  tar_target(
    name = microbial_density,
    command = microbial_raw |>
      filter(!is.na(abundance_per_g)) |>
      pivot_wider(
        id_cols = c(year, siteID, blockID, plotID, treatment),
        names_from = group,
        values_from = abundance_per_g
      ) |>
      filter(!is.na(bacteria), !is.na(fungi), bacteria > 0) |>
      mutate(
        value = fungi / bacteria,
        response = "fungal bacterial ratio"
      ) |> 
      select(year, siteID, blockID, plotID, treatment, response, value) |>
      mutate(
        data_type = "function",
        group = "higher trophic level",
        unit = "unitless"
      )
  ),


  # carbon and nutrient stocks and fluxes
  # decomposition
  tar_target(
    name = decomposition,
    command = decomposition_raw %>%
      dataDocumentation::funcabization(dat = ., convert_to = "FunCaB") |>
      mutate(relative_weight_loss = if_else(relative_weight_loss < 0 & relative_weight_loss > -0.04, 0, relative_weight_loss)) |>
      tidylog::filter(!is.na(relative_weight_loss)) |>
      tidylog::filter(relative_weight_loss >= 0) |>
      select(siteID:treatment, litter_type, value = relative_weight_loss) |>
      mutate(
        year = 2022,
        data_type = "function",
        group = "carbon cycling",
        unit = "%"
      )
  ),
  tar_target(
    name = decomposition_forbs,
    command = decomposition |>
      filter(litter_type == "forb") |>
      mutate(response = "decomposition forbs") |>
      tidylog::filter(value != 0) |>
      select(-litter_type)
  ),
  tar_target(
    name = decomposition_gram,
    command = decomposition |>
      filter(litter_type == "graminoid") |>
      mutate(response = "decomposition graminoids") |>
      tidylog::filter(value != 0) |>
      select(-litter_type)
  ),

  # cn stocks
  tar_target(
    name = cn_stocks,
    command = cn_raw |>
      select(year:plotID, response = variable, value) |>
      mutate(
        data_type = "function",
        response = case_when(
          response == "C" ~ "carbon stock",
          response == "N" ~ "nitrogen stock",
          response == "CN" ~ "CN",
          TRUE ~ response
        ),
        group = case_when(
          response %in% c("carbon stock", "CN") ~ "carbon cycling",
          response == "nitrogen stock" ~ "nutrient cycling",
          TRUE ~ NA_character_
        ),
        unit = "g g\u207B\u00B9"
      )
  ),

  # macronutrients
  tar_target(
    name = macronutrients,
    command = macronutrients_raw |>
      filter(name == "phosphorous") |>
      # convert from mg per 100g dry soil to g per g dry soil
      mutate(value = value / 100) |>
      select(year, siteID, blockID, plotID, treatment, value) |>
      mutate(
        data_type = "function",
        response = "phosphorus stock",
        group = "nutrient cycling",
        unit = "g g\u207B\u00B9"
      )
  ),

  # available nutrients
  # nitrogen and phosphorus
  tar_target(
    name = available_np,
    command = available_nutrients_raw |>
      mutate(year = year(retrieval_date)) |>
      # convert to <U+00B5>g per m2 per 35 days
      mutate(value = value * 10000 / 10) |>
      filter(elements == "P") |>
      mutate(
        data_type = "function",
        group = "nutrient cycling",
        response = "available phosphate",
        unit = "\u00B5g m\u207B\u00B2 35 days"
      ) |>
      select(year, siteID, blockID, plotID, treatment, data_type, group, response, value, unit)
  ),

  # make ordination for other available nutrients
  tar_target(
    name = other_available_nutrients,
    command = make_nutrient_pca(
      available_nutrients_raw |>
        filter(!elements %in% c("NH4-N", "NO3-N", "P")),
      meta
    )
  ),
  tar_target(
    name = macronutrients_pca,
    command = make_nutrient_pca(
      available_nutrients_raw |>
        filter(elements %in% c("NH4-N", "NO3-N", "P", "K", "Ca", "Mg", "S")),
      meta
    )
  ),
  tar_target(
    name = micronutrients_pca,
    command = make_nutrient_pca(
      available_nutrients_raw |>
        filter(elements %in% c("Fe", "Mn", "Zn", "Cu", "B", "Al", "Pb")),
      meta
    )
  ),

  # make ordination for other available nutrients
  tar_target(
    name = available_nutrients,
    command = bind_rows(
      macronutrients = macronutrients_pca[[1]],
      micronutrients = micronutrients_pca[[1]],
      .id = "response"
    ) |>
      mutate(
        data_type = "function",
        group = "nutrient cycling",
        unit = "PCA axis 1"
      ) |>
      select(siteID:year, response, value = PC1, data_type:unit)
  ),

  # # make ordination for other available nutrients
  # tar_target(
  #   name = available_nutrients,
  #   command = bind_rows(
  #     available_np,
  #     other_available_nutrients[[1]] |>
  #       mutate(
  #         data_type = "function",
  #         group = "nutrient cycling",
  #         response = "available micro nutrients",
  #         unit = "PCA axis 1"
  #       ) |>
  #       select(siteID:year, value = PC1, data_type:unit)
  #   )
  # ),

  # cflux
  # prep data
  tar_target(
    name = cflux,
    command = cflux_raw |>
      filter(year == 2017) |>
      filter(!treatment %in% c("RTC", "XC")) |>
      mutate(
        data_type = "function",
        group = "carbon cycling",
        unit = "\u00B5mol m\u207B\u00B2 s\u207B\u00B9"
      )
    # Negative NEE values reflect CO2 uptake in the ecosystem, and positive values reflect CO2 release from the ecosystem to the atmosphere -> needs to be converted to + ecosystem uptake and - is release to atmosphere
  ),

  # gpp
  tar_target(
    name = gpp,
    command = cflux |>
      mutate(gpp = -1 * gpp) |>
      group_by(year, siteID, blockID, plotID, treatment, data_type, group, unit) |>
      summarise(
        value = mean(gpp),
        .groups = "drop"
      ) |>
      mutate(response = "gross primary productivity")
  ),

  # nee
  tar_target(
    name = nee,
    command = cflux |>
      # transform data to not have negative values
      ### IS THIS CORRECT OR SHOULD WE USE SCALE? !!!!
      mutate(nee = nee + 15) |>
      group_by(year, siteID, blockID, plotID, treatment, data_type, group, unit) |>
      summarise(
        value = mean(nee),
        .groups = "drop"
      ) |>
      mutate(response = "net ecosystem exchange")
  ),


  # reco
  tar_target(
    name = reco,
    command = cflux |>
      group_by(year, siteID, blockID, plotID, treatment, data_type, group, unit) |>
      summarise(
        value = mean(Reco),
        .groups = "drop"
      ) |>
      mutate(response = "ecosystem respiration")
  )
)
