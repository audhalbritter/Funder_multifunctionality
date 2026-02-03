# data curation plan
transformation_plan <- list(

  # metadata
  tar_target(
    name = meta,
    command = create_funder_meta_data() %>%
      funcabization(dat = ., convert_to = "FunCaB") |>
      mutate(temperature_degree = case_when(siteID %in% c("Skjelingahaugen", "Gudmedalen", "Lavisdalen", "Ulvehaugen") ~ 6.5,
                                           siteID %in% c("Veskre", "Rambera", "Hogsete", "Alrust") ~ 8.5,
                                           siteID %in% c("Ovstedalen", "Arhelleren", "Vikesland", "Fauske") ~ 10.5),
             habitat = case_when(temperature_degree == 6.5 ~ "alpine",
                                 temperature_degree == 8.5 ~ "sub-alpine",
                                 temperature_degree == 10.5 ~ "boreal"),
             habitat = factor(habitat, levels = c("alpine", "sub-alpine", "boreal")),
             precipitation_mm = case_when(siteID %in% c("Fauske", "Alrust", "Ulvehaugen") ~ 700,
                                           siteID %in% c("Vikesland", "Hogsete", "Lavisdalen") ~ 1400,
                                           siteID %in% c("Arhelleren", "Rambera", "Gudmedalen") ~ 2100,
                                           siteID %in% c("Ovstedalen", "Veskre", "Skjelingahaugen") ~ 2800),
             precipitation_name = paste0(precipitation_mm, " ", "mm"),
             precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm")),
             fg_remaining = case_when(treatment == "FGB" ~ "None",
                                      treatment == "FB" ~ "G",
                                      treatment == "GB" ~ "F",
                                      treatment == "GF" ~ "B",
                                      treatment =="B" ~ "GF",
                                      treatment == "F" ~ "GB",
                                      treatment == "G" ~ "FB",
                                      treatment == "C" ~ "All"),
             fg_remaining = factor(fg_remaining, levels = c("None", "G", "F", "B", "GF", "GB", "FB", "All"))) |>
      # scale temperature and precipitation variable
      mutate(temperature_scaled = scale(temperature_degree)[, 1],
             precipitation_scaled = scale(precipitation_mm)[, 1])

  ),


  # prep biomass
  # 2015 - 2021 (maybe remove)
  tar_target(
    name = biomass,
    command = biomass_raw |>
      # remove extra plots in 2016, select last year
      filter(treatment != "XC",
             year == 2021) |>
      # sum biomass from different rounds
      group_by(year, siteID, blockID, plotID, treatment, removed_fg) |>
      summarise(value = sum(biomass)) |>
      ungroup() |>
      mutate(data_type = "function",
             group = "primary producers",
             response = "biomass",
             unit = "g") |>
      select(year:removed_fg, data_type, group, response, value, unit)
  ),

  # 2022
  tar_target(
    name = plant_biomass,
    command = biomass_22_raw |>
      # sum biomass from different rounds and functional group (add removed_fg to group_by to keep them)
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = sum(biomass), .groups = "drop") |>
      mutate(data_type = "function",
             group = "primary producers",
             response = "biomass",
             unit = "g") |>
      select(year:treatment, data_type, group, response, value, unit)
  ),

  # root productivity and traits
  # Ram2 GB is missing (5GB)
  tar_target(
    name = root_biomass,
    command = root_biomass_raw %>%
      ### THIS SHOULD BE DONE IN THE FUNDER GITHUB CLEANING CODE!!!
      funcabization(dat = ., convert_to = "FunCaB") |>
      mutate(year = 2021) |>
      select(year, siteID, blockID, plotID, treatment, value = root_biomass) |>
      mutate(data_type = "function",
             group = "primary producers",
             response = "root biomass",
             unit = "g m-3")
  ),

  # root traits
  tar_target(
    name = root_traits,
    command = root_traits_raw %>%
      ### THIS SHOULD BE DONE IN THE FUNDER GITHUB CLEANING CODE!!!
      # fix typo, should also be fixed in the cleanin code of Funder
      dataDocumentation::funcabization(dat = ., convert_to = "FunCaB") |>
      mutate(year = year(retrieval_date)) |>
      pivot_longer(cols = c(specific_root_length_m_per_g, root_tissue_density_g_per_m3, root_dry_matter_content), names_to = "root trait", values_to = "value") |>
      select(year, siteID:plotID, treatment, "root trait", value, duration) |>
      mutate(data_type = "function",
             group = "primary producers",
             response = "root traits",
             unit = "g m-3")
  ),

  # community
  tar_target(
    name = community,
    command = community_raw |>
      # remove extra plots in 2016, select last year
      filter(treatment != "XC",
             year == 2022)

  ),

  # richness
  tar_target(
    name = plant_richness,
    command = community |>
      # remove where species are NA, treatments with only bryophytes etc. Is only needed if intersted in moss hieght, litter cover etc.
      filter(!is.na(species)) |>
      group_by(year, siteID, blockID, plotID, treatment, functional_group) |>
      summarise(value = n()) |>
      mutate(data_type = "biodiversity",
             group = "primary producers",
             response = if_else(functional_group == "forb", "forb richness", "graminoid richness"),
             unit = "count")

  ),

  # bryophyte richness
  tar_target(
    name = bryophyte_richness,
    command = bryophyte_raw |>
      mutate(year = 2022) |>
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = n()) |>
      mutate(data_type = "biodiversity",
             group = "primary producers",
             response = "bryophyte richness",
             unit = "count")

  ),

  # prep microarthropod
  tar_target(
    name = microarthropod,
    command = microarthropod_raw |>
      select(year, siteID, blockID, plotID, treatment, functional_group, microarthropods, abundance)
    ),

  tar_target(
    name = microarthropod_density,
    command = microarthropod |>
      # remove missing data (1 plot, Fau2FB)
      filter(!is.na(abundance)) |>
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = sum(abundance)) |>
      mutate(data_type = "function",
             group = "higher trophic level",
             response = "microarthropod density",
             unit = "count")
  ),

  # prep nematodes
  tar_target(
    name = nematode_density,
    command = nematode_raw |>
      mutate(year = 2022) |>
      pivot_longer(cols = c(bacterivores_per_100g_dry_soil, fungivores_per_100g_dry_soil,
                             Omnivores_per_100g_dry_soil, Herbivores_per_100g_dry_soil,
                             Predators_per_100g_dry_soil), names_to = "functional_group",
                             values_to = "value") |>
      mutate(functional_group = str_remove(functional_group, "_per_100g_dry_soil"),
             data_type = "function",
             group = "higher trophic level",
             response = "nematode density",
             unit = "count per 100g soil")
  ),

  # carbon and nutrient stocks and fluxes
  # decomposition
  tar_target(
    name = decomposition,
    command = decomposition_raw |>
      mutate(rel_weight_loss2 = if_else(rel_weight_loss < 0 & rel_weight_loss > -0.04, 0, rel_weight_loss)) |>
      tidylog::filter(!is.na(rel_weight_loss)) |>
      tidylog::filter(rel_weight_loss >= 0) |>
      select(siteID:plotID, litter_type, value = rel_weight_loss) |>
      mutate(year = 2022,
             data_type = "function",
             group = "carbon cycling",
             unit = "%")
  ),

  tar_target(
    name = decomposition_forbs,
    command = decomposition |>
      filter(litter_type == "forbs") |>
      mutate(response = "decomposition forbs")
  ),

  tar_target(
    name = decomposition_gram,
    command = decomposition |>
      filter(litter_type == "graminoids") |>
      mutate(response = "decomposition graminoids")
  ),

  # cn stocks
  tar_target(
    name = cn_stocks,
    command = cn_raw |> 
    select(year:plotID, response = variable, value) |>
      mutate(response = if_else(response == "C", "carbon", "nitrogen"),
             data_type = "function",
             group = "carbon cycling",
             unit = "%")
  ),

  # available nutrients
  # nitrogen and phosphorus
  tar_target(
    name = available_np,
    command = available_nutrients_raw |>
      mutate(year = year(retrieval_date)) |>
      filter(elements == "P") |>
      mutate(data_type = "function",
             group = "nutrient cycling",
             response = "phosphate",
             unit = "micro grams/10cm2/35 days") |>
      select(year, siteID, blockID, plotID, treatment, data_type, group, response, value, unit)
  ),

  # make ordination for other available nutrients
  tar_target(
    name = other_available_nutrients,
    command = make_nutrient_pca(available_nutrients_raw, meta)
  ),

  # make ordination for other available nutrients
  tar_target(
    name = available_nutrients,
    command = bind_rows(
      available_np,
      other_available_nutrients[[1]] |>
        mutate(data_type = "function",
               group = "nutrient cycling",
               response = "micro nutrients",
               unit = NA) |>
        select(siteID:year, value = PC1, data_type:unit)
    )
  ),

  # cflux
  # prep data
  tar_target(
    name = cflux,
    command = cflux_raw |>
      filter(year == 2017) |>
      filter(!treatment %in% c("RTC", "XC")) |>
      mutate(data_type = "function",
             group = "carbon cycling",
             unit = "µmol m−2 s−1")
    # Negative NEE values reflect CO2 uptake in the ecosystem, and positive values reflect CO2 release from the ecosystem to the atmosphere -> needs to be converted to + ecosystem uptake and - is release to atmosphere
  ),

  # gpp
  tar_target(
    name = gpp,
    command = cflux |>
      mutate(gpp = -1*gpp) |>
      group_by(year, siteID, blockID, plotID, treatment, data_type, group, unit) |>
      summarise(value = mean(gpp),
                .groups = "drop") |>
      mutate(response = "gpp")
  ),

  # nee
  tar_target(
    name = nee,
    command = cflux |>
      # transform data to not have negative values
      ### IS THIS CORRECT OR SHOULD WE USE SCALE? !!!!
      mutate(nee = nee + 15) |>
      group_by(year, siteID, blockID, plotID, treatment, data_type, group, unit) |>
      summarise(value = mean(nee),
                .groups = "drop") |>
      mutate(response = "nee")
  ),


  # reco
  tar_target(
    name = reco,
    command = cflux |>
      group_by(year, siteID, blockID, plotID, treatment, data_type, group, unit) |>
      summarise(value = mean(Reco),
                .groups = "drop") |>
      mutate(response = "Reco")
  )

)
