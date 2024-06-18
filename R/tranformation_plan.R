# data curation plan
transformation_plan <- list(

  #### NEED TO CHECK DECOMPOSITION AND ROOT PRODUCTIVITY PLOTID'S SEEM TO BE WRONG. HAVE NOT HAD FUNCABIZATION FUNCTION.

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
             fg_remaining = factor(fg_remaining, levels = c("None", "G", "F", "B", "GF", "GB", "FB", "All")))

  ),


  # prep biomass
  tar_target(
    name = biomass2,
    command = biomass_raw2 |>
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
    command = biomass_raw |>
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
  tar_target(
    name = root_productivity,
    command = root_productivity_raw |>
      mutate(year = year(retrieval_date),
             duration = retrieval_date - burial_date) |>
      select(year, siteID:plotID, treatment, value = root_productivity, duration) |>
      mutate(data_type = "function",
             group = "primary producers",
             response = "root productivity",
             unit = "g m-3 y-1")
  ),

  # community
  tar_target(
    name = community,
    command = community_raw |>
      # remove extra plots in 2016, select last year
      filter(treatment != "XC",
             year == 2019)

  ),

  # cover
  tar_target(
    name = plant_litter,
    command = community |>
      select(year:treatment, value = litter) |>
      distinct() |>
      # replace NA with 0
      mutate(value = if_else(is.na(value), 0, value)) |>
      mutate(data_type = "function",
             group = "primary producers",
             response = "litter cover",
             unit = "%")

  ),

  # cover
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


  # prep microarthropod
  tar_target(
    name = microarthropod,
    command = microarthropod_raw |>
      select(year, siteID, blockID, plotID, treatment, functional_group, microarthropods, abundance)
    ),

  tar_target(
    name = microarthropod_density,
    command = microarthropod |>
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = sum(abundance)) |>
      mutate(data_type = "function",
             group = "higher trophic level",
             response = "microarthropod density",
             unit = "count")
  ),

  # prep nematodes
  tar_target(
    name = nematode,
    command = nematode_raw |>
      mutate(year = 2022) |>
      select(year, siteID:comment)
  ),


  tar_target(
    name = nematode_density,
    command = nematode |>
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = sum(abundance_per_g*100)) |>
      mutate(data_type = "function",
             group = "higher trophic level",
             response = "nematode density",
             unit = "count per g soil")
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

  # loi
  tar_target(
    name = organic_matter,
    command = loi_raw |>
      mutate(year = 2022) |>
      filter(variable == "organic_matter") |>
      mutate(data_type = "function",
             group = "carbon cycling",
             response = "organic matter",
             unit = "%") |>
      select(-variable)
  ),

  # available nutrients
  tar_target(
    name = available_nutrients,
    command = available_nutrients_raw |>
      mutate(year = 2021) |>
      filter(elements %in% c("NH4-N", "NO3-N", "P", "K", "Mg", "Ca")) |>
      mutate(data_type = "function",
             group = "nutrient cycling",
             response = case_when(elements == "NH4-N" ~ "ammonium",
                                  elements == "NO3-N" ~ "nitrate",
                                  elements == "P" ~ "phosphate",
                                  elements == "Mg" ~ "magnesium",
                                  elements == "K" ~ "potassium",
                                  elements == "Ca" ~ "calcium"),
             unit = "micro grams/10cm2/35 days") |>
      filter(response %in% c("nitrate", "ammonium", "phosphate")) |>
      select(- elements, -burial_length, -detection_limit, -burial_date, -retrieval_date, -notes)
  ),

  # cflux
  tar_target(
    name = gpp,
    command = cflux_raw |>
      filter(year == 2017) |>
      filter(!treatment %in% c("RTC", "XC")) |>
      # Negative NEE values reflect CO2 uptake in the ecosystem, and positive values reflect CO2 release from the ecosystem to the atmosphere -> needs to be converted to + ecosystem uptake and - is release to atmosphere
      #mutate(nee = -1*nee) |>
      mutate(gpp = -1*gpp) |>
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = mean(gpp),
                # can also be added
                #gpp = mean(gpp),
                #Reco = mean(Reco),
                .groups = "drop") |>
      mutate(data_type = "function",
             group = "carbon cycling",
             response = "gpp",
             unit = "µmol m−2 s−1")
  ),

  tar_target(
    name = reco,
    command = cflux_raw |>
      filter(year == 2017) |>
      filter(!treatment %in% c("RTC", "XC")) |>
      # Negative NEE values reflect CO2 uptake in the ecosystem, and positive values reflect CO2 release from the ecosystem to the atmosphere -> needs to be converted to + ecosystem uptake and - is release to atmosphere
      group_by(year, siteID, blockID, plotID, treatment) |>
      summarise(value = mean(Reco),
                # can also be added
                #gpp = mean(gpp),
                #Reco = mean(Reco),
                .groups = "drop") |>
      mutate(data_type = "function",
             group = "carbon cycling",
             response = "Reco",
             unit = "µmol m−2 s−1")
  )

)
