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
        fg_remaining = factor(fg_remaining, levels = c("None", "F", "G", "B", "GF", "FB", "GB", "All"))
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

  # standing biomass (final harvest)
  tar_target(
    name = standing_biomass,
    command = standing_biomass_raw |> 
      # remove lichens
      filter(removed_fg != "L") |> 
      # remove the biomass from FGs that were removed
      #filter(is.na(no_treatment)) |> 
      # for FGB replace biomass with 0
      #tidylog::mutate(biomass = if_else(treatment == "FGB", 0, biomass)) |>
      # convert to g per m2
      mutate(biomass = biomass * 10000 / 625) |>
      # z-score once here; sum/wide use biomass_scaled so scaling is not duplicated
      mutate(biomass_scaled = scale(biomass)[, 1]) |>
      mutate(
        unit = "g m\u207B\u00B2"
      ) |>
      # after filtering, removed_fg only contains FGs that are present; rename for clarity
      rename(fg_id = removed_fg) |>
      select(-comments)
  ),

  # sum of standing biomass
  tar_target(
    name = standing_biomass_sum,
    command = standing_biomass |>
      group_by(siteID, blockID, plotID, treatment) |>
      summarise(
        total_biomass = sum(biomass, na.rm = TRUE),
        total_biomass_scaled = sum(biomass_scaled, na.rm = TRUE),
        .groups = "drop"
      )
  ),

  # wide biomass (missing FG = removed, so biomass 0)
  tar_target(
    name = standing_biomass_wide,
    command = standing_biomass |>
      pivot_wider(
        id_cols = c(siteID, blockID, plotID, treatment),
        names_from = fg_id,
        values_from = biomass_scaled,
        names_prefix = "biomass_scaled_",
        values_fill = 0
      )
  ),

  # plant litter
  tar_target(
    name = plant_litter,
    command = {

      litter_prep <- litter_raw_2 |>
        filter(Measure == "Cover") |>
        select(siteID, blockID, turfID, treatment = Treatment, value = Litter) |>
        mutate(
          treatment = if_else(treatment %in% c("B", "FB", "GF", "FGB", "G", "F", "GB"), treatment, "C"),
          blockID = paste0(substr(siteID, 1, 3), blockID)
        ) |>
        select(siteID, blockID, plotID = turfID, treatment, value) %>%
        dataDocumentation::funcabization(dat = ., convert_to = "FunCaB") |>
        mutate(value = if_else(is.na(value), 0, value))

      meta |>
        select(siteID, blockID, plotID, treatment) |>
        tidylog::left_join(litter_prep, by = c("siteID", "blockID", "plotID", "treatment")) |>
        tidylog::mutate(value = replace_na(value, 0)) |>
        mutate(
          data_type = "stocstanding_biomassk",
          group = "primary producers",
          response = "plant litter",
          unit = "% cover"
        )
    }
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
        data_type = "stock",
        group = "primary producers",
        response = "root biomass",
        unit = "g m\u207B\u00B2"
      ) |>
      select(-ric_radius, -ric_area)
  ),

  # prep nematodes
  tar_target(
    name = nematode,
    command = nematode_raw |>
      tidylog::filter(family != "unknown") |>
      tidylog::mutate(functional_group = if_else(functional_group == "omnivore", "predator", functional_group)) |>
      summarise(
        value = sum(per_family_abundance_per_g_dry_soil),
        .by = c(year, siteID, blockID, plotID, treatment, family, functional_group)
      )
  ),

  tar_target(
    name = nematode_density,
    command = nematode_raw |>
      distinct(year, siteID, blockID, plotID, treatment, total_nematode_abundance_per_g_dry_soil) |>
      rename(value = total_nematode_abundance_per_g_dry_soil) |>
      mutate(
        data_type = "stock",
        group = "nematodes",
        response = "nematode density",
        unit = "count g\u207B\u00B9"
      )
  ),

  tar_target(
    name = nematode_feeding_group_density,
    command = nematode |>
      group_by(year, siteID, blockID, plotID, treatment, response = functional_group) |>
      summarise(value = sum(value)) |>
      mutate(
        data_type = "stock",
        group = "nematodes",
        response = paste0(response, " feeder density"),
        unit = "count g\u207B\u00B9"
      )
  ),

    tar_target(
    name = nematode_fungal_bacterial_feeder_ratio,
    command = nematode |> 
      filter(functional_group %in% c("fungivore", "bacterivore")) |>
      group_by(year, siteID, blockID, plotID, treatment, response = functional_group) |>
      summarise(value = sum(value)) |>
      pivot_wider(id_cols = c(year, siteID, blockID, plotID, treatment), names_from = response, values_from = value) |>
      # bacteria to fungi ratio to make it slow to fast
      mutate(value = bacterivore / (fungivore + bacterivore)) |> 
      select(year, siteID, blockID, plotID, treatment, value) |>
      mutate(
        data_type = "process",
        response = "bacterial_fungal_feeder_ratio",
        group = "nematodes",
        unit = "unitless"
      )
  ),
  
  # cp values for nematodes (fill NA cp_group with literature values)
  tar_target(
    name = nematode_cp,
    command = nematode_cp_raw |>
      mutate(
        cp_group = case_when(
          Family == "Desmodoridae" ~ 3L,
          Family == "Diphterophoridae" ~ 3L,
          Family == "Diplogasteridae" ~ 1L,
          Family == "Dorylaimoidea" ~ 4L,
          Family == "Ecphyadophoridae" ~ 2L,
          Family == "Metateratocephalidae" ~ 3L,
          Family == "Neodiplogasteridae" ~ 1L,
          Family == "Odontolaimidae" ~ 3L,
          Family == "Paratylenchidae" ~ 2L,
          Family == "Psilenchidae" ~ 2L,
          TRUE ~ as.integer(cp_group)
        )
      ) |>
      select(Family, `Functional Group`, cp_group)
  ),

  tar_target(
    name = nematode_indices,
    command = {
      joined <- nematode |>
        tidylog::left_join(
          nematode_cp |>
            mutate(Family = tolower(Family)) |>
            rename(family = Family),
          by = "family"
        ) |>
        tidylog::filter(!is.na(cp_group))

      plant_parasite <- joined |>
        filter(`Functional Group` == "Plant") |>
        summarise(
          value = weighted.mean(as.numeric(cp_group), value),
          .by = c(year, siteID, blockID, plotID, treatment)
        ) |>
        mutate(response = "plant_parasite_index")

      maturity <- joined |>
        filter(`Functional Group` != "Plant") |>
        summarise(
          value = weighted.mean(as.numeric(cp_group), value),
          .by = c(year, siteID, blockID, plotID, treatment)
        ) |>
        mutate(response = "maturity_index")

      bind_rows(plant_parasite, maturity) |>
        # invert value to make it slow to fast
        mutate(value = -1 * value) |>
        mutate(
          data_type = "process",
          group = "nematodes",
          unit = "unitless"
        )
    }
  ),

  # fungal and bacterial density
  tar_target(
    name = microbes,
    command = microbial_raw |> 
      tidylog::mutate(abundance_per_g = if_else(is.na(abundance_per_g), 0, abundance_per_g)) |>
      select(year, siteID, blockID, plotID, treatment, group, value = abundance_per_g)
  ),

    # fungal and bacterial density
  tar_target(
    name = microbial_density,
    command = microbes |> 
      select(year, siteID, blockID, plotID, treatment, response = group, value) |>
      mutate(
        data_type = "stock",
        group = "microbes",
        response = paste0(response, " density"),
        unit = "count g\u207B\u00B9"
      )
  ),

  # microbial fungal:bacterial ratio (fungal density / total microbial density per plot)
  tar_target(
    name = microbial_ratio,
    command = microbes |>
      pivot_wider(
        id_cols = c(year, siteID, blockID, plotID, treatment),
        names_from = group,
        values_from = value
      ) |>
      # bacteria to fungi ratio to make it slow to fast
      mutate(
        value = bacteria / (fungi + bacteria),
        response = "bacteria-fungi ratio"
      ) |> 
      select(year, siteID, blockID, plotID, treatment, response, value) |>
      mutate(
        data_type = "process",
        group = "microbes",
        unit = "unitless"
      )
  ),

  # fungal functional groups
  tar_target(
    name = fungal_fg_density,
    command = fungal_fg_raw |> 
      select(-any_of("...1")) |>
      filter(functional_group != "other") |>
      rename(value = relative_abundance) |> 
      mutate(
        data_type = "process",
        group = "microbes",
        response = functional_group,
        unit = "g m\u207B\u00B2"
      ) |>
      select(-functional_group)
  ),

  # fungal necromass
  tar_target(
    name = fungal_necromass,
    command = necromass_raw |>
      mutate(year = year(retrieval_date))|>
      select(year, siteID, blockID, plotID, treatment, response = mycelium, value = relative_weight_loss) %>%
      dataDocumentation::funcabization(dat = ., convert_to = "FunCaB") |>
      tidylog::filter(!is.na(value)) |>
      mutate(
        data_type = "function",
        group = "carbon cycling",
        response = paste0(response, " fungal necromass decomposition"),
        unit = "g m\u207B\u00B2"
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
        data_type = "process",
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
        data_type = "stock",
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

  # soil organic matter
  tar_target(
    name = som,
    command = som_raw |>
      mutate(year = 2022) |>
      filter(variable == "organic_matter") |>
      select(year, siteID, blockID, plotID, treatment, value) |> 
      mutate(
        data_type = "stock",
        response = "soil organic matter",
        group = "carbon cycling",
        unit = "%"
      )
  ),

  # cflux block
  # prep data and average replicates within year × site × block × plot × treatment
  tar_target(
    name = cflux,
    command = {
      # 2017 FunCaB fluxes (may have repeated measurements per plot)
      cflux_2017 <- cflux_raw |> 
        filter(year == 2017, !treatment %in% c("RTC", "XC")) |>
        # remove plots that were cut wrongly or lost after 2017
        filter(!plotID %in% c("Fau2GB", "Ulv1B", "Skj4C")) |>
        group_by(year, siteID, blockID, plotID, treatment) |>
        summarise(
          nee  = mean(nee,  na.rm = TRUE),
          gpp  = mean(gpp,  na.rm = TRUE),
          Reco = mean(Reco, na.rm = TRUE),
          .groups = "drop"
        )

      # 2022 FUNDER fluxes: wide format, then average over replicate per plot
      cflux_2022 <- cflux_22_raw |> 
        tidylog::filter(plotID != "Skj4C") |>
        pivot_wider(
          id_cols = c(siteID, blockID, plotID, treatment, replicate),
          names_from = type,
          values_from = f_flux
        ) |>
        rename(nee = NEE, gpp = GPP) |>
        mutate(year = 2022) |>
        group_by(year, siteID, blockID, plotID, treatment) |>
        summarise(
          nee  = mean(nee,  na.rm = TRUE),
          gpp  = mean(gpp,  na.rm = TRUE),
          Reco = mean(Reco, na.rm = TRUE),
          .groups = "drop"
        ) |> filter(!is.na(gpp))

      bind_rows(cflux_2017, cflux_2022) |>
        group_by(siteID, blockID, plotID, treatment) |>
        summarise(
          nee  = mean(nee,  na.rm = TRUE),
          gpp  = mean(gpp,  na.rm = TRUE),
          Reco = mean(Reco, na.rm = TRUE),
          .groups = "drop"
        ) |> filter(!is.na(gpp)) |>
        mutate(
          data_type = "function",
          group     = "carbon cycling",
          unit      = "\u00B5mol m\u207B\u00B2 s\u207B\u00B9"
        )
    }
    # Negative NEE values reflect CO2 uptake in the ecosystem, and positive values reflect CO2 release from the ecosystem to the atmosphere -> needs to be converted to + ecosystem uptake and - is release to atmosphere
  ),

  # gpp (rename target to avoid name clash with gpp column in cflux)
  tar_target(
    name = gpp_flux,
    command = cflux |>
      mutate(gpp = -1 * gpp) |>
      group_by(siteID, blockID, plotID, treatment, data_type, group, unit) |>
      summarise(
        value = mean(gpp),
        .groups = "drop"
      ) |>
      mutate(response = "gross primary productivity")
  ),

  # nee
  tar_target(
    name = nee_flux,
    command = cflux |>
      # transform data to not have negative values
      #### ADD MIM VALUE OR SOMETHING
      mutate(nee = nee + 15) |>
      group_by(siteID, blockID, plotID, treatment, data_type, group, unit) |>
      summarise(
        value = mean(nee),
        .groups = "drop"
      ) |>
      mutate(response = "net ecosystem exchange")
  ),

  # reco
  tar_target(
    name = reco_flux,
    command = cflux |>
      group_by(siteID, blockID, plotID, treatment, data_type, group, unit) |>
      summarise(
        value = mean(Reco),
        .groups = "drop"
      ) |>
      mutate(response = "ecosystem respiration")
  ),

  ## nutrient cycling
  # macronutrients
  tar_target(
    name = phosphorus_stock,
    command = macronutrients_raw |>
      filter(name == "phosphorous") |>
      # convert from mg per 100g dry soil to g per g dry soil
      mutate(value = value / 100) |>
      select(year, siteID, blockID, plotID, treatment, value) |>
      mutate(
        data_type = "stock",
        response = "phosphorus stock",
        group = "nutrient cycling",
        unit = "g g\u207B\u00B9"
      )
  ),

  # available nutrients
  # make ordination for other available nutrients
  tar_target(
    name = micronutrients_pca,
    command = make_nutrient_pca(
      available_nutrients_raw |>
        filter(!elements %in% c("NH4-N", "NO3-N", "P")),
      meta
    )
  ),

  # make ordination for other available nutrients
  tar_target(
    name = available_nutrients,
    command = bind_rows(
      available_nitrogen = available_nutrients_raw |>
        filter(elements %in% c("NH4-N", "NO3-N")) |>
        group_by(siteID, blockID, plotID, treatment) |>
        summarise(value = sum(value)),
      available_phosphorus = available_nutrients_raw |>
        filter(elements %in% c("P")) |>
        select(siteID, blockID, plotID, treatment, value),
      avialable_micronutrients = micronutrients_pca[[1]] |>
      select(siteID, blockID, plotID, treatment, value = PC1),
      .id = "response"
    ) |>
      mutate(
        year = 2021,
        data_type = "process",
        group = "nutrient cycling",
        unit = case_when(
          response == "available_nitrogen" ~ "µg m\u207B\u00B2",
          response == "available_phosphorus" ~ "µg m\u207B\u00B2",
          response == "avialable_micronutrients" ~ "PCA axis 1"
        )
      ) |>
      select(year,siteID:treatment, response, value, data_type:unit)
  ),

  # microclimate: average min/max ground temperature, daily_temp_amplitude (top 25%),
  # min_soil_moisture (bottom 25%), mean soil moisture
  tar_target(
    name = microclimate,
    command = process_microclimate(microclimate_raw) |>
      mutate(data_type = "process", group = "microenvironment")
  )
)
