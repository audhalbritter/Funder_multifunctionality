# download_data
download_plan <- list(
  # plant data
  # biomass 2015 - 2021
  tar_target(
    name = biomass_download,
    command = get_file(
      node = "4c5v2",
      file = "FunCaB_clean_biomass_2015-2021.csv",
      path = here::here("data"),
      remote_path = "1_Biomass_removal"
    ),
    format = "file"
  ),

  # biomass 2022
  tar_target(
    name = biomass_22_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_biomass_2022.csv",
      path = here::here("data"),
      remote_path = "1_Vegetation/Clean_data"
    ),
    format = "file"
  ),

  # litter
  tar_target(
    name = litter_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_bryophyte_structure_2022.csv",
      path = here::here("data"),
      remote_path = "1_Vegetation/Clean_data"
    ),
    format = "file"
  ),

  # root biomass (need to change to new root biomass 2021)
  # tar_target(
  #   name = root_biomass_download,
  #   command =  get_file(node = "tx9r2",
  #                       file = "FUNDER_initial_roots_clean_data_2024.csv",
  #                       path = here::here("data"),
  #                       remote_path = "1_Vegetation/Clean_data"),
  #   format = "file"
  # ),

  # root traits
  tar_target(
    name = root_traits_download,
    command =  get_file(node = "tx9r2",
                        file = "FUNDER_clean_root_traits_corrected_2022.csv",
                        path = here::here("data"),
                        remote_path = "1_Vegetation/Clean_data"),
    format = "file"
  ),

  # community composition
  tar_target(
    name = community_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_community_2015-2022.csv",
      path = here::here("data"),
      remote_path = "1_Vegetation/Clean_data"
    ),
    format = "file"
  ),

  # bryophyte cover
  tar_target(
    name = bryophyte_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_bryophyte_cover_2022.csv",
      path = here::here("data"),
      remote_path = "1_Vegetation/Clean_data"
    ),
    format = "file"
  ),

  # mesofauna
  tar_target(
    name = microarthropod_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_microarthropods_2022.csv",
      path = here::here("data"),
      remote_path = "2_Micro_and_Mesofauna/Clean_data"
    ),
    format = "file"
  ),

  # nematodes
  tar_target(
    name = nematode_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_nematode_communities_2022.csv",
      path = here::here("data"),
      remote_path = "2_Micro_and_Mesofauna/Clean_data"
    ),
    format = "file"
  ),

  # microbial density
  tar_target(
    name = microbial_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_soil_microbial_abundance.csv",
      path = here::here("data"),
      remote_path = "3_Fungi_microbes/Clean_data"
    ),
    format = "file"
  ),

  # fungal functional groups
  tar_target(
    name = fungal_fg_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_fungal_functional_groups.csv",
      path = here::here("data"),
      remote_path = "3_Fungi_microbes/Clean_data"
    ),
    format = "file"
  ),

  # fungal necromass
  tar_target(
    name = necromass_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_fungal_necromass_decomposition.csv",
      path = here::here("data"),
      remote_path = "5_Carbon_and_nutrient_cycle/Clean_data"
    ),
    format = "file"
  ),

  # carbon and nutrient stocks and dynamics
  # decomposition
  tar_target(
    name = decomposition_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_litter_decomposition.csv",
      path = here::here("data"),
      remote_path = "5_Carbon_and_nutrient_cycle/Clean_data"
    ),
    format = "file"
  ),

  # soil carbon and nitrogen
  tar_target(
    name = cn_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_soil_CN_2022.csv",
      path = here::here("data"),
      remote_path = "5_Carbon_and_nutrient_cycle/Clean_data"
    ),
    format = "file"
  ),

  # soil organic matter
  tar_target(
    name = som_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_LOI_2022.csv",
      path = here::here("data"),
      remote_path = "5_Carbon_and_nutrient_cycle/Clean_data"
    ),
    format = "file"
  ),

  # Nutrient cycling
  tar_target(
    name = macronutrients_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_macronutrients_and_ph_2022.csv",
      path = here::here("data"),
      remote_path = "5_Carbon_and_nutrient_cycle/Clean_data"
    ),
    format = "file"
  ),

  # available nutrients
  tar_target(
    name = available_nutrients_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_available_nutrients_2021.csv",
      path = here::here("data"),
      remote_path = "5_Carbon_and_nutrient_cycle/Clean_data"
    ),
    format = "file"
  ),
  tar_target(
    name = cflux_download,
    command = get_file(
      node = "4c5v2",
      file = "FunCaB_clean_Cflux_2015-2017.csv",
      path = here::here("data"),
      remote_path = "5_Carbon_fluxes"
    ),
    format = "file"
  ),

  # microclimate
  tar_target(
    name = microclimate_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_microclimate_2022.csv",
      path = here::here("data"),
      remote_path = "6_Environment/Clean_data"
    ),
    format = "file"
  ),


  # import data
  # biomass
  tar_target(
    name = biomass_raw,
    command = read_csv(biomass_download)
  ),

  # 2022 biomass
  tar_target(
    name = biomass_22_raw,
    command = read_csv(biomass_22_download)
  ),

  # litter from bryophyte plots
  tar_target(
    name = litter_raw,
    command = read_csv(litter_download)
  ),

  # litter from all plots
  tar_target(
    name = litter_raw_2,
    command = read_csv(here::here("data", "FUNDER_raw_vascular_community_2022.csv"))
  ),

  # root biomass 21
  tar_target(
    name = root_biomass_raw,
    command = read_csv(here::here("data", "FUNDER_clean_root_biomass_2021.csv"))
  ),

  # root productivity and traits
  tar_target(
    name = root_traits_raw,
    command = read_csv(root_traits_download)
  ),

  # community composition
  tar_target(
    name = community_raw,
    command = read_csv(community_download)
  ),

  # bryophyet cover
  tar_target(
    name = bryophyte_raw,
    command = read_csv(bryophyte_download)
  ),

  # mesofauna
  tar_target(
    name = microarthropod_raw,
    command = read_csv(microarthropod_download)
  ),
  tar_target(
    name = nematode_raw,
    command = read_csv(nematode_download)
  ),
  tar_target(
    name = nematode_cp_raw,
    command = read_csv("data/FUNDER_nematode_feeding_group_nemaplex_2022.csv")
  ),

  tar_target(
    name = microbial_raw,
    command = read_csv(microbial_download)
  ),
  tar_target(
    name = fungal_fg_raw,
    command = read_csv(fungal_fg_download)
  ),
  # fungal necromass
  tar_target(
    name = necromass_raw,
    command = read_csv(necromass_download)
  ),

  # carbon and nutrient stocks and dynamics
  # loi
  tar_target(
    name = decomposition_raw,
    command = read_csv(decomposition_download)
  ),

  # cabron and nitrogen
  tar_target(
    name = cn_raw,
    command = read_csv(cn_download)
  ),
  tar_target(
    name = som_raw,
    command = read_csv(som_download)
  ),

  # macronutrients and ph
  tar_target(
    name = macronutrients_raw,
    command = read_csv(macronutrients_download)
  ),

  # available nutrients
  tar_target(
    name = available_nutrients_raw,
    command = read_csv(available_nutrients_download)
  ),

  # cflux
  tar_target(
    name = cflux_raw,
    command = read_csv(cflux_download)
  ),
  # microclimate
  tar_target(
    name = microclimate_raw,
    command = read_csv(microclimate_download)
  )
)
