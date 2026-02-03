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

  # root biomass (need to change to new root biomass 2021)
  # tar_target(
  #   name = root_biomass_download,
  #   command =  get_file(node = "tx9r2",
  #                       file = "FUNDER_initial_roots_clean_data_2024.csv",
  #                       path = here::here("data"),
  #                       remote_path = "1_Vegetation/Clean_data"),
  #   format = "file"
  # ),

  # # root productivity and traits (need to change to newly cleaned root trait data)
  # tar_target(
  #   name = root_productivity_download,
  #   command =  get_file(node = "tx9r2",
  #                       file = "FUNDER_clean_root_productivity_trait_2024.csv",
  #                       path = here::here("data"),
  #                       remote_path = "1_Vegetation/Clean_data"),
  #   format = "file"
  # ),

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
      file = "FUNDER_clean_microarthropod_composition_2022.csv",
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
      file = "FUNDER_clean_nematode_composition_2023.csv",
      path = here::here("data"),
      remote_path = "2_Micro_and_Mesofauna/Clean_data"
    ),
    format = "file"
  ),

  # carbon and nutrient stocks and dynamics
  # decomposition
  tar_target(
    name = decomposition_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNDER_clean_plant_litter_decomposition.csv",
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

  # root biomass 21
  tar_target(
    name = root_biomass_raw,
    command = read_csv(here::here("data", "FUNDER_clean_root_biomass_2021.csv"))
  ),

  # root productivity and traits
  tar_target(
    name = root_traits_raw,
    command = read_csv(here::here("data", "FUNDER_clean_root_traits_2022.csv"))
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
    command = read_csv(microarthropod_download) |>
      select(everything())
  ),
  tar_target(
    name = nematode_raw,
    command = read_csv(nematode_download) |>
      select(everything())
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
  # available nutrients
  tar_target(
    name = available_nutrients_raw,
    command = read_csv(available_nutrients_download)
  ),

  # cflux
  tar_target(
    name = cflux_raw,
    command = read_csv(cflux_download)
  )
)
