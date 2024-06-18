#download_data
download_plan <- list(

  # plant data
  # biomass
  tar_target(
    name = biomass_download2,
    command =  get_file(node = "4c5v2",
                        file = "FunCaB_clean_biomass_2015-2021.csv",
                        path = "data",
                        remote_path = "1_Biomass_removal"),
  format = "file"
  ),

  tar_target(
    name = biomass_download,
    command =  get_file(node = "tx9r2",
                        file = "FUNDER_clean_biomass_2022.csv",
                        path = "data",
                        remote_path = "1_Vegetation/Clean_data"),
    format = "file"
  ),

  # root productivity and traits
  tar_target(
    name = root_productivity_download,
    command =  get_file(node = "tx9r2",
                        file = "FUNDER_clean_root_productivity_trait_2024.csv",
                        path = "data",
                        remote_path = "1_Vegetation/Clean_data"),
    format = "file"
  ),

  # community composition
  tar_target(
    name = community_download,
    command =  get_file(node = "4c5v2",
                        file = "FunCaB_clean_composition_2015-2019.csv",
                        path = "data",
                        remote_path = "3_Plant_composition"),
    format = "file"
  ),

  # mesofauna
  tar_target(
    name = microarthropod_download,
    command = get_file(node = "tx9r2",
                        file = "FUNDER_clean_microarthropod_composition_2022.csv",
                        path = "data",
                        remote_path = "2_Micro_and_Mesofauna/Clean_data"),
    format = "file"
  ),

  # nematodes
  tar_target(
    name = nematode_download,
    command = get_file(node = "tx9r2",
                        file = "FUNDER_clean_nematode_composition_2022.csv",
                        path = "data",
                        remote_path = "2_Micro_and_Mesofauna/Clean_data"),
    format = "file"
  ),

  # carbon and nutrient stocks and dynamics
  # decomposition
  tar_target(
    name = decomposition_download,
    command = get_file(node = "tx9r2",
                        file = "FUNDER_clean_plant_litter_decomposition.csv",
                        path = "data",
                        remote_path = "5_Carbon_and_nutrient_cycle/Clean_data"),
    format = "file"
  ),

  # loi
  tar_target(
    name = loi_download,
    command =  get_file(node = "tx9r2",
                        file = "FUNDER_clean_LOI_2022.csv",
                        path = "data",
                        remote_path = "5_Carbon_and_nutrient_cycle/Clean_data"),
    format = "file"
  ),

  # available nutrients
  tar_target(
    name = available_nutrients_download,
    command =  get_file(node = "tx9r2",
                        file = "FUNDER_clean_available_nutrients_2021.csv",
                        path = "data",
                        remote_path = "5_Carbon_and_nutrient_cycle/Clean_data"),
    format = "file"
  ),

  tar_target(
    name = cflux_download,
    command =  get_file(node = "4c5v2",
                        file = "FunCaB_clean_Cflux_2015-2017.csv",
                        path = "data",
                        remote_path = "5_Carbon_fluxes"),
    format = "file"
  ),


  # import data
  # biomass
  tar_target(
    name = biomass_raw2,
    command =  read_csv(biomass_download2)
  ),

  # 2022 biomass
  tar_target(
    name = biomass_raw,
    command =  read_csv(biomass_download)
  ),

  # root productivity and traits
  tar_target(
    name = root_productivity_raw,
    command =  read_csv(root_productivity_download)
  ),

  # community composition
  tar_target(
    name = community_raw,
    command =  read_csv(community_download)
  ),

  # mesofauna
  tar_target(
    name = microarthropod_raw,
    command =  read_csv(microarthropod_download) |>
      select(everything())
  ),

  tar_target(
    name = nematode_raw,
    command =  read_csv(nematode_download) |>
      select(everything())
  ),

  # carbon and nutrient stocks and dynamics
  # loi
  tar_target(
    name = decomposition_raw,
    command =  read_csv(decomposition_download)
  ),

  # loi
  tar_target(
    name = loi_raw,
    command =  read_csv(loi_download)
  ),
  # available nutrients
  tar_target(
    name = available_nutrients_raw,
    command =  read_csv(available_nutrients_download)
  ),

  # cflux
  tar_target(
    name = cflux_raw,
    command =  read_csv(cflux_download)
  )

)

