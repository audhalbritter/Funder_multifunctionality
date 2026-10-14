# Local import while the clean files are off OSF.
# Reads data/clean_data. Three files are still read from data/ because they
# are not in clean_data: FunCaB carbon fluxes 2015-2017, fungal functional
# groups, and the nematode c-p table.

read_clean <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

import_file <- function(target_name, path) {
  file_target <- paste0(target_name, "_file")
  list(
    targets::tar_target_raw(
      name = file_target,
      command = parse(text = deparse1(path), keep.source = FALSE),
      format = "file"
    ),
    targets::tar_target_raw(
      name = target_name,
      command = parse(
        text = paste0("read_clean(", file_target, ")"),
        keep.source = FALSE
      )
    )
  )
}

import_csv <- function(target_name, file) {
  import_file(target_name, here::here("data", "clean_data", file))
}

import_local <- function(target_name, file) {
  import_file(target_name, here::here("data", file))
}

import_plan <- list(
  # biomass removed 2015-2022 (replaces FunCaB_clean_biomass_2015-2021.csv)
  import_csv("biomass_raw", "i_FUNDER_clean_biomass_removal_2015-2022.csv"),
  import_csv("standing_biomass_raw", "i_FUNDER_clean_standing_biomass_2022.csv"),
  import_csv("community_raw", "iv_FUNDER_clean_community_2015-2022.csv"),
  import_csv("bryophyte_raw", "vi_FUNDER_clean_bryophyte_cover_2022.csv"),
  import_csv("nematode_raw", "x_FUNDER_clean_nematode_community_2022.csv"),

  # c-p scores are a lookup table, not part of the clean_data deposit
  import_local("nematode_cp_raw", "FUNDER_nematode_feeding_group_nemaplex_2022.csv"),

  import_csv(
    "microbial_raw",
    "xi_FUNDER_clean_soil_microbial_abundance_2022.csv"
  ),

  # functional-group table is not in clean_data (the RDS files are phyloseq objects)
  import_local("fungal_fg_raw", "FUNDER_functional_fungi_2022.csv"),

  import_csv(
    "necromass_raw",
    "xxi_FUNDER_clean_fungal_necromass_decomposition_2022.csv"
  ),
  import_csv(
    "decomposition_raw",
    "xx_FUNDER_clean_plant_litter_decomposition_2022.csv"
  ),
  import_csv("cn_raw", "xvi_FUNDER_clean_soil_CN_2022.csv"),
  import_csv(
    "macronutrients_raw",
    "xix_FUNDER_clean_soil_ph_macronutrients_2022.csv"
  ),
  import_csv(
    "available_nutrients_raw",
    "xviii_FUNDER_clean_available_nutrients_2021.csv"
  ),

  # 2015-2017 fluxes are not in clean_data
  import_local("cflux_raw", "FunCaB_clean_Cflux_2015-2017.csv"),
  import_csv("cflux_22_raw", "xxii_FUNDER_clean_cfluxes_2022.csv"),
  import_csv("microclimate_raw", "xxiii_FUNDER_clean_microclimate_2022.csv")
)
