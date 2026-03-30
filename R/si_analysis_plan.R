# Supporting information analysis plan
si_analysis_plan <- list(
  tar_target(
    name = si_fg_biomass_model_selection,
    command = run_fg_biomass_model_selection(
      dat = big_data,
      response_col = "value_std",
      predictors = c("biomass_scaled_F", "biomass_scaled_G", "biomass_scaled_B")
    )
  )
)
