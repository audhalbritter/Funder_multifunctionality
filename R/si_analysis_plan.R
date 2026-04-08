# Supporting information analysis plan
si_analysis_plan <- list(
  tar_target(
    name = si_fg_biomass_model_selection,
    command = run_fg_biomass_model_selection(
      dat = big_data,
      response_col = "value_std",
      predictors = c("biomass_scaled_F", "biomass_scaled_G", "biomass_scaled_B")
    )
  ),
  tar_target(
    name = si_model_plots,
    command = plot_si_fg_biomass(
      dat = big_data, 
      selected_models = si_fg_biomass_model_selection$selected_models,
      temp_colour = temp_colour,
      prec_colour = prec_colour
    )
  )
)
