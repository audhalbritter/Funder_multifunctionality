# run models

model_plan <- list(
  # run treatment model for each response
  tar_target(
    name = model_single_function,
    command = make_model(
      dat = big_data |>
        tidylog::filter(!treatment %in% c("F", "G", "B")),
      response_var = value_std,
      fg_var = treatment,
      group = "response"
    )
  ),
  # Model 1: no biomass (treatment × climate only)
  tar_target(
    name = model_single_function_out,
    command = model_output_gt(model_single_function, unnest_col = "result")
  ),
  # Model 2: model 1 + total_biomass_scaled (z-scored in standing_biomass_sum)
  tar_target(
    name = model_single_function_biomass_out,
    command = model_output_gt(model_single_function, unnest_col = "result_biomass")
  ),
  # Model 3: biomass_G/F/B × climate only (biomass z-scored on big_data)
  tar_target(
    name = model_single_function_FG_biomass_out,
    command = model_output_gt(model_single_function, unnest_col = "result_FG_biomass")
  ),
  # Heatmaps: one per model (same group_lookup; unnest different result_*)
  tar_target(
    name = model_single_function_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function |>
        unnest(result) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      plot_model_heatmap(heatmap_data, group_lookup)
    }
  ),
  tar_target(
    name = model_single_function_biomass_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function |>
        unnest(result_biomass) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      plot_model_heatmap(heatmap_data, group_lookup)
    }
  ),
  tar_target(
    name = model_single_function_FG_biomass_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function |>
        unnest(result_FG_biomass) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      plot_model_heatmap(heatmap_data, group_lookup)
    }
  ),

  # Group-level models (same structure as single response)
  tar_target(
    name = model_group,
    command = make_model(
      dat = absolute_group_data |> filter(!treatment %in% c("F", "G", "B")),
      response_var = value,
      fg_var = treatment,
      group = "group"
    )
  ),
  tar_target(
    name = model_group_out,
    command = model_output_gt(
      model_group |> rename(group_name = group),
      unnest_col = "result"
    )
  ),
  tar_target(
    name = model_group_biomass_out,
    command = model_output_gt(
      model_group |> rename(group_name = group),
      unnest_col = "result_biomass"
    )
  ),
  tar_target(
    name = model_group_FG_biomass_out,
    command = model_output_gt(
      model_group |> rename(group_name = group),
      unnest_col = "result_FG_biomass"
    )
  ),

  # Global multifunctionality models
  tar_target(
    name = model_multifunctionality,
    command = make_model(
      dat = multifunctionality |> filter(!treatment %in% c("F", "G", "B")),
      response_var = multifuntionality,
      fg_var = treatment,
      group = "level"
    )
  ),
  tar_target(
    name = model_multifunctionality_out,
    command = model_output_gt(
      model_multifunctionality |> rename(level_name = level),
      unnest_col = "result",
      extra_drop = "level_name"
    )
  ),
  tar_target(
    name = model_multifunctionality_biomass_out,
    command = model_output_gt(
      model_multifunctionality |> rename(level_name = level),
      unnest_col = "result_biomass",
      extra_drop = "level_name"
    )
  ),
  tar_target(
    name = model_multifunctionality_FG_biomass_out,
    command = model_output_gt(
      model_multifunctionality |> rename(level_name = level),
      unnest_col = "result_FG_biomass",
      extra_drop = "level_name"
    )
  ),

  # model for functional group richness
  tar_target(
    name = model_multifunctionality_richness,
    command = make_model(
      dat = multifunctionality |> filter(!treatment %in% c("F", "G", "B")),
      response_var = multifuntionality,
      fg_var = fg_richness,
      group = "level"
    )
  ),
  tar_target(
    name = model_multifunctionality_richness_out,
    command = model_output_gt(
      model_multifunctionality_richness |> rename(level_name = level),
      unnest_col = "result",
      extra_drop = "level_name",
      functional_group_as = "richness"
    )
  ),
  tar_target(
    name = model_multifunctionality_richness_biomass_out,
    command = model_output_gt(
      model_multifunctionality_richness |> rename(level_name = level),
      unnest_col = "result_biomass",
      extra_drop = "level_name",
      functional_group_as = "richness"
    )
  )
)
