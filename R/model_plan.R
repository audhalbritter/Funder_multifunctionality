# run models

model_plan <- list(
  ### run treatment model for each response
  tar_target(
    name = model_single_function,
    command = make_model(
      dat = big_data,
      response_var = value_std,
      fg_var = fg_remaining,
      group = "response"
    )
  ),
  # Model 1: factorial model without biomass (treatment × climate only)
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

  # post hoc test for model 3
  # tests if slopes of FG are different
  # comparison at average climate (0 scaled T and P)
  # multiple testing correction (Holm)
  tar_target(
    name = model_single_function_FG_biomass_slope_tests_out,
    command = {
      model_single_function |>
        select(response, model_FG_biomass) |>
        mutate(
          slope_tests = purrr::map(
            model_FG_biomass,
            ~ compare_fg_biomass_slopes(
              fit = .x,
              temp_value = 0,
              precip_value = 0,
              adjust_method = "holm"
            )
          )
        ) |>
        select(response, slope_tests) |>
        unnest(slope_tests) |>
        select(response, contrast, estimate_diff, std_error, z_value, p_value, p_value_adjusted) |>
        arrange(response, contrast) |>
        mutate(
          response = as.character(response),
          contrast = as.character(contrast)
        ) |>
        gt(groupname_col = "response") |>
        cols_label(
          contrast = "Slope contrast",
          estimate_diff = "Difference",
          std_error = "SE",
          z_value = "z",
          p_value = "p",
          p_value_adjusted = "Holm p"
        ) |>
        fmt_number(
          columns = c(estimate_diff, std_error, z_value, p_value, p_value_adjusted),
          decimals = 3
        ) |>
        tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(
            columns = p_value,
            rows = !is.na(p_value) & p_value <= 0.05
          )
        ) |>
        tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(
            columns = p_value_adjusted,
            rows = !is.na(p_value_adjusted) & p_value_adjusted <= 0.05
          )
        ) |>
        tab_header(
          title = "Pairwise post hoc slope tests for FG biomass (Model 3)",
          subtitle = "Contrasts tested at scaled temperature = 0 and scaled precipitation = 0"
        )
    }
  ),

  ### Main-effects models (FG/biomass + climate mains + T:P only)
  tar_target(
    name = model_single_function_main,
    command = make_main_effects_model(
      dat = big_data,
      response_var = value_std,
      fg_var = fg_remaining,
      group = "response"
    )
  ),

  # AIC Comparison Table for all single-function models
  tar_target(
    name = model_aic_comparison,
    command = {
      group_lookup <- big_data |> distinct(response, group)

      # AIC from interaction models
      aic_int <- model_single_function |>
        select(response, aic, aic_biomass, aic_FG_biomass) |>
        rename(`int climate` = aic, `int bio` = aic_biomass, `int FG bio` = aic_FG_biomass)

      # AIC from main-effects models
      aic_main <- model_single_function_main |>
        select(response, aic_main, aic_main_biomass, aic_main_FG_biomass) |>
        rename(`main climate` = aic_main, `main bio` = aic_main_biomass, `main FG bio` = aic_main_FG_biomass)

      group_order <- c("microenvironment", "nutrient cycling", "carbon cycling", "nematodes", "microbes")

      full_join(aic_int, aic_main, by = "response") |>
        left_join(group_lookup, by = "response") |>
        # Explicit group order matching heatmap
        mutate(group = factor(group, levels = group_order)) |>
        tidyr::pivot_longer(c(-response, -group), names_to = "model", values_to = "aic") |>
        group_by(response) |>
        mutate(ΔAIC = aic - min(aic, na.rm = TRUE)) |>
        arrange(group, response, ΔAIC) |>
        rename_response_pretty() |>
        mutate(group = stringr::str_to_sentence(as.character(group)))
    }
  ),
  tar_target(
    name = model_aic_comparison_out,
    command = model_aic_comparison |>
      filter(ΔAIC <= 2) |>
      gt(groupname_col = "group") |> # Group by the category (Nematodes, C cycling, etc.)
      fmt_number(columns = c(aic, ΔAIC), decimals = 2) |>
      tab_header(title = "Model Comparison by AIC (ΔAIC ≤ 2)") |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = ΔAIC == 0)
      ) |>
      cols_label(response = "Variable", model = "Model", aic = "AIC")
  ),


  ### Global multifunctionality models
  tar_target(
    name = model_multifunctionality,
    command = make_model(
      dat = multifunctionality,
      response_var = multifuntionality,
      fg_var = fg_remaining,
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
  )
)
