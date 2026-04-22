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
        mutate(delta_AIC = aic - min(aic, na.rm = TRUE)) |>
        arrange(group, response, delta_AIC) |>
        rename_response_pretty() |>
        mutate(group = stringr::str_to_sentence(as.character(group)))
    }
  ),
  tar_target(
    name = model_aic_comparison_out,
    command = model_aic_comparison |>
      filter(delta_AIC <= 2) |>
      gt(groupname_col = "group") |> # Group by the category (Nematodes, C cycling, etc.)
      fmt_number(columns = c(aic, delta_AIC), decimals = 2) |>
      tab_header(title = "Model Comparison by AIC (delta AIC <= 2)") |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = delta_AIC == 0)
      ) |>
      cols_label(response = "Variable", model = "Model", aic = "AIC", delta_AIC = "ΔAIC")
  ),

  ### Model comparison statistics (1:1 line tests)
  tar_target(
    name = model_single_function_comparison_stats,
    command = calculate_model_comparison_stats(
      model_tbl = model_single_function,
      response_group_lookup = big_data |> distinct(response, group)
    )
  ),
  tar_target(
    name = model_single_function_comparison_stats_out,
    command = {
      # Return a combined gt table for the manuscript
      # For estimates
      est_tbl <- model_single_function_comparison_stats$estimates |>
        gt() |>
        fmt_number(columns = c(mean_diff, wilcox_p, slope, p_slope_1), decimals = 3) |>
        tab_header(title = "Fixed-effect Estimates: Deviation from 1:1 line")

      # For R2
      r2_tbl <- model_single_function_comparison_stats$r2 |>
        gt() |>
        fmt_number(columns = c(mean_diff, wilcox_p, slope, p_slope_1), decimals = 3) |>
        tab_header(title = "R²: Deviation from 1:1 line")

      list(estimates = est_tbl, r2 = r2_tbl)
    }
  ),
  tar_target(
    name = model_interaction_vs_main_comparison_stats,
    command = calculate_interaction_vs_main_stats(
      model_int = model_single_function,
      model_main = model_single_function_main,
      response_group_lookup = big_data |> distinct(response, group)
    )
  ),
  tar_target(
    name = model_interaction_vs_main_comparison_stats_out,
    command = {
      # For estimates
      est_tbl <- model_interaction_vs_main_comparison_stats$estimates |>
        gt() |>
        fmt_number(columns = c(mean_diff, wilcox_p, slope, p_slope_1), decimals = 3) |>
        tab_header(title = "Fixed-effect Estimates (Main vs Int): Deviation from 1:1 line")

      # For R2
      r2_tbl <- model_interaction_vs_main_comparison_stats$r2 |>
        gt() |>
        fmt_number(columns = c(mean_diff, wilcox_p, slope, p_slope_1), decimals = 3) |>
        tab_header(title = "R² (Main vs Int): Deviation from 1:1 line")

      list(estimates = est_tbl, r2 = r2_tbl)
    }
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
  ),

  ### Global multifunctionality main effects models
  tar_target(
    name = model_multifunctionality_main,
    command = make_main_effects_model(
      dat = multifunctionality,
      response_var = multifuntionality,
      fg_var = fg_remaining,
      group = "level"
    )
  ),
  tar_target(
    name = model_multifunctionality_main_out,
    command = model_output_gt(
      model_multifunctionality_main |> rename(level_name = level),
      unnest_col = "result_main",
      extra_drop = "level_name"
    )
  ),
  tar_target(
    name = model_multifunctionality_main_biomass_out,
    command = model_output_gt(
      model_multifunctionality_main |> rename(level_name = level),
      unnest_col = "result_main_biomass",
      extra_drop = "level_name"
    )
  ),
  tar_target(
    name = model_multifunctionality_main_FG_biomass_out,
    command = model_output_gt(
      model_multifunctionality_main |> rename(level_name = level),
      unnest_col = "result_main_FG_biomass",
      extra_drop = "level_name"
    )
  ),

  # AIC Comparison Table for multifunctionality models
  tar_target(
    name = model_multifunctionality_aic_comparison,
    command = {
      # Helper to extract aic and r2
      extract_stats <- function(dat, suffix = "") {
        # Define the exact columns we expect to have r2 lists
        r2_list_cols <- intersect(names(dat), c("r2", "r2_biomass", "r2_FG_biomass", "r2_main", "r2_main_biomass", "r2_main_FG_biomass"))

        dat |>
          # Pre-extract R2 values to double to avoid list-double conflicts in pivot_longer
          mutate(
            across(all_of(r2_list_cols), ~ map_dbl(.x, ~ if (is.null(.x)) NA_real_ else .x$R2_marginal), .names = "{.col}_marginal"),
            across(all_of(r2_list_cols), ~ map_dbl(.x, ~ if (is.null(.x)) NA_real_ else .x$R2_conditional), .names = "{.col}_conditional")
          ) |>
          # Clean up names to make them consistent (r2 vs r2_main)
          rename_with(~ gsub("r2_main", "r2", .x), contains("r2_main")) |>
          rename_with(~ gsub("aic_main", "aic", .x), contains("aic_main")) |>
          select(level, starts_with("aic"), contains("marginal"), contains("conditional")) |>
          pivot_longer(c(-level), names_to = "name", values_to = "val") |>
          mutate(
            model_type = case_when(
              grepl("FG_biomass", name) ~ "FG bio",
              grepl("biomass", name) ~ "bio",
              TRUE ~ "climate"
            ),
            metric = case_when(
              grepl("aic", name) ~ "aic",
              grepl("marginal", name) ~ "R2_marginal",
              grepl("conditional", name) ~ "R2_conditional"
            ),
            model = paste(suffix, model_type)
          ) |>
          select(level, model, metric, val) |>
          pivot_wider(names_from = metric, values_from = val)
      }

      dat_int <- extract_stats(model_multifunctionality, "int")
      dat_main <- extract_stats(model_multifunctionality_main, "main")

      bind_rows(dat_int, dat_main) |>
        group_by(level) |>
        mutate(delta_AIC = aic - min(aic, na.rm = TRUE)) |>
        arrange(delta_AIC) |>
        ungroup()
    }
  ),
  tar_target(
    name = model_multifunctionality_aic_comparison_out,
    command = model_multifunctionality_aic_comparison |>
      gt() |>
      fmt_number(columns = c(aic, delta_AIC, R2_marginal, R2_conditional), decimals = 2) |>
      tab_header(title = "Multifunctionality Model Comparison: AIC and R2") |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = delta_AIC == 0)
      ) |>
      cols_label(
        model = "Model",
        aic = "AIC",
        delta_AIC = "ΔAIC",
        R2_marginal = "R² marginal",
        R2_conditional = "R² conditional"
      )
  )
)
