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
  tar_target(
    name = model_single_function_r2_plot,
    command = {
      # Extract scalar marginal / conditional R2 for each response and model variant
      r2_safe_get <- function(tbl, col) {
        if (is.null(tbl) || !col %in% names(tbl)) {
          return(NA_real_)
        }
        as.numeric(tbl[[col]][1])
      }

      r2_dat <- model_single_function |>
        mutate(
          R2m_no   = purrr::map_dbl(r2, ~ r2_safe_get(.x, "R2_marginal")),
          R2c_no   = purrr::map_dbl(r2, ~ r2_safe_get(.x, "R2_conditional")),
          R2m_bio  = purrr::map_dbl(r2_biomass, ~ r2_safe_get(.x, "R2_marginal")),
          R2c_bio  = purrr::map_dbl(r2_biomass, ~ r2_safe_get(.x, "R2_conditional")),
          R2m_fg   = purrr::map_dbl(r2_FG_biomass, ~ r2_safe_get(.x, "R2_marginal")),
          R2c_fg   = purrr::map_dbl(r2_FG_biomass, ~ r2_safe_get(.x, "R2_conditional"))
        )

      comp_bio <- r2_dat |>
        transmute(
          response,
          type = "marginal",
          comparison = "No biomass vs + total biomass",
          R2_no = R2m_no,
          R2_other = R2m_bio
        ) |>
        bind_rows(
          r2_dat |>
            transmute(
              response,
              type = "conditional",
              comparison = "No biomass vs + total biomass",
              R2_no = R2c_no,
              R2_other = R2c_bio
            )
        )

      comp_fg <- r2_dat |>
        transmute(
          response,
          type = "marginal",
          comparison = "No biomass vs FG biomass",
          R2_no = R2m_no,
          R2_other = R2m_fg
        ) |>
        bind_rows(
          r2_dat |>
            transmute(
              response,
              type = "conditional",
              comparison = "No biomass vs FG biomass",
              R2_no = R2c_no,
              R2_other = R2c_fg
            )
        )

      plot_dat <- bind_rows(comp_bio, comp_fg) |>
        filter(!is.na(R2_no), !is.na(R2_other))

      ggplot(plot_dat, aes(x = R2_no, y = R2_other, colour = type)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
        geom_point(alpha = 0.7) +
        coord_equal() +
        facet_wrap(~comparison) +
        scale_colour_manual(
          values = c(marginal = "#1b9e77", conditional = "#d95f02"),
          name = "R² type",
          labels = c("Marginal", "Conditional")
        ) +
        labs(
          x = "R² (no biomass model)",
          y = "R² (biomass model)",
          title = "R² comparison for single-function models"
        ) +
        theme_bw()
    }
  ),
  tar_target(
    name = model_single_function_estimate_plot,
    command = plot_single_function_estimate_comparison(
      model_single_function_tbl = model_single_function,
      response_group_lookup = big_data |> distinct(response, group),
      colour_by = "group"
    )
  ),
  tar_target(
    name = model_single_function_estimate_term_plot,
    command = plot_single_function_estimate_comparison(
      model_single_function_tbl = model_single_function,
      colour_by = "term"
    )
  ),
  tar_target(
    name = single_function_fg_biomass_panel_plot,
    command = plot_response_vs_fg_biomass(
      data = big_data,
      response_name = "drought_index",
      model_fg_biomass_tbl = model_single_function |> select(response, model_FG_biomass),
      temp_value = 0,
      precip_value = 0
    )
  ),

  ### Group-level models (same structure as single response)
  tar_target(
    name = model_group,
    command = make_model(
      dat = absolute_group_data,
      response_var = value,
      fg_var = fg_remaining,
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
