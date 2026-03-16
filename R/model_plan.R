# run models

model_plan <- list(
  ### run treatment model for each response
  tar_target(
    name = model_single_function,
    command = make_model(
      dat = big_data |>
        tidylog::filter(!fg_remaining %in% c("GF", "FB", "GB")),
      response_var = value_std,
      fg_var = fg_remaining,
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
      r2_tbl <- model_single_function |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
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
      r2_tbl <- model_single_function |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2_biomass,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
    }
  ),

  ### Main-effects models (FG/biomass + climate mains + T:P only)
  tar_target(
    name = model_single_function_main,
    command = make_main_effects_model(
      dat = big_data |>
        tidylog::filter(!fg_remaining %in% c("GF", "FB", "GB")),
      response_var = value_std,
      fg_var = fg_remaining,
      group = "response"
    )
  ),
  tar_target(
    name = model_single_function_main_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function_main |>
        unnest(result_main) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      r2_tbl <- model_single_function_main |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2_main,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
    }
  ),
  tar_target(
    name = model_single_function_main_biomass_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function_main |>
        unnest(result_main_biomass) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      r2_tbl <- model_single_function_main |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2_main_biomass,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
    }
  ),
  tar_target(
    name = model_single_function_main_FG_biomass_heatmap,
    command = {
      group_lookup <- big_data |> distinct(response, group)
      heatmap_data <- model_single_function_main |>
        unnest(result_main_FG_biomass) |>
        filter(effect == "fixed") |>
        select(response, term, estimate, p.value)
      r2_tbl <- model_single_function_main |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2_main_FG_biomass,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
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
      r2_tbl <- model_single_function |>
        mutate(
          R2_marginal = purrr::map_dbl(
            r2_FG_biomass,
            ~ {
              if (is.null(.x) || !"R2_marginal" %in% names(.x)) {
                NA_real_
              } else {
                as.numeric(.x$R2_marginal[1])
              }
            }
          )
        ) |>
        select(response, R2_marginal)
      plot_model_heatmap(heatmap_data, group_lookup, r2_tibble = r2_tbl)
    }
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
          R2m_no   = purrr::map_dbl(r2,          ~ r2_safe_get(.x, "R2_marginal")),
          R2c_no   = purrr::map_dbl(r2,          ~ r2_safe_get(.x, "R2_conditional")),
          R2m_bio  = purrr::map_dbl(r2_biomass,  ~ r2_safe_get(.x, "R2_marginal")),
          R2c_bio  = purrr::map_dbl(r2_biomass,  ~ r2_safe_get(.x, "R2_conditional")),
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
        facet_wrap(~ comparison) +
        scale_colour_manual(values = c(marginal = "#1b9e77", conditional = "#d95f02"),
                            name = "R² type",
                            labels = c("Marginal", "Conditional")) +
        labs(
          x = "R² (no biomass model)",
          y = "R² (biomass model)",
          title = "R² comparison for single-function models"
        ) +
        theme_bw()
    }
  ),
  tar_target(
    name = single_function_fg_biomass_panel_plot,
    command = plot_response_vs_fg_biomass(
      data = big_data |> tidylog::filter(!fg_remaining %in% c("GF", "FB", "GB")),
      response_name = "min_soil_moisture"
    )
  ),

  ### Group-level models (same structure as single response)
  tar_target(
    name = model_group,
    command = make_model(
      dat = absolute_group_data |> filter(!fg_remaining %in% c("GF", "FB", "GB")),
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
      dat = multifunctionality |> filter(!fg_remaining %in% c("GF", "FB", "GB")),
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

  # model for functional group richness
  tar_target(
    name = model_multifunctionality_richness,
    command = make_model(
      dat = multifunctionality |> filter(!fg_remaining %in% c("GF", "FB", "GB")),
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
  ),
  tar_target(
    name = model_multifunctionality_richness_plot,
    command = model_multifunctionality_richness |>
      unnest(data) |>
      ggplot(aes(x = total_biomass, y = .response, size = total_biomass, shape = as.factor(.functional_group))) +
      geom_point(alpha = 0.6) +
      scale_x_continuous(
        name = "Total biomass",
        expand = expansion(mult = c(0.05, 0.05))
      ) +
      scale_y_continuous(name = "Multifunctionality") +
      scale_size_continuous(name = "Total biomass") +
      scale_shape_discrete(name = "Functional group") +
      theme_bw()
  )
)
