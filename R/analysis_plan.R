# data analysis plan
analysis_plan <- list(

  #### ANALYSIS FOR EACH FUNCTION SEPARATELY ####
  # run 3 models for each function separately
  tar_target(
    name = model_response,
    command = run_models(dat = big_data,
                         response_var = value_std,
                         fg_var = fg_richness,
                         group = "response")
  ),

  # results number of functional groups
  tar_target(
    name = model_response_nr_out,
    command = {

      out <- model_response |>
        unnest(result) |>
        filter(effect == "fixed") |>
        select(-data, -model, -model_factorial, -model_treatment, -result_factorial, -result_treatment, -group, -effect, -anova, -anova_tidy) %>%
        fancy_stats(.)

      round_numbers_tidy(out) |>
        gt() %>%
        # significant: p <= 0.05 (bold, default colour)
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw <= 0.05
          )
        ) %>%
        # marginal: 0.05 < p <= 0.07 (bold, grey)
        tab_style(
          style = list(
            cell_text(style = "italic")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw > 0.05 & p_raw <= 0.07
          )
        ) %>%
        cols_hide(p_raw) %>%
        table_style(., font_size = 11)

    }
  ),

  # # results factorial analysis
  # tar_target(
  #   name = model_response_factorial_out,
  #   command = {

  #     out <- model_response |>
  #       unnest(result_factorial) |>
  #       filter(effect == "fixed") |>
  #       select(-data, -model, -model_factorial, -model_treatment, -result, -result_treatment, -group, -effect, -anova, -anova_tidy) %>% 
  #       fancy_stats(.)

  #     round_numbers_tidy(out) |>
  #       gt() |>
  #       tab_style(
  #         style = list(
  #           cell_text(weight = "bold")
  #         ),
  #         locations = cells_body(
  #           columns = c(term, estimate, std.error, statistic, df, p.value),
  #           rows = `p.value` <= 0.05
  #         )
  #       ) %>%
  #       table_style(., font_size = 11)

  #   }
  # ),

  # results treatment only model
  tar_target(
    name = model_response_trt_out,
    command = {

      out <- model_response |>
        unnest(result_treatment) |>
        filter(effect == "fixed") |>
        select(-data, -model, -model_factorial, -model_treatment, -result, -result_factorial, -group, -effect, -anova, -anova_tidy) %>%
        fancy_stats(.)

      round_numbers_tidy(out) |>
        gt() %>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw <= 0.05
          )
        ) %>%
        tab_style(
          style = list(
            cell_text(style = "italic")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw > 0.05 & p_raw <= 0.07
          )
        ) %>%
        cols_hide(p_raw) %>%
        table_style(., font_size = 11)

    }
  ),


  #### ANALYSIS FOR 4 GROUPS ####
  # run all models
  tar_target(
    name = model_group,
    command = run_models(dat = big_data |> 
                           # remove functions where we have many
                           filter(!response %in% c("specific_root_length_m_per_g", "root_tissue_density_g_per_m3", "root_dry_matter_content",
                           "nema_bacterivores_density", "nema_omnivores_density", "nema_herbivores_density",
                           "nema_predators_density", "nema_fungivores_density", "micro_fungivorous_density",
                           "micro_nematophagous_density", "micro_predaceous_density",
                            "decomposition forbs", "Reco")) |>
                           rename(level = group),
                         response_var = value_std,
                         fg_var = fg_richness,
                         group = "level")
  ),

  # result for number of functional groups present
  tar_target(
    name = model_group_nr_out,
    command = {

      out <- model_group |>
        unnest(result) |>
        filter(effect == "fixed") |>
        select(-data, -model, -model_factorial, -model_treatment, -result_factorial, -result_treatment, -group, -effect, -anova, -anova_tidy) %>%
        fancy_stats(.)

      round_numbers_tidy(out) |>
        gt() %>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw <= 0.05
          )
        ) %>%
        tab_style(
          style = list(
            cell_text(style = "italic")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw > 0.05 & p_raw <= 0.07
          )
        ) %>%
        cols_hide(p_raw) %>%
        table_style(., font_size = 11)

    }
  ),

  # result for treatment only model
  tar_target(
    name = model_group_trt_out,
    command = {

      out <- model_group |>
        unnest(result_treatment) |>
        filter(effect == "fixed") |>
        select(-data, -model, -model_factorial, -model_treatment, -result, -result_factorial, -group, -effect, -anova, -anova_tidy) %>%
        fancy_stats(.)

      round_numbers_tidy(out) |>
        gt() %>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw <= 0.05
          )
        ) %>%
        tab_style(
          style = list(
            cell_text(style = "italic")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw > 0.05 & p_raw <= 0.07
          )
        ) %>%
        cols_hide(p_raw) %>%
        table_style(., font_size = 11)

    }
  ),


  #### MULTIFUNCTIONALITY ANALYSIS ####

  # run models at multifunctionality level (drop rows with NA in model variables)
  tar_target(
    name = model_multifun,
    command = multifunctionality %>%
      filter(complete.cases(pick(fg_richness, temperature_scaled, precipitation_scaled, multifuntionality))) %>%
      run_models(dat = .,
                 group = "level",
                 response = multifuntionality,
                 fg_var = fg_richness)
  ),

  # make prediction for functional groups present model
  tar_target(
    name = multi_nr_pred,
    command = model_multifun |>
      select(data, model) |>
      mutate(prediction = map2(.x = data, .y = model, .f = ~ safely(lmer_prediction)(.x, .y)$result)) |>
      # merge data and prediction; when prediction is NULL, add NA columns so fitted exists
      mutate(output = map2(.x = data, .y = prediction, .f = ~ {
        if (is.null(.y)) {
          n <- nrow(.x)
          bind_cols(.x, tibble(fitted = rep(NA_real_, n), plo = NA_real_, phi = NA_real_, tlo = NA_real_, thi = NA_real_))
        } else {
          # bind only prediction columns to avoid duplicating .response, .functional_group, etc.
          bind_cols(.x, .y %>% select(fitted, plo, phi, tlo, thi))
        }
      })) |>
      unnest(output)
  ),


  # results for number of functional groups present
  tar_target(
    name = model_multi_nr_out,
    command = {

      out <- model_multifun |>
        unnest(result) |>
        filter(effect == "fixed") |>
        select(-data, -model, -model_factorial, -model_treatment, -group, -effect, -anova, -anova_tidy, -result_factorial, -result_treatment) %>%
        fancy_stats(.)

      round_numbers_tidy(out) |>
        gt() %>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw <= 0.05
          )
        ) %>%
        tab_style(
          style = list(
            cell_text(style = "italic")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw > 0.05 & p_raw <= 0.07
          )
        ) %>%
        cols_hide(p_raw) %>%
        table_style(., font_size = 11)

    }
  ),

  # results for factorial model
  tar_target(
    name = model_multi_fact_out,
    command = {

      out <- model_multifun |>
        unnest(result_factorial) |>
        filter(effect == "fixed") |>
        select(-data, -model, -model_factorial, -model_treatment, -group, -effect, -result, -anova, -anova_tidy, -result_treatment) %>%
        fancy_stats(., sort = FALSE)

      round_numbers_tidy(out) |>
        gt() %>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw <= 0.05
          )
        ) %>%
        tab_style(
          style = list(
            cell_text(style = "italic")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw > 0.05 & p_raw <= 0.07
          )
        ) %>%
        cols_hide(p_raw) %>%
        table_style(., font_size = 11)

    }
  ),

  # results for treatmemt model only
  tar_target(
    name = model_multi_trt_out,
    command = {

      out <- model_multifun |>
        unnest(result_treatment) |>
        filter(effect == "fixed") |>
        select(-data, -model, - model_factorial, -model_treatment, -result, -result_factorial, -group, -effect, -anova, -anova_tidy) %>%
        fancy_stats(.)

      round_numbers_tidy(out) |>
        gt() %>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw <= 0.05
          )
        ) %>%
        tab_style(
          style = list(
            cell_text(style = "italic")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = p_raw > 0.05 & p_raw <= 0.07
          )
        ) %>%
        cols_hide(p_raw) %>%
        table_style(., font_size = 11)

    }
  )

  # check models (turn this off when still playing with models!)
  # tar_quarto(name = model_check,
  #            path = "R/model_checking.qmd")


  #### VARIANCE PARTITIONING ####

  # run vp for responses
  # tar_target(
  #   name = vp_response,
  #   command = make_variance_partioning(dat = big_data,
  #                                      response_var = fg_richness,
  #                                      fg_var = value_std,
  #                                      group = "response")
  # ),

  # run vp for multifunctionality
  # tar_target(
  #   name = vp_multifun,
  #   command = make_variance_partioning(dat = multifunctionality,
  #                                      response_var = fg_richness,
  #                                      fg_var = multifuntionality,
  #                                      group = "level")
  # )

)