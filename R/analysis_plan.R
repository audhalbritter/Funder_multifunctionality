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
        gt() |>
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = `p.value` <= 0.05
          )
        ) %>%
        table_style(., font_size = 11)

    }
  ),

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
        gt() |>
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = `p.value` <= 0.05
          )
        )%>%
        table_style(., font_size = 11)

    }
  ),


  #### ANALYSIS FOR 4 GROUPS ####
  # run all models
  tar_target(
    name = model_group,
    command = run_models(dat = big_data |>
                           # remove functions where we have many
                           filter(!response %in% c("decomposition forbs", "Reco")) |>
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
        gt() |>
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = `p.value` <= 0.05
          )
        ) %>%
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
        gt() |>
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = `p.value` <= 0.05
          )
        )%>%
        table_style(., font_size = 11)

    }
  ),


  #### MULTIFUNCTIONALITY ANALYSIS ####

  # run models at multifunctionality level
  tar_target(
    name = model_multifun,
    command = run_models(dat = multifunctionality |>
                           mutate(level = "global"),
                           group = "level",
                            response = multifuntionality,
                            fg_var = fg_richness)

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
        gt() |>
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = `p.value` <= 0.05
          )
        )%>%
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
        gt() |>
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = `p.value` <= 0.05
          )
        )%>%
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
        gt() |>
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_body(
            columns = c(term, estimate, std.error, statistic, df, p.value),
            rows = `p.value` <= 0.05
          )
        )%>%
        table_style(., font_size = 11)

    }
  ),

  # check models (turn this off when still playing with models!)
  # tar_quarto(name = model_check,
  #            path = "R/model_checking.qmd")

)









