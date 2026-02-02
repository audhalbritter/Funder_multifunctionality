# Supporting information analysis plan
si_analysis_plan <- list(

  #### MULTIVARIATE ANALYSES ####
  # Run models for only 9 sites (remove prec level 2100mm)
  tar_target(
    name = model_multi_9s,
    command = run_models(dat = multifunctionality |>
                           filter(precipitation_mm != 2100) |>
                           mutate(level = "global"),
                         group = "level",
                         response = multifuntionality,
                         fg_var = fg_richness)
  ),

  # results
  tar_target(
    name = model_multi_9s_out,
    command = {

      out <- model_multi_9s |>
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

  # Run models for 5 treatments only (remove all double treatments)
  tar_target(
    name = model_multi_5t,
    command = run_models(dat = multifunctionality |>
                           filter(fg_richness != 2) |>
                           mutate(level = "global"),
                         group = "level",
                         response = multifuntionality,
                         fg_var = fg_richness)
  ),

  # results
  tar_target(
    name = model_multi_5t_out,
    command = {

      out <- model_multi_5t |>
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

  # Run minimal model
  tar_target(
    name = model_multi_min,
    command = run_models(dat = multifunctionality |>
                           filter(precipitation_mm != 2100) |>
                           filter(fg_richness != 2) |>
                           mutate(level = "global"),
                         group = "level",
                         response = multifuntionality,
                         fg_var = fg_richness)
  ),

  # results
  tar_target(
    name = model_multi_min_out,
    command = {

      out <- model_multi_min |>
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
  )


)
