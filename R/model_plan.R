# run models

model_plan <- list(

    # run treatment model for each response
    tar_target(
        name = model_single_function,
        command = make_model(dat = big_data |> 
          filter(!treatment %in% c("F", "G", "B")),
                             response_var = value_std,
                             fg_var = treatment,
                             group = "response")
    ),

    tar_target(
        name = model_single_function_out,
        command = {
            out <- model_single_function |>
                unnest(result) |>
                filter(effect == "fixed") |>
                select(-data, -model, -effect, -group, -anova, -anova_tidy) %>%
                fancy_stats(.)
            round_numbers_tidy(out) |>
                gt() |>
                tab_style(
                    style = list(cell_text(weight = "bold")),
                    locations = cells_body(
                        columns = c(term, estimate, std.error, statistic, df, p.value),
                        rows = p_raw <= 0.05
                    )
                ) |>
                tab_style(
                    style = list(cell_text(style = "italic")),
                    locations = cells_body(
                        columns = c(term, estimate, std.error, statistic, df, p.value),
                        rows = p_raw > 0.05 & p_raw <= 0.07
                    )
                ) |>
                cols_hide(p_raw) %>%
                table_style(., font_size = 11)
        }
    ),

    tar_target(
        name = model_single_function_heatmap,
        command = {
            heatmap_data <- model_single_function |>
                unnest(result) |>
                filter(effect == "fixed") |>
                select(response, term, estimate, p.value)
            group_lookup <- big_data |> distinct(response, group)
            plot_model_heatmap(heatmap_data, group_lookup)
        }
    ),

    # Same analysis with contrast to bare ground (value_contrast)
    tar_target(
        name = model_single_function_contrast,
        command = make_model(dat = big_data_contrast |>
          filter(!treatment %in% c("F", "G", "B")),
                             response_var = value_contrast,
                             fg_var = treatment,
                             group = "response")
    ),

    tar_target(
        name = model_single_function_contrast_out,
        command = {
            out <- model_single_function_contrast |>
                unnest(result) |>
                filter(effect == "fixed") |>
                select(-data, -model, -effect, -group, -anova, -anova_tidy) |>
                fancy_stats()
            round_numbers_tidy(out) |>
                gt() |>
                tab_style(
                    style = list(cell_text(weight = "bold")),
                    locations = cells_body(
                        columns = c(term, estimate, std.error, statistic, df, p.value),
                        rows = p_raw <= 0.05
                    )
                ) |>
                tab_style(
                    style = list(cell_text(style = "italic")),
                    locations = cells_body(
                        columns = c(term, estimate, std.error, statistic, df, p.value),
                        rows = p_raw > 0.05 & p_raw <= 0.07
                    )
                ) |>
                cols_hide(p_raw) |>
                table_style(font_size = 11)
        }
    ),

    tar_target(
        name = model_single_function_contrast_heatmap,
        command = {
            heatmap_data <- model_single_function_contrast |>
                unnest(result) |>
                filter(effect == "fixed") |>
                select(response, term, estimate, p.value)
            group_lookup <- big_data_contrast |> distinct(response, group)
            plot_model_heatmap(heatmap_data, group_lookup)
        }
    )
)