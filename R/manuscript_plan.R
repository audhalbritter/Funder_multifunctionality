# Manuscript plan
manuscript_plan <- list(
    tar_target(
        name = response_group_table,
        command = big_data |>
            mutate(
                group = factor(
                    group,
                    levels = c(
                        "microenvironment",
                        "nutrient cycling",
                        "carbon cycling",
                        "nematodes",
                        "microbes"
                    )
                ),
                response = pretty_response_label(response)
            ) |>
            summarise(
                response = paste(sort(unique(response)), collapse = ", "),
                .by = group
            ) |>
            arrange(group) |>
            gt() |>
            tab_header(title = "Ecosystem functions by group") |>
            cols_label(group = "Group", response = "Ecosystem functions / responses")
    ),

    # manuscript
    tar_quarto(
        name = manuscript,
        path = "manuscript/manuscript.qmd",
        # Render from manuscript/ so store = "../_targets" is this project's store.
        working_directory = "manuscript"
    )

    # SI
    # tar_quarto(name = si,
    #            path = "SI.qmd")
)
