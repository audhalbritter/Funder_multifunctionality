# pca plan

pca_plan <- list(
    # PERMANOVA models: (1) climate only, (2) treatment + climate + biomass, (3) FG biomass + climate
    tar_target(
        name = pca_permanova_models,
        command = list(
            climate = "temperature_scaled * precipitation_scaled",
            fg_climate_biomass = "treatment + temperature_scaled * precipitation_scaled + total_biomass_scaled",
            biomass_fg_climate = "biomass_scaled_G + biomass_scaled_F + biomass_scaled_B + temperature_scaled * precipitation_scaled"
        )
    ),

    # PCA of all plots (excluding single removals F, G, B); PERMANOVA with full three models
    tar_target(
        name = pca_all,
        command = make_pca(big_data |> filter(!treatment %in% c("F", "G", "B")), formula_terms = pca_permanova_models)
    ),

    # PCA with site effect partitioned out; same PERMANOVA models
    tar_target(
        name = pca_no_site,
        command = make_pca(big_data |> filter(!treatment %in% c("F", "G", "B")), partition = "siteID", formula_terms = pca_permanova_models)
    ),
    tar_target(
        name = pca_all_vs_no_site_plot,
        command = {
            full <- plot_pca_separate(
                pca_all,
                facet_var = "habitat",
                facet_ncol = 1,
                show_loadings_legend = FALSE,
                components = TRUE
            )
            site <- plot_pca_separate(
                pca_no_site,
                display = "treatment",
                facet_var = "fg_remaining",
                facet_ncol = 2,
                facet_nrow = 3,
                point_size_var = "total_biomass",
                point_alpha = 0.6,
                flip_axes = TRUE,
                components = TRUE
            )
            panel_title <- function(label) {
                list(
                    ggtitle(label),
                    theme(
                        plot.title = element_text(
                            size = 18,
                            face = "bold",
                            hjust = 0,
                            margin = margin(t = 0, r = 0, b = 4, l = 0)
                        ),
                        plot.title.position = "panel",
                        plot.margin = margin(t = 6, r = 4, b = 2, l = 4)
                    )
                )
            }
            full$sites <- full$sites + panel_title("a) All data")
            site$sites <- site$sites + panel_title("b) Site effect partitioned out")
            # Points keep the 1:2 column split. Loadings are joined on their own
            # row so the two biplots are the same size.
            points <- full$sites + site$sites +
                patchwork::plot_layout(widths = c(1, 2))
            loadings <- full$loadings + site$loadings +
                patchwork::plot_layout(widths = c(1, 1), guides = "collect")
            points / loadings +
                patchwork::plot_layout(heights = c(3, 1.5))
        }
    ),
    tar_target(
        name = pca_permanova_table,
        command = make_permanova_comparison_table(pca_all, pca_no_site)
    ),
    tar_target(
        name = pca_scree_plot,
        command = (make_scree_plot(pca_all) + ggtitle("Scree: all data")) |
            (make_scree_plot(pca_no_site) + ggtitle("Scree: site effect partitioned out")) +
                patchwork::plot_layout(guides = "collect") +
                patchwork::plot_annotation(tag_levels = "a") &
                theme(legend.position = "top")
    )
)
