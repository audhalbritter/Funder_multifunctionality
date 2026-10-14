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
            p_all <- plot_pca_separate(pca_all, facet_var = "habitat") + ggtitle("All data")
            p_no_site <- plot_pca_separate(
                pca_no_site,
                display = "treatment",
                facet_var = "fg_remaining",
                point_size_var = "total_biomass",
                point_alpha = 0.6
            ) + ggtitle("Site effect partitioned out")
            p_all | p_no_site +
                patchwork::plot_layout(guides = "collect") +
                patchwork::plot_annotation(tag_levels = "a")
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
