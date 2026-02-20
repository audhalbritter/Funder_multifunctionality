# new analysis plan

new_analysis_plan <- list(

# PCA of bare ground and intact vegetation plots
    # pca of bare ground plots (with PERMANOVA: temperature × precipitation grid)
    tar_target(
        name = pca_bare,
        command = make_pca(big_data |> filter(treatment == "FGB"), formula_terms = "temperature_scaled * precipitation_scaled")
    ),

    # pca of intact vegetation (with PERMANOVA: temperature × precipitation grid)
    tar_target(
        name = pca_intact,
        command = make_pca(big_data |> filter(treatment == "C"), formula_terms = "temperature_scaled * precipitation_scaled")
    ),

    tar_target(
        name = pca_combined_scree,
        command = (make_scree_plot(pca_bare) + ggtitle("Bare ground")) +
            (make_scree_plot(pca_intact) + ggtitle("Intact vegetation")) +
            patchwork::plot_layout(ncol = 2)
    ),

    tar_target(
        name = pca_combined_plot,
        command = {
            p_bare <- plot_pca_separate(pca_bare) + ggtitle("Bare ground")
            p_intact <- plot_pca_separate(pca_intact) + ggtitle("Intact vegetation")
            (p_bare | p_intact) + patchwork::plot_layout(guides = "collect")
        }
    ),

    # PCA of all plots partitioned by siteID (excluding single removals F, G, B)
    tar_target(
        name = pca_all,
        command = make_pca(big_data |> filter(!treatment %in% c("F", "G", "B")))
    ),

    tar_target(
        name = pca_no_site,
        command = make_pca(big_data |> filter(!treatment %in% c("F", "G", "B")), partition = "siteID")
    ),
    tar_target(
        name = pca_all_scree,
        command = make_scree_plot(pca_all)
    ),
    tar_target(
        name = pca_no_site_scree,
        command = make_scree_plot(pca_no_site)
    ),
    tar_target(
        name = pca_all_vs_no_site_plot,
        command = {
            p_all <- plot_pca_separate(pca_all, facet_var = "treatment") + ggtitle("All data")
            p_no_site <- plot_pca_separate(pca_no_site, display = "treatment", facet_var = "treatment") + ggtitle("Site effect partitioned out")
            (p_all | p_no_site) + patchwork::plot_layout(guides = "collect")
        }
    )

)