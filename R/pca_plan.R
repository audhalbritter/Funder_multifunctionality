# new analysis plan

new_analysis_plan <- list(

    # pca of all functions
    tar_target(
        name = all_functions_pca,
        command = make_pca(big_data)
    ),

    tar_target(
        name = all_functions_pca_plot,
        command = plot_pca(all_functions_pca)
    )

)