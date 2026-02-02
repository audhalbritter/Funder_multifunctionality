# community figures

make_sp_pca_figure <- function(community_pca){

  e_B <- eigenvals(community_pca[[3]])/sum(eigenvals(community_pca[[3]]))

  species <- community_pca[[2]] |>
    mutate(length = sqrt(PC1^2 + PC2^2),
           Label = capitalize(Label)) |>
    filter(length > 0.7) |>
    select(Label, length)

  community_pca[[1]] |>
    #mutate(Treatment = recode(Treatment, CTL = "Control", OTC = "Warming")) |>
    ggplot(aes(x = PC1, y = PC2, colour = treatment)) +
    # ## arrows
    # geom_segment(data = pca_sp[[2]], aes(x = 0, y = 0, xend = PC1, yend = PC2),
    #              arrow=arrow(length=unit(0.2,"cm")),
    #              alpha = 0.75, color = 'grey70') +
    # points and path
    geom_point(aes(size = ifelse(year == min(as.numeric(year)), "First", "Other"))) +
    geom_path(aes(linetype = treatment, group = plotID)) +
    coord_equal() +
    scale_size_discrete(name = "Year", range = c(1.5, 3), limits = c("Other", "First"), breaks = c("First", "Other")) +
    # scale_linetype_manual(values = c("dashed", "solid")) +
    # scale_shape_manual(values = c(1, 17)) +
    facet_grid(temperature_level ~ precipitation_level) +

    theme_bw()
    theme(text = element_text(size = 13),
          legend.box="vertical",
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())


  # p_all <- p1 +
  #   labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
  #        y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)"),
  #        tag = "A") +
  #   scale_color_manual(name = "Habitat type",
  #                      values = c("blue", "forestgreen", "orange"),
  #                      labels = expression(Snowbed, italic(Cassiope)~heath, italic(Dryas)~heath)) +
  #   xlim(-3, 3.8) +
  #   # species names
  #   geom_text(data = pca_sp[[2]] |>
  #               mutate(Label = capitalize(Label)) |>
  #               inner_join(species, by = "Label") |>
  #               mutate(Label = if_else(Label == "Unidentified liverwort sp", "Liverwort sp", Label)),
  #             aes(x = PC1 + case_when(Label == "Festuca rubra" ~ -1,
  #                                     Label == "Liverwort sp" ~ -1,
  #                                     Label == "Peltigera sp" ~ 0.5,
  #                                     Label == "Dicranum sp" ~ 1,
  #                                     TRUE ~ 0),
  #                 y = PC2 + case_when(PC2 > 0 ~ 0.3,
  #                                     Label == "Dicranum sp" ~ 0.1,
  #                                     TRUE ~ -0.3),
  #                 label = Label), col = 'black') +
  #   # stats
  #   geom_text(aes(x = -2.5, y = 1, label = "T x H*** + H x Y***"), colour = "black") +
  #   theme(legend.position = "top")
}
