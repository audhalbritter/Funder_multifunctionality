### make figures


# make_group_figure
make_group_figure <- function(big_data, group){

  big_data |>
    filter(group == {{group}}) |>
    mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm"))) |>
    ggplot(aes(x = fg_richness, y = value_std, colour = response)) +
    geom_point() +
    geom_smooth(method = "lm", mapping = aes(fill = response), alpha = 0.15) +
    scale_x_continuous(breaks = c(0, 1, 2, 3)) +
    scale_colour_viridis_d(option = "inferno", end = 0.85) +
    scale_fill_viridis_d(option = "inferno", end = 0.85) +
    labs(x = "Number of functional groups present",
         y ="Standardized function",
         title = {{group}}) +
    guides(fill = "none") +
    facet_grid(habitat ~ precipitation_name) +
    theme_bw()

}
