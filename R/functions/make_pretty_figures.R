# Pretty figures

#' Heatmap of model estimates (paletteer ramp); bold numbers = p <= 0.01.
#' Faceted by group (facet_grid, free_y) instead of side stripes — compare visually.
#'
#' @param model_data Data frame with columns: response, term, estimate, p.value (numeric)
#' @param group_lookup Data frame with columns: response, group (for sorting responses)
#' @param group_order Character vector: order of groups for y-axis (default: microenvironment, nutrient cycling, carbon cycling, nematodes, microbes)
#' @param fancy_terms Apply fancy_stats term formatting (default TRUE)
#' @param functional_group_as Passed to fancy_stats: `"fg_remaining"` (default), `"richness"`, or
#'   `"fg_bio"`. For `"fg_bio"`, the intercept label is kept as-is (not renamed to "Bare").
plot_model_heatmap <- function(model_data,
                               group_lookup,
                               group_order = c("microenvironment", "nutrient cycling", "carbon cycling", "nematodes", "microbes"),
                               fancy_terms = TRUE,
                               functional_group_as = "fg_remaining") {

  # Join group and define explicit response order
  group_lookup_distinct <- group_lookup |> distinct(response, group)
  dat <- model_data |>
    left_join(group_lookup_distinct, by = "response")

  # Explicit response order: by group_order, then alphabetically within group
  group_order_df <- tibble(group = group_order, group_order = seq_along(group_order))
  response_order <- dat |>
    distinct(response, group) |>
    left_join(group_order_df, by = "group") |>
    mutate(group_order = replace_na(group_order, 999L)) |>
    arrange(group_order, response) |>
    pull(response)

  dat <- dat |>
    mutate(response = factor(response, levels = response_order))

  # Apply fancy term names if requested
  if (fancy_terms) {
    dat <- dat |> fancy_stats(sort = FALSE, functional_group_as = functional_group_as)
  } else {
    dat <- dat |>
      mutate(term = str_replace(term, "\\.functional_group", ""),
             term = str_replace(term, "temperature_scaled", "T"),
             term = str_replace(term, "precipitation_scaled", "P"),
             term = str_replace(term, "\\(Intercept\\)", "Intercept"))
  }

  # Heatmap-only display tweaks (not applied to tables):
  # "Intercept" -> "Bare" only for fg_remaining / richness models
  #   (baseline = bare ground / no FGs remaining; NOT meaningful for fg_bio where baseline = FG "B")
  # "microenvironment" -> "microenv" (shorter facet label)
  dat <- dat |>
    mutate(
      term = if_else(
        as.character(term) == "Intercept" & functional_group_as != "fg_bio",
        "Bare", as.character(term)
      ),
      group = str_replace(as.character(group), "microenvironment", "microenv")
    )

  # Significance: p <= 0.01 = bold
  dat <- dat |>
    mutate(p_raw = as.numeric(p.value)) |>
    mutate(p_raw = if_else(is.na(p_raw), 1, p_raw)) |>
    mutate(significant = p_raw <= 0.01)

  # Order terms: FGs first (baseline = "None"; intact control = "All"),
  # then biomass, then climate, then interactions.
  # (matches fancy_stats labels: Bio = total_biomass; BioG/BioF/BioB = composition)
  # fg_bio model: Intercept = baseline FG "B"; contrasts F/G; Bio = biomass_scaled
  term_levels <- c(
    "Bare", "Intercept",
    # fg_remaining contrasts (None = baseline; All = intact vegetation control)
    "F", "G", "B", "GF", "FB", "GB", "All",
    # Biomass mains
    "Bio", "BioG", "BioF", "BioB",
    # Climate mains
    "T", "P",
    # FG × climate (two-way)
    "F:T", "G:T", "B:T", "GF:T", "FB:T", "GB:T", "All:T",
    "F:P", "G:P", "B:P", "GF:P", "FB:P", "GB:P", "All:P",
    # FG × climate (three-way)
    "F:T:P", "G:T:P", "B:T:P", "GF:T:P", "FB:T:P", "GB:T:P", "All:T:P",
    # Total biomass × climate (model 2)
    "Bio:T", "Bio:P", "Bio:T:P",
    # Composition × climate (model 3)
    "BioG:T", "BioF:T", "BioB:T",
    "BioG:P", "BioF:P", "BioB:P",
    "BioG:T:P", "BioF:T:P", "BioB:T:P",
    # fg_bio model: FG identity × biomass × climate interactions
    "F:Bio", "G:Bio",
    "F:Bio:T", "G:Bio:T", "F:Bio:P", "G:Bio:P",
    "F:Bio:T:P", "G:Bio:T:P",
    # Climate interaction only
    "T:P"
  )
  all_terms <- unique(as.character(dat$term))
  ordered_terms <- c(intersect(term_levels, all_terms), setdiff(all_terms, term_levels))
  dat <- dat |>
    mutate(term = factor(term, levels = ordered_terms))

  # Apply the same "microenvironment" -> "microenv" rename to group_order so levels align
  group_order_display <- str_replace(group_order, "microenvironment", "microenv")

  # Group factor for facets: group_order first, then any other groups
  dat <- dat |>
    mutate(
      group = factor(
        group,
        levels = unique(c(group_order_display, as.character(dat$group)))
      )
    )

  # Per-facet y-axis: only responses in that group; level order reversed so first in
  # response_order appears at top of panel (matches previous rev(levels(...)) behaviour)
  dat <- dat |>
    mutate(response = as.character(response)) |>
    group_by(group) |>
    mutate(
      response = factor(
        response,
        # intersect keeps response_order; rev so first in order is at top of panel
        levels = rev(intersect(response_order, unique(response)))
      )
    ) |>
    ungroup()

  # Heatmap fill: colorBlindness Blue2DarkOrange (12 steps); symmetric limits when span includes 0
  est_range <- range(dat$estimate, na.rm = TRUE)
  fill_limits <- if (!anyNA(est_range) && est_range[1] < 0 && est_range[2] > 0) {
    m <- max(abs(est_range))
    c(-m, m)
  } else {
    NULL
  }

  # Facet by group: free_y + free space so panel height matches #responses per group
  p_heatmap <- ggplot(dat, aes(x = term, y = response, fill = estimate)) +
    geom_tile(colour = "grey90", linewidth = 0.3) +
    geom_text(
      aes(label = round(estimate, 2), fontface = ifelse(significant, "bold", "plain")),
      size = 5.2, colour = "grey20"
    ) +
    scale_fill_gradientn(
      colours = paletteer::paletteer_d("colorBlindness::Blue2DarkOrange12Steps"),
      limits = fill_limits,
      name = "Estimate"
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    scale_y_discrete() +
    facet_grid(
      rows = vars(group),
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      legend.position = "right",
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "grey92", colour = "grey80"),
      # Facet labels on left: rotate 90° so they read vertically
      strip.text.y.left = element_text(size = 11, angle = 90, hjust = 0.5, vjust = 0.5),
      panel.spacing.y = unit(0.4, "lines")
    )

  p_heatmap
}
