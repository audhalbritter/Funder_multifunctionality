### make figures

# Pretty labels for response names used in plots.
pretty_response_lookup <- c(
  "bacterivore feeder density" = "Bacterivore feeder",
  "fungivore feeder density" = "Fungivore feeder",
  "plant_feeder feeder density" = "Plant feeder",
  "predator feeder density" = "Predator feeder",
  "bacterial_fungal_feeder_ratio" = "Bacteria:fungal feeder",
  "plant_parasite_index" = "Plant parasite index",
  "maturity_index" = "Maturity index",
  "bacteria density" = "Bacteria",
  "fungi density" = "Fungi",
  "bacteria-fungi ratio" = "Bacteria:Fungi",
  "bacteria_fungi_ratio" = "Bacteria:Fungi",
  "root_associated_fungi" = "Root associated",
  "saprotroph" = "Saprotroph",
  "yeast_dimorphic" = "Yeast diamorphic",
  "ectomycorrhizal" = "Ectomycorrhizal",
  "melanised fungal necromass decomposition" = "Melanised",
  "non-melanised fungal necromass decomposition" = "Non-melanised",
  "decomposition forbs" = "Forb decomp",
  "decomposition graminoids" = "Graminoid decomp.",
  "carbon stock" = "C stock",
  "nitrogen stock" = "N stock",
  "gross primary productivity" = "GPP",
  "ecosystem respiration" = "ER",
  "phosphorus stock" = "P stock",
  "available_nitrogen" = "Avail. N",
  "available_phosphorus" = "Avail. P",
  "avialable_micronutrients" = "Avail. micronutrients",
  "daily_temp_amplitude" = "Daily T amplitude",
  "drought_index" = "Drought index"
)

pretty_response_label <- function(x) {
  dplyr::recode(as.character(x), !!!pretty_response_lookup, .default = as.character(x))
}

# Transformation applied before standardization. "shift" is the micronutrient
# PC1 location shift; that value is then copied through unchanged.
response_transformation <- function(response) {
  dplyr::case_when(
    response == "avialable_micronutrients" ~ "shift",
    stringr::str_detect(response, "ratio") & !stringr::str_detect(response, "respiration") ~ "logit",
    response %in% c("root_associated_fungi", "saprotroph", "yeast_dimorphic", "ectomycorrhizal") ~ "logit",
    response %in% c(
      "available_nitrogen", "available_phosphorus", "bacteria density",
      "bacterivore feeder density", "bacterivore density", "carbon stock",
      "fungi density", "fungivore feeder density", "nematode density",
      "nitrogen stock", "phosphorus stock", "plant_feeder feeder density",
      "predator feeder density"
    ) ~ "log",
    TRUE ~ "none"
  )
}

# Rename a response column to pretty labels in-place.
rename_response_pretty <- function(dat, response_col = response) {
  response_col <- rlang::as_string(rlang::ensym(response_col))
  dat |>
    mutate("{response_col}" := pretty_response_label(.data[[response_col]]))
}

# --- Model Comparison Diagnostic Helpers ---

#' Internal helper to extract scalar R2 values safely
r2_safe_get <- function(tbl, col) {
  if (is.null(tbl) || !col %in% names(tbl)) {
    return(NA_real_)
  }
  as.numeric(tbl[[col]][1])
}

#' Internal helper for estimate mapping labels
climate_term_short <- function(term) {
  case_when(
    term == "(Intercept)" ~ "Intercept",
    term == ".functional_groupF" ~ "F",
    term == ".functional_groupG" ~ "G",
    term == ".functional_groupB" ~ "B",
    term == "temperature_scaled" ~ "T",
    term == "precipitation_scaled" ~ "P",
    term == "temperature_scaled:precipitation_scaled" ~ "T:P",
    term == ".functional_groupF:temperature_scaled" ~ "F:T",
    term == ".functional_groupG:temperature_scaled" ~ "G:T",
    term == ".functional_groupB:temperature_scaled" ~ "B:T",
    term == ".functional_groupF:precipitation_scaled" ~ "F:P",
    term == ".functional_groupG:precipitation_scaled" ~ "G:P",
    term == ".functional_groupB:precipitation_scaled" ~ "B:P",
    TRUE ~ term
  )
}

#' Map terms between climate/biomass models and composition models
map_term_to_comparison_key <- function(term, model_type = c("standard", "composition")) {
  model_type <- match.arg(model_type)
  if (model_type == "standard") {
    case_when(
      term == ".functional_groupF" ~ "F",
      term == ".functional_groupG" ~ "G",
      term == ".functional_groupB" ~ "B",
      term == ".functional_groupF:temperature_scaled" ~ "F:temperature_scaled",
      term == ".functional_groupG:temperature_scaled" ~ "G:temperature_scaled",
      term == ".functional_groupB:temperature_scaled" ~ "B:temperature_scaled",
      term == ".functional_groupF:precipitation_scaled" ~ "F:precipitation_scaled",
      term == ".functional_groupG:precipitation_scaled" ~ "G:precipitation_scaled",
      term == ".functional_groupB:precipitation_scaled" ~ "B:precipitation_scaled",
      TRUE ~ NA_character_
    )
  } else {
    case_when(
      term == "biomass_scaled_F" ~ "F",
      term == "biomass_scaled_G" ~ "G",
      term == "biomass_scaled_B" ~ "B",
      term == "biomass_scaled_F:temperature_scaled" ~ "F:temperature_scaled",
      term == "biomass_scaled_G:temperature_scaled" ~ "G:temperature_scaled",
      term == "biomass_scaled_B:temperature_scaled" ~ "B:temperature_scaled",
      term == "biomass_scaled_F:precipitation_scaled" ~ "F:precipitation_scaled",
      term == "biomass_scaled_G:precipitation_scaled" ~ "G:precipitation_scaled",
      term == "biomass_scaled_B:precipitation_scaled" ~ "B:precipitation_scaled",
      TRUE ~ NA_character_
    )
  }
}

#' Plot R2 comparisons across three models
plot_single_function_r2_comparison <- function(model_tbl) {
  r2_dat <- model_tbl |>
    mutate(
      mod1_m = purrr::map_dbl(r2, ~ r2_safe_get(.x, "R2_marginal")),
      mod1_c = purrr::map_dbl(r2, ~ r2_safe_get(.x, "R2_conditional")),
      mod2_m = purrr::map_dbl(r2_biomass, ~ r2_safe_get(.x, "R2_marginal")),
      mod2_c = purrr::map_dbl(r2_biomass, ~ r2_safe_get(.x, "R2_conditional")),
      mod3_m = purrr::map_dbl(r2_FG_biomass, ~ r2_safe_get(.x, "R2_marginal")),
      mod3_c = purrr::map_dbl(r2_FG_biomass, ~ r2_safe_get(.x, "R2_conditional"))
    )

  prep_comp <- function(df, col_x_suffix, col_y_suffix, label) {
    bind_rows(
      df |> transmute(response, comparison = label, type = "marginal", x = .data[[paste0(col_x_suffix, "_m")]], y = .data[[paste0(col_y_suffix, "_m")]]),
      df |> transmute(response, comparison = label, type = "conditional", x = .data[[paste0(col_x_suffix, "_c")]], y = .data[[paste0(col_y_suffix, "_c")]])
    )
  }

  plot_dat <- bind_rows(
    prep_comp(r2_dat, "mod1", "mod2", "Mod 1 vs 2\n(No vs Total Bio)"),
    prep_comp(r2_dat, "mod1", "mod3", "Mod 1 vs 3\n(No vs FG Bio)"),
    prep_comp(r2_dat, "mod2", "mod3", "Mod 2 vs 3\n(Total vs FG Bio)")
  ) |> filter(!is.na(x), !is.na(y))

  ggplot(plot_dat, aes(x = x, y = y, colour = type)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    geom_point(alpha = 0.7) +
    coord_equal() +
    facet_wrap(~comparison) +
    scale_colour_manual(values = c(marginal = "#1b9e77", conditional = "#d95f02"), name = "R² type") +
    labs(x = "R² (lower index model)", y = "R² (higher index model)", title = "R² Comparison") +
    theme_bw() +
    theme(legend.position = "right")
}


#' Shared helper to group terms for estimate comparison plots
group_terms_for_plot <- function(df, response_group_lookup) {
  group_order <- c("microenvironment", "nutrient cycling", "carbon cycling", "nematodes", "microbes")

  df |>
    left_join(response_group_lookup |> distinct(response, group), by = "response") |>
    # Group terms for coloring and shapes
    mutate(
      # Color category: Identity of the functional group or Bare
      color_cat = case_when(
        stringr::str_detect(term, "F") ~ "F",
        stringr::str_detect(term, "G") ~ "G",
        stringr::str_detect(term, "B") ~ "B",
        stringr::str_detect(term, "All") ~ "All",
        term %in% c("Intercept", "Bare") ~ "Bare",
        TRUE ~ "Climate"
      ),
      # Shape category: Main effect vs T or P driver
      shape_cat = case_when(
        stringr::str_detect(term, "T") & stringr::str_detect(term, "P") ~ "T:P",
        stringr::str_detect(term, "T") ~ "T",
        stringr::str_detect(term, "P") ~ "P",
        TRUE ~ "Main"
      )
    ) |>
    mutate(
      color_cat = factor(color_cat, levels = c("Bare", "F", "G", "B", "All", "Climate")),
      shape_cat = factor(shape_cat, levels = c("Main", "T", "P", "T:P")),
      group = factor(stringr::str_to_sentence(group), levels = stringr::str_to_sentence(group_order))
    ) |>
    filter(!is.na(x), !is.na(y))
}

#' Plot Estimate comparisons across three models
plot_single_function_estimate_comparison <- function(model_tbl, response_group_lookup) {
  extract_fixed <- function(tbl, col) {
    tbl |>
      select(response, all_of(col)) |>
      unnest(cols = all_of(col)) |>
      filter(effect == "fixed") |>
      select(response, term, estimate)
  }

  est1 <- extract_fixed(model_tbl, "result")
  est2 <- extract_fixed(model_tbl, "result_biomass")
  est3 <- extract_fixed(model_tbl, "result_FG_biomass")

  comp12 <- inner_join(
    est1 |> mutate(term = climate_term_short(term)) |> rename(x = estimate),
    est2 |> mutate(term = climate_term_short(term)) |> rename(y = estimate),
    by = c("response", "term")
  ) |> mutate(comparison = "Mod 1 vs 2\n(No vs Total Bio)")

  comp13 <- inner_join(
    est1 |> mutate(key = map_term_to_comparison_key(term, "standard")) |> filter(!is.na(key)) |> rename(x = estimate),
    est3 |> mutate(key = map_term_to_comparison_key(term, "composition")) |> filter(!is.na(key)) |> rename(y = estimate),
    by = c("response", "key")
  ) |> mutate(comparison = "Mod 1 vs 3\n(No vs FG Bio)", term = key)

  comp23 <- inner_join(
    est2 |> mutate(key = map_term_to_comparison_key(term, "standard")) |> filter(!is.na(key)) |> rename(x = estimate),
    est3 |> mutate(key = map_term_to_comparison_key(term, "composition")) |> filter(!is.na(key)) |> rename(y = estimate),
    by = c("response", "key")
  ) |> mutate(comparison = "Mod 2 vs 3\n(Total vs FG Bio)", term = key)

  plot_dat <- bind_rows(comp12, comp13, comp23) |>
    group_terms_for_plot(response_group_lookup)

  ggplot(plot_dat, aes(x = x, y = y, colour = color_cat, shape = shape_cat)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    geom_point(alpha = 0.7, size = 2) +
    facet_grid(group ~ comparison) +
    scale_colour_manual(
      values = c("Bare" = "#7f7f7f", "F" = "#e41a1c", "G" = "#377eb8",
                 "B" = "#4daf4a", "All" = "#984ea3", "Climate" = "#ff7f00"),
      name = "Identity"
    ) +
    scale_shape_manual(
      values = c("Main" = 16, "T" = 17, "P" = 15, "T:P" = 8),
      name = "Interaction"
    ) +
    labs(x = "Estimate (lower index model)", y = "Estimate (higher index model)", title = "Fixed-Effect Estimates Comparison") +
    theme_bw() + theme(legend.position = "right")
}

#' --- Interaction vs Main Effects Comparison ---

#' Plot R2: Interaction vs Main Effects
plot_r2_interaction_vs_main <- function(model_int, model_main) {
  r2_int <- model_int |>
    mutate(
      int1_m = purrr::map_dbl(r2, ~ r2_safe_get(.x, "R2_marginal")),
      int1_c = purrr::map_dbl(r2, ~ r2_safe_get(.x, "R2_conditional")),
      int2_m = purrr::map_dbl(r2_biomass, ~ r2_safe_get(.x, "R2_marginal")),
      int2_c = purrr::map_dbl(r2_biomass, ~ r2_safe_get(.x, "R2_conditional")),
      int3_m = purrr::map_dbl(r2_FG_biomass, ~ r2_safe_get(.x, "R2_marginal")),
      int3_c = purrr::map_dbl(r2_FG_biomass, ~ r2_safe_get(.x, "R2_conditional"))
    )

  r2_main <- model_main |>
    mutate(
      main1_m = purrr::map_dbl(r2_main, ~ r2_safe_get(.x, "R2_marginal")),
      main1_c = purrr::map_dbl(r2_main, ~ r2_safe_get(.x, "R2_conditional")),
      main2_m = purrr::map_dbl(r2_main_biomass, ~ r2_safe_get(.x, "R2_marginal")),
      main2_c = purrr::map_dbl(r2_main_biomass, ~ r2_safe_get(.x, "R2_conditional")),
      main3_m = purrr::map_dbl(r2_main_FG_biomass, ~ r2_safe_get(.x, "R2_marginal")),
      main3_c = purrr::map_dbl(r2_main_FG_biomass, ~ r2_safe_get(.x, "R2_conditional"))
    )

  prep_comp <- function(mi, mm, mod_idx, label) {
    df <- inner_join(
      mi |> select(response, int_m = paste0("int", mod_idx, "_m"), int_c = paste0("int", mod_idx, "_c")),
      mm |> select(response, main_m = paste0("main", mod_idx, "_m"), main_c = paste0("main", mod_idx, "_c")),
      by = "response"
    )
    bind_rows(
      df |> transmute(response, comparison = label, type = "marginal", x = main_m, y = int_m),
      df |> transmute(response, comparison = label, type = "conditional", x = main_c, y = int_c)
    )
  }

  plot_dat <- bind_rows(
    prep_comp(r2_int, r2_main, 1, "Model 1\n(Climate only)"),
    prep_comp(r2_int, r2_main, 2, "Model 2\n(+Total Bio)"),
    prep_comp(r2_int, r2_main, 3, "Model 3\n(FG Bio)")
  ) |> filter(!is.na(x), !is.na(y))

  ggplot(plot_dat, aes(x = x, y = y, colour = type)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    geom_point(alpha = 0.7) +
    coord_equal() +
    facet_wrap(~comparison) +
    scale_colour_manual(values = c(marginal = "#1b9e77", conditional = "#d95f02"), name = "R² type") +
    labs(x = "R² (Main effects)", y = "R² (Interactions)", title = "R² Comparison: Main vs Interactions") +
    theme_bw() + theme(legend.position = "right")
}

#' Plot Estimates: Interaction vs Main Effects
plot_estimate_interaction_vs_main <- function(model_int, model_main, response_group_lookup) {
  extract_fixed <- function(tbl, col) {
    tbl |> select(response, all_of(col)) |> unnest(cols = all_of(col)) |> filter(effect == "fixed") |> select(response, term, estimate)
  }

  int1 <- extract_fixed(model_int, "result"); int2 <- extract_fixed(model_int, "result_biomass"); int3 <- extract_fixed(model_int, "result_FG_biomass")
  main1 <- extract_fixed(model_main, "result_main"); main2 <- extract_fixed(model_main, "result_main_biomass"); main3 <- extract_fixed(model_main, "result_main_FG_biomass")

  comp1 <- inner_join(main1 |> mutate(term = climate_term_short(term)) |> rename(x = estimate),
                      int1 |> mutate(term = climate_term_short(term)) |> rename(y = estimate), by = c("response", "term")) |> mutate(comparison = "Model 1\n(Climate only)")
  
  comp2 <- inner_join(main2 |> mutate(term = climate_term_short(term)) |> rename(x = estimate),
                      int2 |> mutate(term = climate_term_short(term)) |> rename(y = estimate), by = c("response", "term")) |> mutate(comparison = "Model 2\n(+Total Bio)")
  
  comp3 <- inner_join(main3 |> mutate(term = climate_term_short(term)) |> rename(x = estimate),
                      int3 |> mutate(term = climate_term_short(term)) |> rename(y = estimate), by = c("response", "term")) |> mutate(comparison = "Model 3\n(FG Bio)")

  plot_dat <- bind_rows(comp1, comp2, comp3) |>
    group_terms_for_plot(response_group_lookup)

  ggplot(plot_dat, aes(x = x, y = y, colour = color_cat, shape = shape_cat)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    geom_point(alpha = 0.7, size = 2) +
    facet_grid(group ~ comparison) +
    scale_colour_manual(
      values = c("Bare" = "#7f7f7f", "F" = "#e41a1c", "G" = "#377eb8",
                 "B" = "#4daf4a", "All" = "#984ea3", "Climate" = "#ff7f00"),
      name = "Identity"
    ) +
    scale_shape_manual(
      values = c("Main" = 16, "T" = 17, "P" = 15, "T:P" = 8),
      name = "Interaction"
    ) +
    labs(x = "Estimate (Main effects)", y = "Estimate (Interactions)", title = "Estimates Comparison: Main vs Interactions") +
    theme_bw() + theme(legend.position = "right")
}

#' Consolidated Interaction vs Main diagnostic plot
plot_interaction_vs_main_diagnostic <- function(model_int, model_main, response_group_lookup) {
  p_r2 <- plot_r2_interaction_vs_main(model_int, model_main)
  p_est <- plot_estimate_interaction_vs_main(model_int, model_main, response_group_lookup)

  p_r2 / p_est +
    patchwork::plot_layout(heights = c(1, 4)) +
    patchwork::plot_annotation(
      title = "Model Diagnostic Comparison: Main Effects vs Interactions",
      subtitle = "Comparing Main effects only (X-axis) vs Full interactions (Y-axis) for each model variant",
      tag_levels = "A"
    )
}

#' Consolidated multi-panel diagnostic plot
plot_model_comparison_diagnostic <- function(model_tbl, response_group_lookup) {
  p_r2 <- plot_single_function_r2_comparison(model_tbl)
  p_est <- plot_single_function_estimate_comparison(model_tbl, response_group_lookup)

  # Stack them: Give the R2 plot (A) less height than the large estimate grid (B)
  p_r2 / p_est +
    patchwork::plot_layout(heights = c(1, 3.5)) +
    patchwork::plot_annotation(
      title = "Single-Function Model Diagnostic Comparisons",
      subtitle = "Comparing No Biomass (Mod 1), +Total Biomass (Mod 2), and FG Biomass (Mod 3) variants",
      tag_levels = "A"
    )
}


# make_group_figure: one plot per group (4 total), raw multifunctionality + prediction lines,
# significance letters in upper right; FG-only = simple plot, FG:T = colour by habitat
make_group_figure <- function(multifunctionality_group, group, model_group, temp_col, prec_lt, prec_sh) {
  model_data <- model_group |>
    unnest(result) |>
    filter(effect == "fixed") |>
    filter(level == {{ group }}) |>
    select(-data, -model, -model_factorial, -model_treatment, -result_factorial, -result_treatment, -group, -effect, -anova, -anova_tidy)

  plot_data <- multifunctionality_group |> filter(group == {{ group }})

  sig <- get_significant_terms_group(model_data)

  # If nothing significant, return NULL
  if (!sig$fg && !sig$temp && !sig$precip) {
    return(NULL)
  }

  p <- ggplot(plot_data, aes(x = fg_richness, y = multifuntionality))

  if (sig$fg_temp_interaction || (sig$temp && !sig$precip)) {
    p <- p +
      geom_point(aes(colour = habitat), alpha = 0.5) +
      geom_smooth(aes(colour = habitat), method = "lm", se = TRUE, linewidth = 0.8) +
      scale_colour_manual(values = temp_col, name = "Habitat")
  } else if (sig$fg_precip_interaction || (sig$precip && !sig$temp)) {
    p <- p +
      geom_point(aes(shape = precipitation_name), alpha = 0.5) +
      geom_smooth(aes(linetype = precipitation_name), method = "lm", se = TRUE, linewidth = 0.8) +
      scale_linetype_manual(values = prec_lt, name = "Precipitation") +
      scale_shape_manual(values = prec_sh, name = "Precipitation")
  } else if (sig$temp && sig$precip) {
    p <- p +
      geom_point(aes(colour = habitat, shape = precipitation_name), alpha = 0.5) +
      geom_smooth(aes(colour = habitat, linetype = precipitation_name),
        method = "lm", se = FALSE, linewidth = 0.8
      ) +
      scale_colour_manual(values = temp_col, name = "Habitat") +
      scale_linetype_manual(values = prec_lt, name = "Precipitation") +
      scale_shape_manual(values = prec_sh, name = "Precipitation")
  } else {
    p <- p +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, colour = "black")
  }

  sig_terms_this <- model_data |>
    filter(p.value < 0.05) |>
    pull(term)
  sig_labels <- c()
  if (sig$fg) sig_labels <- c(sig_labels, "FG")
  if (sig$fg_temp_interaction) {
    sig_labels <- c(sig_labels, "FG x T")
  } else if ("temperature_scaled" %in% sig_terms_this) {
    sig_labels <- c(sig_labels, "T")
  }
  if (sig$fg_precip_interaction) {
    sig_labels <- c(sig_labels, "FG x P")
  } else if ("precipitation_scaled" %in% sig_terms_this) {
    sig_labels <- c(sig_labels, "P")
  }
  sig_text <- paste(sig_labels, collapse = "\n")

  p +
    scale_x_continuous(breaks = c(0, 1, 2, 3)) +
    labs(
      x = "Number of functional groups present",
      y = "Average multifunctionality",
      title = {{ group }}
    ) +
    annotate("text",
      x = Inf, y = Inf, label = sig_text,
      hjust = 1.1, vjust = 1.1, size = 3.5, fontface = "bold"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
}

# get_significant_terms for group-level model (no response column)
get_significant_terms_group <- function(model_data, alpha = 0.05) {
  sig_terms <- model_data |>
    filter(p.value < alpha) |>
    pull(term)
  list(
    fg = ".functional_group" %in% sig_terms,
    temp = any(c("temperature_scaled", ".functional_group:temperature_scaled") %in% sig_terms),
    precip = any(c("precipitation_scaled", ".functional_group:precipitation_scaled") %in% sig_terms),
    fg_temp_interaction = ".functional_group:temperature_scaled" %in% sig_terms,
    fg_precip_interaction = ".functional_group:precipitation_scaled" %in% sig_terms
  )
}


# Function to check which terms are significant
# model_data may have "response" (from model_response) or no response col (from model_group)
get_significant_terms <- function(response_name, model_data, alpha = 0.05) {
  if ("response" %in% names(model_data)) {
    sig_terms <- model_data |>
      filter(response == response_name, p.value < alpha) |>
      pull(term)
  } else {
    sig_terms <- model_data |>
      filter(p.value < alpha) |>
      pull(term)
  }

  list(
    fg = ".functional_group" %in% sig_terms,
    temp = any(c("temperature_scaled", ".functional_group:temperature_scaled") %in% sig_terms),
    precip = any(c("precipitation_scaled", ".functional_group:precipitation_scaled") %in% sig_terms),
    fg_temp_interaction = ".functional_group:temperature_scaled" %in% sig_terms,
    fg_precip_interaction = ".functional_group:precipitation_scaled" %in% sig_terms
  )
}

# Function to create plot based on significant terms
make_response_plot <- function(response_name, data, model_data, temp_col, prec_lt, prec_sh) {
  sig <- get_significant_terms(response_name, model_data)

  # If nothing significant, return NULL
  if (!sig$fg && !sig$temp && !sig$precip) {
    return(NULL)
  }

  # Filter data for this response
  plot_data <- data |> filter(response == response_name)

  # Base plot
  p <- ggplot(plot_data, aes(x = fg_richness, y = value_std))

  # Determine aesthetics based on significance - prioritize interactions
  if (sig$fg_temp_interaction || (sig$temp && !sig$precip)) {
    # FG:T interaction OR T only: use habitat (categorical temperature) for lines
    p <- p +
      geom_point(aes(colour = habitat), alpha = 0.5) +
      geom_smooth(aes(colour = habitat), method = "lm", se = TRUE, linewidth = 0.8) +
      scale_colour_manual(values = temp_col, name = "Habitat")
  } else if (sig$fg_precip_interaction || (sig$precip && !sig$temp)) {
    # FG:P interaction OR P only: linetype and shape for P
    p <- p +
      geom_point(aes(shape = precipitation_name), alpha = 0.5) +
      geom_smooth(aes(linetype = precipitation_name), method = "lm", se = TRUE, linewidth = 0.8) +
      scale_linetype_manual(values = prec_lt, name = "Precipitation") +
      scale_shape_manual(values = prec_sh, name = "Precipitation")
  } else if (sig$temp && sig$precip) {
    # Both T and P (no interactions): use habitat for color, linetype for P
    p <- p +
      geom_point(aes(colour = habitat, shape = precipitation_name), alpha = 0.5) +
      geom_smooth(aes(colour = habitat, linetype = precipitation_name),
        method = "lm", se = FALSE, linewidth = 0.8
      ) +
      scale_colour_manual(values = temp_col, name = "Habitat") +
      scale_linetype_manual(values = prec_lt, name = "Precipitation") +
      scale_shape_manual(values = prec_sh, name = "Precipitation")
  } else {
    # Only FG: simple plot
    p <- p +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, colour = "black")
  }

  # Build significance labels
  if ("response" %in% names(model_data)) {
    sig_terms_this_response <- model_data |>
      filter(response == response_name, p.value < 0.05) |>
      pull(term)
  } else {
    sig_terms_this_response <- model_data |>
      filter(p.value < 0.05) |>
      pull(term)
  }

  sig_labels <- c()
  if (sig$fg) sig_labels <- c(sig_labels, "FG")
  if (sig$fg_temp_interaction) {
    sig_labels <- c(sig_labels, "FG x T")
  } else if ("temperature_scaled" %in% sig_terms_this_response) {
    sig_labels <- c(sig_labels, "T")
  }
  if (sig$fg_precip_interaction) {
    sig_labels <- c(sig_labels, "FG x P")
  } else if ("precipitation_scaled" %in% sig_terms_this_response) {
    sig_labels <- c(sig_labels, "P")
  }

  sig_text <- paste(sig_labels, collapse = "\n")

  # Add common elements
  p <- p +
    scale_x_continuous(breaks = c(0, 1, 2, 3)) +
    labs(
      x = "Number of functional groups present",
      y = "Standardized function value",
      title = pretty_response_label(response_name)
    ) +
    annotate("text",
      x = Inf, y = Inf, label = sig_text,
      hjust = 1.1, vjust = 1.1, size = 3.5, fontface = "bold"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")

  return(p)
}


# Factorial multifunctionality plot (forb/gram/bryo presence vs predicted multifunctionality)
# climate_var: "temp" (habitat), "prec" (precipitation), or "combined" (prec colours, temp facets)
make_factorial_multifun_plot <- function(model_multifun, temp_colour, prec_colour, climate_var = c("temp", "prec", "combined")) {
  climate_var <- match.arg(climate_var)

  fact_dat <- model_multifun |>
    unnest(data) |>
    filter(treatment != "FGB") |>
    select(-model, -model_factorial, -model_treatment, -result, -result_factorial, -result_treatment, -anova, -anova_tidy)

  fact_fit <- model_multifun$model_factorial[[1]]
  fact_dat$pred <- predict(fact_fit, newdata = fact_dat, re.form = NA)

  fg_long <- fact_dat |>
    pivot_longer(
      cols = c(forb, gram, bryo),
      names_to = "fg",
      values_to = "present"
    ) |>
    mutate(precipitation_name = factor(precipitation_name, levels = c("700 mm", "1400 mm", "2100 mm", "2800 mm")))

  if (climate_var == "temp") {
    fg_means <- fg_long |>
      group_by(habitat, fg, present) |>
      summarise(mean_pred = mean(pred), .groups = "drop")

    ggplot(fg_long, aes(x = factor(present), y = pred)) +
      geom_line(
        data = fg_means,
        aes(x = factor(present), y = mean_pred, colour = habitat, group = interaction(habitat, fg)),
        linewidth = 1
      ) +
      geom_point(
        aes(colour = habitat),
        position = position_jitter(width = 0.1, height = 0),
        alpha = 0.6
      ) +
      scale_colour_manual(values = temp_colour, name = "Habitat") +
      facet_wrap(~fg) +
      labs(
        x = "Presence (0 = absent, 1 = present)",
        y = "Average multifunctionality",
        colour = "Habitat"
      ) +
      theme_bw()
  } else if (climate_var == "prec") {
    fg_means_prec <- fg_long |>
      group_by(precipitation_name, fg, present) |>
      summarise(mean_pred = mean(pred), .groups = "drop")

    ggplot(fg_long, aes(x = factor(present), y = pred)) +
      geom_line(
        data = fg_means_prec,
        aes(x = factor(present), y = mean_pred, colour = precipitation_name, group = interaction(precipitation_name, fg)),
        linewidth = 1
      ) +
      geom_point(
        aes(colour = precipitation_name),
        position = position_jitter(width = 0.1, height = 0),
        alpha = 0.6
      ) +
      scale_colour_manual(values = prec_colour, name = "Precipitation") +
      facet_wrap(~fg) +
      labs(
        x = "Presence (0 = absent, 1 = present)",
        y = "Average multifunctionality",
        colour = "Precipitation"
      ) +
      theme_bw()
  } else {
    # combined: colours for precipitation, facet by temperature (habitat) and fg
    fg_means_combined <- fg_long |>
      group_by(habitat, precipitation_name, fg, present) |>
      summarise(mean_pred = mean(pred), .groups = "drop")

    ggplot(fg_long, aes(x = factor(present), y = pred)) +
      geom_line(
        data = fg_means_combined,
        aes(x = factor(present), y = mean_pred, colour = precipitation_name, group = interaction(precipitation_name, fg)),
        linewidth = 1
      ) +
      geom_point(
        aes(colour = precipitation_name),
        position = position_jitter(width = 0.1, height = 0),
        alpha = 0.6
      ) +
      scale_colour_manual(values = prec_colour, name = "Precipitation") +
      facet_grid(habitat ~ fg) +
      labs(
        x = "Presence (0 = absent, 1 = present)",
        y = "Average multifunctionality",
        colour = "Precipitation"
      ) +
      theme_bw()
  }
}
