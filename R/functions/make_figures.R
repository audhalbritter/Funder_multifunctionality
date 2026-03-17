### make figures

# Build prediction lines from Model 3 at fixed climate (scaled T/P values),
# varying one biomass axis per panel while holding the other two at 0.
make_fg_biomass_prediction_lines <- function(data,
                                             response_name,
                                             model_fg_biomass_tbl,
                                             plot_dat,
                                             temp_value = 0,
                                             precip_value = 0,
                                             n_points = 100) {
  if (is.null(model_fg_biomass_tbl)) {
    return(NULL)
  }

  fit <- model_fg_biomass_tbl |>
    filter(response == response_name) |>
    pull(model_FG_biomass)
  fit <- if (length(fit) == 0) NULL else fit[[1]]
  if (is.null(fit)) {
    return(NULL)
  }

  x_ranges <- plot_dat |>
    group_by(fg_biomass) |>
    summarise(
      x_min = min(biomass_value, na.rm = TRUE),
      x_max = max(biomass_value, na.rm = TRUE),
      .groups = "drop"
    )

  site_ref <- data |>
    filter(response == response_name) |>
    pull(siteID)
  site_ref <- site_ref[!is.na(site_ref)][1]
  if (length(site_ref) == 0) {
    site_ref <- NA_character_
  }

  pred_lines <- x_ranges |>
    rowwise() |>
    do({
      fg_name <- .$fg_biomass
      x_seq <- seq(.$x_min, .$x_max, length.out = n_points)
      n_pts <- length(x_seq)

      newdat <- tibble(
        biomass_scaled_G = rep(0, n_pts),
        biomass_scaled_F = rep(0, n_pts),
        biomass_scaled_B = rep(0, n_pts),
        temperature_scaled = rep(temp_value, n_pts),
        precipitation_scaled = rep(precip_value, n_pts),
        siteID = rep(site_ref, n_pts)
      )

      if (fg_name == "Graminoid biomass") newdat$biomass_scaled_G <- x_seq
      if (fg_name == "Forb biomass") newdat$biomass_scaled_F <- x_seq
      if (fg_name == "Bryophyte biomass") newdat$biomass_scaled_B <- x_seq

      newdat$biomass_value <- x_seq
      newdat$fg_biomass <- fg_name
      newdat$pred <- as.numeric(predict(fit, newdata = newdat, re.form = NA))

      # 95% CI for fixed-effects prediction at specified climate values.
      fixed_form <- lme4::nobars(formula(fit))
      fixed_terms <- stats::delete.response(stats::terms(fixed_form))
      mm <- model.matrix(fixed_terms, data = newdat)
      mm <- mm[, names(lme4::fixef(fit)), drop = FALSE]
      vcv <- vcov(fit)
      se_fit <- sqrt(diag(mm %*% vcv %*% t(mm)))
      newdat$pred_lo <- newdat$pred - 1.96 * se_fit
      newdat$pred_hi <- newdat$pred + 1.96 * se_fit
      newdat
    }) |>
    ungroup()

  pred_lines
}

# Plot one response against biomass of each FG in 3 panels.
# Prediction lines come from Model 3 (biomass composition model) at fixed climate values.
plot_response_vs_fg_biomass <- function(data,
                                        response_name,
                                        model_fg_biomass_tbl = NULL,
                                        response_var = value_std,
                                        temp_value = 0,
                                        precip_value = 0) {
  plot_dat <- data |>
    filter(response == response_name) |>
    select(
      response,
      .response = {{response_var}},
      biomass_scaled_G,
      biomass_scaled_F,
      biomass_scaled_B
    ) |>
    pivot_longer(
      cols = c(biomass_scaled_G, biomass_scaled_F, biomass_scaled_B),
      names_to = "fg_biomass",
      values_to = "biomass_value"
    ) |>
    mutate(
      fg_biomass = recode(
        fg_biomass,
        biomass_scaled_G = "Graminoid biomass",
        biomass_scaled_F = "Forb biomass",
        biomass_scaled_B = "Bryophyte biomass"
      )
    )

  pred_lines <- make_fg_biomass_prediction_lines(
    data = data,
    response_name = response_name,
    model_fg_biomass_tbl = model_fg_biomass_tbl,
    plot_dat = plot_dat,
    temp_value = temp_value,
    precip_value = precip_value
  )

  p <- ggplot(plot_dat, aes(x = biomass_value, y = .response)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ fg_biomass, scales = "free_x") +
    labs(
      x = "Functional-group biomass (scaled)",
      y = "Standardized function value",
      title = response_name
    ) +
    theme_bw()

  if (!is.null(pred_lines)) {
    p <- p +
      geom_ribbon(
        data = pred_lines,
        aes(x = biomass_value, ymin = pred_lo, ymax = pred_hi),
        inherit.aes = FALSE,
        fill = "grey40",
        alpha = 0.2
      ) +
      geom_line(
        data = pred_lines,
        aes(x = biomass_value, y = pred),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.9
      )
  } else {
    # Fallback if model object is unavailable.
    p <- p + geom_smooth(method = "lm", se = TRUE, colour = "black", linewidth = 0.8)
  }

  p
}




# Compare fixed-effect estimates across single-function model variants.
# Plots climate-model estimates on x against estimates from biomass/FG-biomass models on y.
# colour_by can be "group" (response group) or "term" (model term).
plot_single_function_estimate_comparison <- function(model_single_function_tbl, response_group_lookup = NULL, colour_by = c("group", "term")) {
  colour_by <- match.arg(colour_by)

  extract_fixed <- function(tbl, model_col, label) {
    tbl |>
      select(response, all_of(model_col)) |>
      unnest(cols = all_of(model_col)) |>
      filter(effect == "fixed") |>
      select(response, term, estimate) |>
      mutate(model = label)
  }

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

  # Map climate-model FG terms to biomassFG-model terms so they are comparable.
  # Example: .functional_groupF <-> biomass_scaled_F and corresponding climate interactions.
  map_climate_to_fg_key <- function(term) {
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
  }

  map_biofg_to_fg_key <- function(term) {
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

  est_climate <- extract_fixed(model_single_function_tbl, "result", "climate")
  est_biomass <- extract_fixed(model_single_function_tbl, "result_biomass", "biomass")
  est_fg_biomass <- extract_fixed(model_single_function_tbl, "result_FG_biomass", "biomassFG")

  comp_biomass <- est_climate |>
    mutate(term = climate_term_short(term)) |>
    rename(estimate_climate = estimate) |>
    inner_join(
      est_biomass |>
        mutate(term = climate_term_short(term)) |>
        rename(estimate_other = estimate),
      by = c("response", "term")
    ) |>
    mutate(comparison = "Climate vs biomass")

  comp_fg <- est_climate |>
    mutate(term_key = map_climate_to_fg_key(term)) |>
    filter(!is.na(term_key)) |>
    rename(estimate_climate = estimate) |>
    inner_join(
      est_fg_biomass |>
        mutate(term_key = map_biofg_to_fg_key(term)) |>
        filter(!is.na(term_key)) |>
        rename(estimate_other = estimate),
      by = c("response", "term_key")
    ) |>
    transmute(
      response,
      term = case_when(
        term_key == "F" ~ "F",
        term_key == "G" ~ "G",
        term_key == "B" ~ "B",
        term_key == "F:temperature_scaled" ~ "F:T",
        term_key == "G:temperature_scaled" ~ "G:T",
        term_key == "B:temperature_scaled" ~ "B:T",
        term_key == "F:precipitation_scaled" ~ "F:P",
        term_key == "G:precipitation_scaled" ~ "G:P",
        term_key == "B:precipitation_scaled" ~ "B:P",
        TRUE ~ term_key
      ),
      estimate_climate,
      estimate_other,
      comparison = "Climate vs biomassFG"
    )

  plot_dat <- bind_rows(comp_biomass, comp_fg) |>
    select(response, term, comparison, estimate_climate, estimate_other) |>
    filter(!is.na(estimate_climate), !is.na(estimate_other))

  if (colour_by == "group") {
    if (is.null(response_group_lookup)) {
      stop("response_group_lookup is required when colour_by = 'group'")
    }
    plot_dat <- plot_dat |>
      left_join(
        response_group_lookup |>
          select(response, group) |>
          distinct(),
        by = "response"
      )
  }

  colour_name <- if (colour_by == "group") "Function group" else "Model term"

  ggplot(plot_dat, aes(x = estimate_climate, y = estimate_other, colour = .data[[colour_by]])) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    geom_point(alpha = 0.6) +
    facet_wrap(~ comparison) +
    scale_colour_discrete(name = colour_name, na.translate = FALSE) +
    labs(
      x = "Estimate (climate model)",
      y = "Estimate (comparison model)",
      title = "Fixed-effect estimate comparison for single-function models"
    ) +
    theme_bw()
}


# make_group_figure: one plot per group (4 total), raw multifunctionality + prediction lines,
# significance letters in upper right; FG-only = simple plot, FG:T = colour by habitat
make_group_figure <- function(multifunctionality_group, group, model_group, temp_col, prec_lt, prec_sh) {
  model_data <- model_group |>
    unnest(result) |>
    filter(effect == "fixed") |>
    filter(level == {{group}}) |>
    select(-data, -model, -model_factorial, -model_treatment, -result_factorial, -result_treatment, -group, -effect, -anova, -anova_tidy)

  plot_data <- multifunctionality_group |> filter(group == {{group}})

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
                  method = "lm", se = FALSE, linewidth = 0.8) +
      scale_colour_manual(values = temp_col, name = "Habitat") +
      scale_linetype_manual(values = prec_lt, name = "Precipitation") +
      scale_shape_manual(values = prec_sh, name = "Precipitation")
  } else {
    p <- p +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, colour = "black")
  }

  sig_terms_this <- model_data |> filter(p.value < 0.05) |> pull(term)
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
    labs(x = "Number of functional groups present",
         y = "Average multifunctionality",
         title = {{group}}) +
    annotate("text", x = Inf, y = Inf, label = sig_text,
             hjust = 1.1, vjust = 1.1, size = 3.5, fontface = "bold") +
    theme_bw() +
    theme(legend.position = "bottom")
}

# get_significant_terms for group-level model (no response column)
get_significant_terms_group <- function(model_data, alpha = 0.05) {
  sig_terms <- model_data |> filter(p.value < alpha) |> pull(term)
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
    sig_terms <- model_data |> filter(response == response_name, p.value < alpha) |> pull(term)
  } else {
    sig_terms <- model_data |> filter(p.value < alpha) |> pull(term)
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
                  method = "lm", se = FALSE, linewidth = 0.8) +
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
    sig_terms_this_response <- model_data |> filter(response == response_name, p.value < 0.05) |> pull(term)
  } else {
    sig_terms_this_response <- model_data |> filter(p.value < 0.05) |> pull(term)
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
    labs(x = "Number of functional groups present",
         y = "Standardized function value",
         title = response_name) +
    annotate("text", x = Inf, y = Inf, label = sig_text, 
             hjust = 1.1, vjust = 1.1, size = 3.5, fontface = "bold") +
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
      facet_wrap(~ fg) +
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
      facet_wrap(~ fg) +
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
