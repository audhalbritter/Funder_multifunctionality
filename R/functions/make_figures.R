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


# Function to check which terms are significant
get_significant_terms <- function(response_name, model_data, alpha = 0.05) {
  sig_terms <- model_data |>
    filter(response == response_name, p.value < alpha) |>
    pull(term)
  
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
  sig_terms_this_response <- model_data |> 
    filter(response == response_name, p.value < 0.05) |> 
    pull(term)
  
  sig_labels <- c()
  if (sig$fg) sig_labels <- c(sig_labels, "FR")
  if (sig$fg_temp_interaction) {
    sig_labels <- c(sig_labels, "FR x T")
  } else if ("temperature_scaled" %in% sig_terms_this_response) {
    sig_labels <- c(sig_labels, "T")
  }
  if (sig$fg_precip_interaction) {
    sig_labels <- c(sig_labels, "FR x P")
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
    annotate("text", x = -Inf, y = Inf, label = sig_text, 
             hjust = -0.1, vjust = 1.1, size = 3.5, fontface = "bold") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  return(p)
}


# Factorial multifunctionality plot (forb/gram/bryo presence vs predicted multifunctionality)
# climate_var: "temp" (habitat) or "prec" (precipitation)
make_factorial_multifun_plot <- function(model_multifun, temp_colour, prec_colour, climate_var = c("temp", "prec")) {
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
        y = "Predicted multifunctionality",
        colour = "Habitat"
      ) +
      theme_bw()
  } else {
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
        y = "Predicted multifunctionality",
        colour = "Precipitation"
      ) +
      theme_bw()
  }
}
