# Additional analyses for SI: response ~ functional-group biomass

utils::globalVariables(
  c(
    "response",
    "response_value",
    "predictor",
    "predictor_value",
    "aic",
    "best_aic",
    "delta_aic",
    "model_type",
    "model_rank",
    "n",
    "log_shift"
  )
)

run_fg_biomass_model_selection <- function(
    dat,
    response_col = "value_std",
    predictors = c("biomass_scaled_F", "biomass_scaled_G", "biomass_scaled_B"),
    temp_col = "temperature_scaled",
    precip_col = "precipitation_scaled",
    min_n = 10
) {
  # Basic checks: response, climate and predictor columns must exist in input data.
  if (!response_col %in% names(dat)) {
    stop("response_col not found in dat: ", response_col)
  }

  if (!temp_col %in% names(dat)) {
    stop("temp_col not found in dat: ", temp_col)
  }

  if (!precip_col %in% names(dat)) {
    stop("precip_col not found in dat: ", precip_col)
  }

  missing_predictors <- setdiff(predictors, names(dat))
  if (length(missing_predictors) > 0) {
    stop("Missing predictor columns in dat: ", paste(missing_predictors, collapse = ", "))
  }

  # Keep only columns needed for this analysis and standardize names used in formulas.
  dat_prepared <- dat |>
    dplyr::select(dplyr::all_of(c("response", response_col, temp_col, precip_col, predictors))) |>
    dplyr::rename(
      response_value = dplyr::all_of(response_col),
      temperature_value = dplyr::all_of(temp_col),
      precipitation_value = dplyr::all_of(precip_col)
    )

  # Tie-breaker preference when competing models have similar AIC (delta <= 2).
  model_preference <- c("linear", "log", "poly")

  # Fit 3 candidate models per response x predictor and collect AIC values.
  aic_table <- predictors |>
    purrr::map_dfr(function(pred_col) {
      # Build a temporary long-format table for the current predictor.
      dat_pred <- dat_prepared[, c("response", "response_value", "temperature_value", "precipitation_value", pred_col)]
      names(dat_pred)[names(dat_pred) == pred_col] <- "predictor_value"

      # Remove incomplete rows before model fitting.
      dat_pred <- dat_pred |>
        dplyr::filter(
          !is.na(response_value),
          !is.na(predictor_value),
          !is.na(temperature_value),
          !is.na(precipitation_value)
        )

      # Fit separately for each response variable.
      split(dat_pred, dat_pred$response) |>
        purrr::imap_dfr(function(df_response, response_name) {
          # Skip underpowered cases or near-constant predictors.
          if (nrow(df_response) < min_n || dplyr::n_distinct(df_response$predictor_value) < 3) {
            return(tibble::tibble())
          }

          # Log model uses a response-specific shift so predictor + shift > 0.
          log_shift <- abs(min(df_response$predictor_value, na.rm = TRUE)) + 1
          dat_model <- df_response |>
            dplyr::mutate(
              x = predictor_value,
              y = response_value,
              t = temperature_value,
              p = precipitation_value,
              x_log = log(predictor_value + log_shift)
            )

          # Candidate models:
          # - linear biomass with climate interactions: y ~ x * (t + p)
          # - log biomass with climate interactions:    y ~ log(x + shift) * (t + p)
          # - polynomial biomass with climate interactions (quadratic x): y ~ poly(x, 2) * (t + p)
          fit_linear <- stats::lm(y ~ x * (t + p), data = dat_model)
          fit_log <- stats::lm(y ~ x_log * (t + p), data = dat_model)
          fit_poly <- stats::lm(y ~ stats::poly(x, 2, raw = TRUE) * (t + p), data = dat_model)

          tibble::tibble(
            response = response_name,
            predictor = pred_col,
            n = nrow(df_response),
            log_shift = log_shift,
            model_type = c("linear", "log", "poly"),
            aic = c(stats::AIC(fit_linear), stats::AIC(fit_log), stats::AIC(fit_poly)),
            fit = list(fit_linear, fit_log, fit_poly)
          )
        })
    })

  # Model selection rule:
  # 1) Keep models within 2 AIC units of the best model for each response/predictor.
  # 2) If multiple remain, choose by simplicity preference: linear > log > poly.
  selected_models <- aic_table |>
    dplyr::group_by(response, predictor) |>
    dplyr::mutate(
      best_aic = min(aic),
      delta_aic = aic - best_aic
    ) |>
    dplyr::filter(delta_aic <= 2) |>
    dplyr::mutate(
      model_rank = match(model_type, model_preference)
    ) |>
    dplyr::slice_min(order_by = model_rank, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      response,
      predictor,
      n,
      log_shift,
      selected_model = model_type,
      selected_aic = aic,
      best_aic,
      delta_aic_from_best = aic - best_aic,
      fit
    )

  # Return both the complete AIC comparison and the selected model summary.
  list(
    aic_table = aic_table,
    selected_models = selected_models
  )
}

#' Plot selected SI functional group biomass models
#' Generates 3-panel plots (F, G, B) for each response based on selected models.
#' Only lines with significant predictors (Temperature or Precipitation) are drawn.
plot_si_fg_biomass <- function(dat, selected_models, temp_colour, prec_colour) {
  
  # Predictor labels for facets
  pred_labels <- c("biomass_scaled_F" = "Forbs", "biomass_scaled_G" = "Graminoids", "biomass_scaled_B" = "Bryophytes")
  
  # Levels mapping for predictions (using true categorical variables, matching data)
  t_levels <- dat |> dplyr::group_by(habitat) |> dplyr::summarize(val = mean(temperature_scaled, na.rm=TRUE), .groups = "drop") |> tidyr::drop_na()
  p_levels <- dat |> dplyr::group_by(precipitation_name) |> dplyr::summarize(val = mean(precipitation_scaled, na.rm=TRUE), .groups = "drop") |> tidyr::drop_na()
  
  responses <- unique(selected_models$response)
  plots <- list()
  
  for (resp in responses) {
    plots[[resp]] <- list()
    sub_models <- selected_models |> dplyr::filter(response == resp)
    
    # Store predictions and raw data across predictors for the 3 panels
    pred_data_temp <- list()
    pred_data_prec <- list()
    pred_data_main <- list()
    raw_data <- list()
    sig_data <- list()
    
    any_temp_sig <- FALSE
    any_prec_sig <- FALSE
    any_main_sig <- FALSE
    
    for (i in seq_len(nrow(sub_models))) {
      row <- sub_models[i, ]
      pred_col <- row$predictor
      mod_type <- row$selected_model
      shift <- row$log_shift
      pred_label <- pred_labels[[pred_col]]
      
      dat_pred <- dat |> 
        dplyr::filter(!is.na(value_std), !is.na(.data[[pred_col]]), !is.na(temperature_scaled), !is.na(precipitation_scaled), response == resp) |>
        dplyr::mutate(
          x = .data[[pred_col]],
          y = value_std,
          t = temperature_scaled,
          p = precipitation_scaled,
          x_log = log(x + shift)
        )
      
      raw_data[[pred_col]] <- dat_pred |> 
        dplyr::select(x, y, habitat, precipitation_name) |>
        dplyr::mutate(predictor_label = pred_label)
      
      fit <- row$fit[[1]]
      
      anv <- car::Anova(fit, type = 2)
      terms <- rownames(anv)
      pvals <- anv$`Pr(>F)`
      
      # Determine significance using exact term boundary matches
      t_sig <- any(pvals[grepl("(^|:)t(:|$)", terms)] < 0.05, na.rm = TRUE)
      p_sig <- any(pvals[grepl("(^|:)p(:|$)", terms)] < 0.05, na.rm = TRUE)
      x_sig <- any(pvals[grepl("(^|:)x(:|$)|x_log|poly", terms)] < 0.05, na.rm = TRUE)
      
      sig_terms <- terms[!is.na(pvals) & pvals < 0.05]
      friendly_terms <- sig_terms
      friendly_terms <- gsub("stats::", "", friendly_terms)
      friendly_terms <- gsub("poly\\(x[^)]*\\)", "B", friendly_terms)
      friendly_terms <- gsub("x_log", "B", friendly_terms)
      friendly_terms <- gsub("\\bx\\b", "B", friendly_terms)
      friendly_terms <- gsub("\\bt\\b", "T", friendly_terms)
      friendly_terms <- gsub("\\bp\\b", "P", friendly_terms)
      friendly_terms <- gsub(":", "x", friendly_terms)
      label_text <- paste(unique(friendly_terms), collapse = "\n")
      
      sig_data[[pred_col]] <- data.frame(
        predictor_label = pred_label,
        label_text = label_text,
        stringsAsFactors = FALSE
      )
      
      if (t_sig) any_temp_sig <- TRUE
      if (p_sig) any_prec_sig <- TRUE
      if (x_sig && !t_sig && !p_sig) any_main_sig <- TRUE
      
      # Predictions
      x_seq <- seq(min(dat_pred$x, na.rm=TRUE), max(dat_pred$x, na.rm=TRUE), length.out = 50)
      
      grid_temp <- expand.grid(x = x_seq, t = t_levels$val, p = 0)
      if (mod_type == "log") grid_temp$x_log <- log(grid_temp$x + shift)
      grid_temp$pred <- stats::predict(fit, newdata = grid_temp)
      grid_temp <- grid_temp |> dplyr::left_join(t_levels, by = c("t" = "val")) |> dplyr::mutate(predictor_label = pred_label, is_sig = t_sig)
      pred_data_temp[[pred_col]] <- grid_temp
      
      grid_prec <- expand.grid(x = x_seq, p = p_levels$val, t = 0)
      if (mod_type == "log") grid_prec$x_log <- log(grid_prec$x + shift)
      grid_prec$pred <- stats::predict(fit, newdata = grid_prec)
      grid_prec <- grid_prec |> dplyr::left_join(p_levels, by = c("p" = "val")) |> dplyr::mutate(predictor_label = pred_label, is_sig = p_sig)
      pred_data_prec[[pred_col]] <- grid_prec
      
      grid_main <- expand.grid(x = x_seq, t = 0, p = 0)
      if (mod_type == "log") grid_main$x_log <- log(grid_main$x + shift)
      grid_main$pred <- stats::predict(fit, newdata = grid_main)
      grid_main <- grid_main |> dplyr::mutate(predictor_label = pred_label, is_sig = x_sig)
      pred_data_main[[pred_col]] <- grid_main
    }
    
    raw_df <- dplyr::bind_rows(raw_data) |> dplyr::mutate(predictor_label = factor(predictor_label, levels = c("Forbs", "Graminoids", "Bryophytes")))
    ptemp <- dplyr::bind_rows(pred_data_temp) |> dplyr::mutate(predictor_label = factor(predictor_label, levels = c("Forbs", "Graminoids", "Bryophytes")))
    pprec <- dplyr::bind_rows(pred_data_prec) |> dplyr::mutate(predictor_label = factor(predictor_label, levels = c("Forbs", "Graminoids", "Bryophytes")))
    pmain <- dplyr::bind_rows(pred_data_main) |> dplyr::mutate(predictor_label = factor(predictor_label, levels = c("Forbs", "Graminoids", "Bryophytes")))
    sig_df <- dplyr::bind_rows(sig_data) |> dplyr::mutate(predictor_label = factor(predictor_label, levels = c("Forbs", "Graminoids", "Bryophytes")))
    
    # Generate temperature plot
    if (any_temp_sig) {
      p <- ggplot2::ggplot() +
        ggplot2::geom_point(data = raw_df, ggplot2::aes(x = x, y = y, color = habitat), alpha = 0.4)
      if (any(ptemp$is_sig)) {
        p <- p + ggplot2::geom_line(data = ptemp |> dplyr::filter(is_sig), ggplot2::aes(x = x, y = pred, color = habitat), linewidth = 1)
      }
      p <- p + 
        ggplot2::geom_text(data = sig_df, ggplot2::aes(label = label_text), x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, size = 3, fontface = "bold", inherit.aes = FALSE) +
        ggplot2::facet_grid(~ predictor_label) +
        ggplot2::scale_color_manual(values = temp_colour) +
        ggplot2::labs(x = "Biomass (scaled)", y = resp, title = paste0(resp, " - Temperature effect"), color = "Temperature") +
        ggplot2::theme_bw()
      plots[[resp]]$temperature_plot <- p
    }
    
    # Generate precipitation plot
    if (any_prec_sig) {
      p <- ggplot2::ggplot() +
        ggplot2::geom_point(data = raw_df, ggplot2::aes(x = x, y = y, color = precipitation_name), alpha = 0.4)
      if (any(pprec$is_sig)) {
        p <- p + ggplot2::geom_line(data = pprec |> dplyr::filter(is_sig), ggplot2::aes(x = x, y = pred, color = precipitation_name), linewidth = 1)
      }
      p <- p + 
        ggplot2::geom_text(data = sig_df, ggplot2::aes(label = label_text), x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, size = 3, fontface = "bold", inherit.aes = FALSE) +
        ggplot2::facet_grid(~ predictor_label) +
        ggplot2::scale_color_manual(values = prec_colour) +
        ggplot2::labs(x = "Biomass (scaled)", y = resp, title = paste0(resp, " - Precipitation effect"), color = "Precipitation") +
        ggplot2::theme_bw()
      plots[[resp]]$precipitation_plot <- p
    }
    
    # Generate main plot if no T/P but X is sig, OR nothing is sig
    if (!any_temp_sig && !any_prec_sig) {
      p <- ggplot2::ggplot() +
        ggplot2::geom_point(data = raw_df, ggplot2::aes(x = x, y = y), alpha = 0.4, color = "grey50")
      if (any_main_sig && any(pmain$is_sig)) {
        p <- p + ggplot2::geom_line(data = pmain |> dplyr::filter(is_sig), ggplot2::aes(x = x, y = pred), linewidth = 1, color = "black")
      }
      p <- p + 
        ggplot2::geom_text(data = sig_df, ggplot2::aes(label = label_text), x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, size = 3, fontface = "bold", inherit.aes = FALSE) +
        ggplot2::facet_grid(~ predictor_label) +
        ggplot2::labs(x = "Biomass (scaled)", y = resp, title = paste0(resp, " - Biomass effect")) +
        ggplot2::theme_bw()
      plots[[resp]]$main_plot <- p
    }
  }
  return(plots)
}
