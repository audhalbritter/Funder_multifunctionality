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
          # - linear biomass with climate interactions: y ~ x * T * P
          # - log biomass with climate interactions:    y ~ log(x + shift) * T * P
          # - polynomial biomass with climate interactions (quadratic x): y ~ poly(x, 2) * T * P
          fit_linear <- stats::lm(y ~ x * t * p, data = dat_model)
          fit_log <- stats::lm(y ~ x_log * t * p, data = dat_model)
          fit_poly <- stats::lm(y ~ stats::poly(x, 2, raw = TRUE) * t * p, data = dat_model)

          tibble::tibble(
            response = response_name,
            predictor = pred_col,
            n = nrow(df_response),
            log_shift = log_shift,
            model_type = c("linear", "log", "poly"),
            aic = c(stats::AIC(fit_linear), stats::AIC(fit_log), stats::AIC(fit_poly))
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
      delta_aic_from_best = aic - best_aic
    )

  # Return both the complete AIC comparison and the selected model summary.
  list(
    aic_table = aic_table,
    selected_models = selected_models
  )
}
