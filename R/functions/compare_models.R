#' Statistical comparison of model variants
#'
#' @param model_tbl Tibble of models from pipeline
#' @param response_group_lookup Lookup for response groups
#' @return A list of summary tables for R2 and Estimate comparisons
calculate_model_comparison_stats <- function(model_tbl, response_group_lookup) {
  
  # --- R2 Comparisons ---
  r2_dat <- model_tbl |>
    mutate(
      mod1_m = purrr::map_dbl(r2, ~ r2_safe_get(.x, "R2_marginal")),
      mod1_c = purrr::map_dbl(r2, ~ r2_safe_get(.x, "R2_conditional")),
      mod2_m = purrr::map_dbl(r2_biomass, ~ r2_safe_get(.x, "R2_marginal")),
      mod2_c = purrr::map_dbl(r2_biomass, ~ r2_safe_get(.x, "R2_conditional")),
      mod3_m = purrr::map_dbl(r2_FG_biomass, ~ r2_safe_get(.x, "R2_marginal")),
      mod3_c = purrr::map_dbl(r2_FG_biomass, ~ r2_safe_get(.x, "R2_conditional"))
    )

  prep_r2_comp <- function(df, col_x_suffix, col_y_suffix, label) {
    bind_rows(
      df |> transmute(response, comparison = label, type = "marginal", x = .data[[paste0(col_x_suffix, "_m")]], y = .data[[paste0(col_y_suffix, "_m")]]),
      df |> transmute(response, comparison = label, type = "conditional", x = .data[[paste0(col_x_suffix, "_c")]], y = .data[[paste0(col_y_suffix, "_c")]])
    )
  }

  r2_comp_dat <- bind_rows(
    prep_r2_comp(r2_dat, "mod1", "mod2", "Mod 1 vs 2 (No vs Total Bio)"),
    prep_r2_comp(r2_dat, "mod1", "mod3", "Mod 1 vs 3 (No vs FG Bio)"),
    prep_r2_comp(r2_dat, "mod2", "mod3", "Mod 2 vs 3 (Total vs FG Bio)")
  ) |> filter(!is.na(x), !is.na(y))

  r2_stats <- r2_comp_dat |>
    group_by(comparison, type) |>
    group_modify(~ test_1to1_deviation(.x$x, .x$y)) |>
    ungroup()

  # --- Estimate Comparisons ---
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
  ) |> mutate(comparison = "Mod 1 vs 2 (No vs Total Bio)")

  comp13 <- inner_join(
    est1 |> mutate(key = map_term_to_comparison_key(term, "standard")) |> filter(!is.na(key)) |> rename(x = estimate),
    est3 |> mutate(key = map_term_to_comparison_key(term, "composition")) |> filter(!is.na(key)) |> rename(y = estimate),
    by = c("response", "key")
  ) |> mutate(comparison = "Mod 1 vs 3 (No vs FG Bio)", term = key)

  comp23 <- inner_join(
    est2 |> mutate(key = map_term_to_comparison_key(term, "standard")) |> filter(!is.na(key)) |> rename(x = estimate),
    est3 |> mutate(key = map_term_to_comparison_key(term, "composition")) |> filter(!is.na(key)) |> rename(y = estimate),
    by = c("response", "key")
  ) |> mutate(comparison = "Mod 2 vs 3 (Total vs FG Bio)", term = key)

  est_comp_dat <- bind_rows(comp12, comp13, comp23) |> filter(!is.na(x), !is.na(y))

  est_stats <- est_comp_dat |>
    group_by(comparison) |>
    group_modify(~ test_1to1_deviation(.x$x, .x$y)) |>
    ungroup()

  list(r2 = r2_stats, estimates = est_stats)
}

#' Statistical comparison of Interaction vs Main Effects
#'
#' @param model_int Tibble of interaction models
#' @param model_main Tibble of main effects models
#' @param response_group_lookup Lookup for response groups
#' @return A list of summary tables for R2 and Estimate comparisons
calculate_interaction_vs_main_stats <- function(model_int, model_main, response_group_lookup) {
  
  # --- R2 Comparisons ---
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

  r2_comp_dat <- bind_rows(
    prep_comp(r2_int, r2_main, 1, "Model 1 (Climate only)"),
    prep_comp(r2_int, r2_main, 2, "Model 2 (+Total Bio)"),
    prep_comp(r2_int, r2_main, 3, "Model 3 (FG Bio)")
  ) |> filter(!is.na(x), !is.na(y))

  r2_stats <- r2_comp_dat |>
    group_by(comparison, type) |>
    group_modify(~ test_1to1_deviation(.x$x, .x$y)) |>
    ungroup()

  # --- Estimate Comparisons ---
  extract_fixed <- function(tbl, col) {
    tbl |> select(response, all_of(col)) |> unnest(cols = all_of(col)) |> filter(effect == "fixed") |> select(response, term, estimate)
  }

  int1 <- extract_fixed(model_int, "result"); int2 <- extract_fixed(model_int, "result_biomass"); int3 <- extract_fixed(model_int, "result_FG_biomass")
  main1 <- extract_fixed(model_main, "result_main"); main2 <- extract_fixed(model_main, "result_main_biomass"); main3 <- extract_fixed(model_main, "result_main_FG_biomass")

  comp1 <- inner_join(main1 |> mutate(term = climate_term_short(term)) |> rename(x = estimate),
                      int1 |> mutate(term = climate_term_short(term)) |> rename(y = estimate), by = c("response", "term")) |> mutate(comparison = "Model 1 (Climate only)")
  
  comp2 <- inner_join(main2 |> mutate(term = climate_term_short(term)) |> rename(x = estimate),
                      int2 |> mutate(term = climate_term_short(term)) |> rename(y = estimate), by = c("response", "term")) |> mutate(comparison = "Model 2 (+Total Bio)")
  
  comp3 <- inner_join(main3 |> mutate(term = climate_term_short(term)) |> rename(x = estimate),
                      int3 |> mutate(term = climate_term_short(term)) |> rename(y = estimate), by = c("response", "term")) |> mutate(comparison = "Model 3 (FG Bio)")

  est_comp_dat <- bind_rows(comp1, comp2, comp3) |> filter(!is.na(x), !is.na(y))

  est_stats <- est_comp_dat |>
    group_by(comparison) |>
    group_modify(~ test_1to1_deviation(.x$x, .x$y)) |>
    ungroup()

  list(r2 = r2_stats, estimates = est_stats)
}

#' Internal helper: Test if paired observations deviate from 1:1 line
test_1to1_deviation <- function(x, y) {
  if(length(x) < 3) return(tibble(n = length(x), mean_diff = NA_real_, wilcox_p = NA_real_, slope = NA_real_, p_slope_1 = NA_real_))
  
  # Wilcoxon signed-rank test (paired)
  w_test <- tryCatch(wilcox.test(x, y, paired = TRUE), error = function(e) list(p.value = NA_real_))
  
  # Mean difference
  mean_diff <- mean(y - x, na.rm = TRUE)
  
  # Regression slope test (H0: b = 1)
  mod <- lm(y ~ x)
  b <- coef(mod)[2]
  se_b <- summary(mod)$coefficients[2,2]
  t_val <- (b - 1) / se_b
  p_slope_1 <- 2 * pt(abs(t_val), df = mod$df.residual, lower.tail = FALSE)
  
  tibble(
    n = length(x),
    mean_diff = mean_diff,
    wilcox_p = w_test$p.value,
    slope = b,
    p_slope_1 = p_slope_1
  )
}
