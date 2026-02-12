# Variance partitioning for multifunctionality model using glmm.hp
#
# Goal: partition variance in multifunctionality into:
#   - FG, T, P alone (per-term contributions)
#   - FG vs Climate (T + P) as grouped blocks
#
# Uses the model from run_models (which uses colon notation for glmm.hp compatibility).
# Refits with explicit data so glmm.hp can access it (glmm.hp uses eval(mod@call$data)).

run_multifun_variance_partitioning <- function(model_multifun) {
  fit <- model_multifun$model[[1]]
  dat <- model_multifun$data[[1]]
  # Refit with explicit data so glmm.hp's eval(mod@call$data) works
  fit <- update(fit, data = dat)

  vp <- glmm.hp::glmm.hp(fit)

  # Summarise by FG vs Climate
  # FG terms: .functional_group (and all interactions with it)
  # Climate terms: temperature_scaled, precipitation_scaled, T:P (no FG)
  hp_table <- vp$hierarchical.partitioning |>
    as_tibble(rownames = "term")

  fg_terms <- hp_table$term[str_detect(hp_table$term, "functional_group")]
  climate_terms <- hp_table$term[str_detect(hp_table$term, "temperature_scaled|precipitation_scaled") &
                                  !str_detect(hp_table$term, "functional_group")]

  summary_fg_climate <- tibble(
    component = c("FG (functional groups)", "Climate (T + P)", "Total"),
    Individual = c(
      sum(hp_table$Individual[hp_table$term %in% fg_terms], na.rm = TRUE),
      sum(hp_table$Individual[hp_table$term %in% climate_terms], na.rm = TRUE),
      sum(hp_table$Individual, na.rm = TRUE)
    ),
    `I.perc(%)` = c(
      sum(hp_table[["Percentage"]][hp_table$term %in% fg_terms], na.rm = TRUE),
      sum(hp_table[["Percentage"]][hp_table$term %in% climate_terms], na.rm = TRUE),
      100
    )
  )

  list(
    model = fit,
    glmm_hp = vp,
    fg_terms = fg_terms,
    climate_terms = climate_terms,
    hp_table = hp_table,
    summary_fg_vs_climate = summary_fg_climate,
    marginal_r2 = vp$r.squaredGLMM
  )
}
