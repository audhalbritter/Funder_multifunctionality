# make models including models without biomass, with biomass and with functional group biomass
make_model <- function(dat, response_var, fg_var, group){
  dat |>
    rename(.response = {{response_var}},
           .functional_group = {{fg_var}}) |>
    group_by(across(all_of({{group}}))) |>
    nest() |>
    mutate(
      # 1) No biomass: treatment × climate only
      model = map(
        data,
        ~ {
          safe_fit <- purrr::safely(function(df) {
            lmerTest::lmer(
              data = df,
              .response ~ .functional_group * (temperature_scaled + precipitation_scaled) + (1 | siteID)
            )
          })
          res <- safe_fit(.)
          if (!is.null(res$error)) {
            warning("Model (no biomass) failed for one group: ", conditionMessage(res$error))
          }
          res$result
        }
      ),
      # 2) Model 1 + total_biomass_scaled (z-scored in standing_biomass_sum)
      model_biomass = map(
        data,
        ~ {
          safe_fit <- purrr::safely(function(df) {
            lmerTest::lmer(
              data = df,
              .response ~ .functional_group * (temperature_scaled + precipitation_scaled) + total_biomass_scaled + (1 | siteID)
            )
          })
          res <- safe_fit(.)
          if (!is.null(res$error)) {
            warning("Model + total_biomass_scaled failed for one group: ", conditionMessage(res$error))
          }
          res$result
        }
      ),
      # 3) Functional group biomass × climate only (biomass_G/F/B; no treatment term)
      model_FG_biomass = map(
        data,
        ~ {
          safe_fit <- purrr::safely(function(df) {
            lmerTest::lmer(
              data = df,
              .response ~ (biomass_scaled_G + biomass_scaled_F + biomass_scaled_B) * (temperature_scaled + precipitation_scaled) + (1 | siteID)
            )
          })
          res <- safe_fit(.)
          if (!is.null(res$error)) {
            warning("FG biomass model failed for one group: ", conditionMessage(res$error))
          }
          res$result
        }
      ),
      # model output (NULL when model failed to converge or downstream fails)
      result = map(model, ~ purrr::safely(tidy)(.)$result),
      anova = map(model, ~ purrr::safely(car::Anova)(.)$result),
      anova_tidy = map(anova, ~ purrr::safely(tidy)(.)$result),
      # model 2 output (model 1 + total_biomass)
      result_biomass = map(model_biomass, ~ purrr::safely(tidy)(.)$result),
      anova_biomass = map(model_biomass, ~ purrr::safely(car::Anova)(.)$result),
      anova_tidy_biomass = map(anova_biomass, ~ purrr::safely(tidy)(.)$result),
      # model 3 output (model_FG_biomass: biomass_G/F/B × climate only)
      result_FG_biomass = map(model_FG_biomass, ~ purrr::safely(tidy)(.)$result),
      anova_FG_biomass = map(model_FG_biomass, ~ purrr::safely(car::Anova)(.)$result),
      anova_tidy_FG_biomass = map(anova_FG_biomass, ~ purrr::safely(tidy)(.)$result)
    )
}

# Model: FG identity × FG biomass × climate (long-format data from big_data_long)
# dat must have one row per FG per response per plot (from big_data |> left_join(standing_biomass))
# fg_id identifies which FG is present in each row; biomass_scaled is that FG's biomass.
# Baseline FG for contrasts = alphabetically first level of fg_id ("B").
make_model_fg_bio <- function(dat, response_var, group) {
  dat |>
    rename(.response = {{response_var}}) |>
    group_by(across(all_of({{group}}))) |>
    nest() |>
    mutate(
      model_fg_bio = map(
        data,
        ~ {
          safe_fit <- purrr::safely(function(df) {
            lmerTest::lmer(
              data = df,
              .response ~ fg_id * biomass_scaled * (temperature_scaled + precipitation_scaled) + (1 | siteID)
            )
          })
          res <- safe_fit(.)
          if (!is.null(res$error)) {
            warning("FG bio model failed: ", conditionMessage(res$error))
          }
          res$result
        }
      ),
      result_fg_bio     = map(model_fg_bio, ~ purrr::safely(tidy)(.)$result),
      anova_fg_bio      = map(model_fg_bio, ~ purrr::safely(car::Anova)(.)$result),
      anova_tidy_fg_bio = map(anova_fg_bio, ~ purrr::safely(tidy)(.)$result)
    )
}


# make prediction for functional group analysis
# expects dat with no NA in .response, .functional_group, temperature_scaled, precipitation_scaled
lmer_prediction <- function(dat, fit) {
  if (is.null(fit)) return(NULL)

  result <- tryCatch({
    newdat <- dat %>%
      select(.response, .functional_group, temperature_scaled, precipitation_scaled)

    newdat$fitted <- predict(fit, newdat, re.form = NA)

    # fixed-effects model matrix (matches run_models formula)
    form_fixed <- ~ .functional_group + temperature_scaled + precipitation_scaled +
      .functional_group:temperature_scaled + .functional_group:precipitation_scaled +
      temperature_scaled:precipitation_scaled +
      .functional_group:temperature_scaled:precipitation_scaled
    mm <- model.matrix(form_fixed, data = newdat)
    mm <- mm[, names(lme4::fixef(fit)), drop = FALSE]

    vc <- as.data.frame(VarCorr(fit))
    sigma2_re <- vc$vcov[1]

    newdat %>%
      mutate(pvar1 = diag(mm %*% tcrossprod(vcov(fit), mm)),
             tvar1 = pvar1 + sigma2_re,
             cmult = 1.96) %>%
      mutate(plo = fitted - cmult * sqrt(pvar1),
             phi = fitted + cmult * sqrt(pvar1),
             tlo = fitted - cmult * sqrt(tvar1),
             thi = fitted + cmult * sqrt(tvar1))
  }, error = function(e) {
    warning("lmer_prediction failed: ", conditionMessage(e))
    NULL
  })

  return(result)
}