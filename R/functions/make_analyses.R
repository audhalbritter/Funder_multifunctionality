# make models including models without biomass, with biomass and with functional group biomass
make_model <- function(dat, response_var, fg_var, group) {
  dat |>
    rename(
      .response = {{ response_var }},
      .functional_group = {{ fg_var }}
    ) |>
    group_by(across(all_of({{ group }}))) |>
    nest() |>
    mutate(
      # 1) Factorial model without biomass: treatment × climate only
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
      # marginal / conditional R2 for model 1
      r2 = map(
        model,
        ~ {
          if (is.null(.x)) {
            return(NULL)
          }
          r2_res <- purrr::safely(performance::r2)(.x)
          if (!is.null(r2_res$error) || is.null(r2_res$result)) {
            return(NULL)
          }
          as_tibble(r2_res$result)
        }
      ),
      # model 2 output (model 1 + total_biomass)
      result_biomass = map(model_biomass, ~ purrr::safely(tidy)(.)$result),
      anova_biomass = map(model_biomass, ~ purrr::safely(car::Anova)(.)$result),
      anova_tidy_biomass = map(anova_biomass, ~ purrr::safely(tidy)(.)$result),
      r2_biomass = map(
        model_biomass,
        ~ {
          if (is.null(.x)) {
            return(NULL)
          }
          r2_res <- purrr::safely(performance::r2)(.x)
          if (!is.null(r2_res$error) || is.null(r2_res$result)) {
            return(NULL)
          }
          as_tibble(r2_res$result)
        }
      ),
      # model 3 output (model_FG_biomass: biomass_G/F/B × climate only)
      result_FG_biomass = map(model_FG_biomass, ~ purrr::safely(tidy)(.)$result),
      anova_FG_biomass = map(model_FG_biomass, ~ purrr::safely(car::Anova)(.)$result),
      anova_tidy_FG_biomass = map(anova_FG_biomass, ~ purrr::safely(tidy)(.)$result),
      r2_FG_biomass = map(
        model_FG_biomass,
        ~ {
          if (is.null(.x)) {
            return(NULL)
          }
          r2_res <- purrr::safely(performance::r2)(.x)
          if (!is.null(r2_res$error) || is.null(r2_res$result)) {
            return(NULL)
          }
          as_tibble(r2_res$result)
        }
      ),
      # AIC for model comparison
      aic = map_dbl(model, ~ if (is.null(.x)) NA_real_ else AIC(.x)),
      aic_biomass = map_dbl(model_biomass, ~ if (is.null(.x)) NA_real_ else AIC(.x)),
      aic_FG_biomass = map_dbl(model_FG_biomass, ~ if (is.null(.x)) NA_real_ else AIC(.x))
    )
}

# make simpler models: main effects of FG/biomass and climate, plus T:P interaction (no biomass interactions)
make_main_effects_model <- function(dat, response_var, fg_var, group) {
  dat |>
    rename(
      .response = {{ response_var }},
      .functional_group = {{ fg_var }}
    ) |>
    group_by(across(all_of({{ group }}))) |>
    nest() |>
    mutate(
      # 1) No biomass: FG main effect + climate mains + T:P
      model_main = map(
        data,
        ~ {
          safe_fit <- purrr::safely(function(df) {
            lmerTest::lmer(
              data = df,
              .response ~ .functional_group + temperature_scaled + precipitation_scaled +
                temperature_scaled:precipitation_scaled + (1 | siteID)
            )
          })
          res <- safe_fit(.)
          if (!is.null(res$error)) {
            warning("Main-effects model (no biomass) failed for one group: ", conditionMessage(res$error))
          }
          res$result
        }
      ),
      # 2) Add total_biomass_scaled as a main effect (still only T:P interaction)
      model_main_biomass = map(
        data,
        ~ {
          safe_fit <- purrr::safely(function(df) {
            lmerTest::lmer(
              data = df,
              .response ~ .functional_group + temperature_scaled + precipitation_scaled +
                temperature_scaled:precipitation_scaled + total_biomass_scaled + (1 | siteID)
            )
          })
          res <- safe_fit(.)
          if (!is.null(res$error)) {
            warning("Main-effects model + total_biomass_scaled failed for one group: ", conditionMessage(res$error))
          }
          res$result
        }
      ),
      # 3) Biomass composition only: biomass_G/F/B mains + climate mains + T:P (no biomass interactions)
      model_main_FG_biomass = map(
        data,
        ~ {
          safe_fit <- purrr::safely(function(df) {
            lmerTest::lmer(
              data = df,
              .response ~ biomass_scaled_G + biomass_scaled_F + biomass_scaled_B +
                temperature_scaled + precipitation_scaled +
                temperature_scaled:precipitation_scaled + (1 | siteID)
            )
          })
          res <- safe_fit(.)
          if (!is.null(res$error)) {
            warning("Main-effects FG biomass model failed for one group: ", conditionMessage(res$error))
          }
          res$result
        }
      ),
      # tidy/anova and R2 for each variant
      result_main = map(model_main, ~ purrr::safely(tidy)(.)$result),
      anova_main = map(model_main, ~ purrr::safely(car::Anova)(.)$result),
      anova_tidy_main = map(anova_main, ~ purrr::safely(tidy)(.)$result),
      r2_main = map(
        model_main,
        ~ {
          if (is.null(.x)) {
            return(NULL)
          }
          r2_res <- purrr::safely(performance::r2)(.x)
          if (!is.null(r2_res$error) || is.null(r2_res$result)) {
            return(NULL)
          }
          as_tibble(r2_res$result)
        }
      ),
      result_main_biomass = map(model_main_biomass, ~ purrr::safely(tidy)(.)$result),
      anova_main_biomass = map(model_main_biomass, ~ purrr::safely(car::Anova)(.)$result),
      anova_tidy_main_biomass = map(anova_main_biomass, ~ purrr::safely(tidy)(.)$result),
      r2_main_biomass = map(
        model_main_biomass,
        ~ {
          if (is.null(.x)) {
            return(NULL)
          }
          r2_res <- purrr::safely(performance::r2)(.x)
          if (!is.null(r2_res$error) || is.null(r2_res$result)) {
            return(NULL)
          }
          as_tibble(r2_res$result)
        }
      ),
      result_main_FG_biomass = map(model_main_FG_biomass, ~ purrr::safely(tidy)(.)$result),
      anova_main_FG_biomass = map(model_main_FG_biomass, ~ purrr::safely(car::Anova)(.)$result),
      anova_tidy_main_FG_biomass = map(anova_main_FG_biomass, ~ purrr::safely(tidy)(.)$result),
      r2_main_FG_biomass = map(
        model_main_FG_biomass,
        ~ {
          if (is.null(.x)) {
            return(NULL)
          }
          r2_res <- purrr::safely(performance::r2)(.x)
          if (!is.null(r2_res$error) || is.null(r2_res$result)) {
            return(NULL)
          }
          as_tibble(r2_res$result)
        }
      ),
      # AIC for main effects model comparison
      aic_main = map_dbl(model_main, ~ if (is.null(.x)) NA_real_ else AIC(.x)),
      aic_main_biomass = map_dbl(model_main_biomass, ~ if (is.null(.x)) NA_real_ else AIC(.x)),
      aic_main_FG_biomass = map_dbl(model_main_FG_biomass, ~ if (is.null(.x)) NA_real_ else AIC(.x))
    )
}

# Post hoc pairwise tests: do BioG, BioF, BioB slopes differ in Model 3?
# Slopes are evaluated at fixed climate values (default: scaled T = 0, P = 0).
compare_fg_biomass_slopes <- function(fit, temp_value = 0, precip_value = 0, adjust_method = "holm") {
  if (is.null(fit)) {
    return(tibble())
  }

  coefs <- lme4::fixef(fit)
  vcv <- as.matrix(vcov(fit))

  # Fetch coefficient by exact name; also try reversed interaction order.
  get_coef <- function(name_a, name_b = NULL) {
    if (is.null(name_b)) {
      return(if (name_a %in% names(coefs)) coefs[[name_a]] else 0)
    }
    name_1 <- paste0(name_a, ":", name_b)
    name_2 <- paste0(name_b, ":", name_a)
    if (name_1 %in% names(coefs)) {
      return(coefs[[name_1]])
    }
    if (name_2 %in% names(coefs)) {
      return(coefs[[name_2]])
    }
    0
  }

  # Build contrast vectors c so diff = c' * beta.
  mk_contrast <- function(fg_pos, fg_neg) {
    cvec <- rep(0, length(coefs))
    names(cvec) <- names(coefs)

    add_terms <- function(prefix, sign) {
      main <- prefix
      int_t_1 <- paste0(prefix, ":temperature_scaled")
      int_t_2 <- paste0("temperature_scaled:", prefix)
      int_p_1 <- paste0(prefix, ":precipitation_scaled")
      int_p_2 <- paste0("precipitation_scaled:", prefix)

      if (main %in% names(cvec)) cvec[main] <<- cvec[main] + sign
      if (int_t_1 %in% names(cvec)) cvec[int_t_1] <<- cvec[int_t_1] + sign * temp_value
      if (int_t_2 %in% names(cvec)) cvec[int_t_2] <<- cvec[int_t_2] + sign * temp_value
      if (int_p_1 %in% names(cvec)) cvec[int_p_1] <<- cvec[int_p_1] + sign * precip_value
      if (int_p_2 %in% names(cvec)) cvec[int_p_2] <<- cvec[int_p_2] + sign * precip_value
    }

    add_terms(fg_pos, +1)
    add_terms(fg_neg, -1)
    cvec
  }

  contrasts <- list(
    "F-G" = mk_contrast("biomass_scaled_F", "biomass_scaled_G"),
    "F-B" = mk_contrast("biomass_scaled_F", "biomass_scaled_B"),
    "G-B" = mk_contrast("biomass_scaled_G", "biomass_scaled_B")
  )

  test_out <- purrr::imap_dfr(contrasts, function(cvec, nm) {
    est <- as.numeric(cvec %*% coefs)
    se <- sqrt(as.numeric(cvec %*% vcv %*% cvec))
    z_val <- est / se
    p_val <- 2 * stats::pnorm(abs(z_val), lower.tail = FALSE)
    tibble(
      contrast = nm,
      estimate_diff = est,
      std_error = se,
      z_value = z_val,
      p_value = p_val
    )
  }) |>
    mutate(
      p_value_adjusted = p.adjust(p_value, method = adjust_method)
    )

  test_out
}

# make prediction for functional group analysis
# expects dat with no NA in .response, .functional_group, temperature_scaled, precipitation_scaled
lmer_prediction <- function(dat, fit) {
  if (is.null(fit)) {
    return(NULL)
  }

  result <- tryCatch(
    {
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
        mutate(
          pvar1 = diag(mm %*% tcrossprod(vcov(fit), mm)),
          tvar1 = pvar1 + sigma2_re,
          cmult = 1.96
        ) %>%
        mutate(
          plo = fitted - cmult * sqrt(pvar1),
          phi = fitted + cmult * sqrt(pvar1),
          tlo = fitted - cmult * sqrt(tvar1),
          thi = fitted + cmult * sqrt(tvar1)
        )
    },
    error = function(e) {
      warning("lmer_prediction failed: ", conditionMessage(e))
      NULL
    }
  )

  return(result)
}
