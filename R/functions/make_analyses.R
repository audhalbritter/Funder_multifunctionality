# make analyses

# run full model
run_models <- function(dat, response_var, fg_var, group){

  dat |>
    rename(.response = {{response_var}},
           .functional_group = {{fg_var}}) |>
    group_by(across(all_of({{group}}))) |>
    nest() |>
    mutate(
      # formula with colons (not *) so glmm.hp works on the same model
      model = map(data, ~ lmerTest::lmer(
        data = .,
        formula = .response ~ .functional_group + temperature_scaled + precipitation_scaled +
          .functional_group:temperature_scaled + .functional_group:precipitation_scaled +
          temperature_scaled:precipitation_scaled +
          .functional_group:temperature_scaled:precipitation_scaled +
          (1 | siteID)
      )),

      # factorial analysis
      model_factorial = map(data,
                            ~lmerTest::lmer(data = . |>
                                              filter(treatment != "FGB"),
                                            .response ~ forb * gram * bryo * temperature_scaled * precipitation_scaled + (1|siteID))),

      # treatment only model
      model_treatment = map(data, ~lmerTest::lmer(data = ., .response ~ treatment + (1|siteID))),

      # model output
      result = map(model, tidy),
      anova = map(model, car::Anova),
      anova_tidy = map(anova, tidy),

      result_factorial = map(model_factorial, tidy),
      result_treatment = map(model_treatment, tidy))

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

