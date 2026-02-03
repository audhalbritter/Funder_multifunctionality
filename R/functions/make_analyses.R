# make analyses

# run full model
run_models <- function(dat, response_var, fg_var, group){

  dat |>
    rename(.response = {{response_var}},
           .functional_group = {{fg_var}}) |>
    group_by(across(all_of({{group}})))|>
    nest() |>
    mutate(
      # nr of functional groups analysis
      model = map(data, ~lmerTest::lmer(data = ., .response ~ .functional_group * temperature_scaled * precipitation_scaled + (1|siteID))),

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
# works with lme4/lmerTest merMod objects; returns NULL on error
lmer_prediction <- function(dat, fit) {
  if (is.null(fit)) return(NULL)

  result <- tryCatch({
    # include siteID so formula(fit) can be evaluated (model has (1|siteID))
    newdat <- dat %>%
      select(.response, .functional_group, temperature_scaled, precipitation_scaled, any_of("siteID"))

    newdat$fitted <- predict(fit, newdat, re.form = NA)

    # model matrix for new data (formula evaluation needs siteID in data)
    mm <- model.matrix(formula(fit), data = newdat)

    # random-effect variance (first grouping factor, e.g. siteID)
    vc <- as.data.frame(VarCorr(fit))
    sigma2_re <- vc$vcov[1]

    prediction <- newdat %>%
      mutate(pvar1 = diag(mm %*% tcrossprod(vcov(fit), mm)),
             tvar1 = pvar1 + sigma2_re,
             cmult = 1.96) %>%
      mutate(plo = fitted - cmult * sqrt(pvar1),
             phi = fitted + cmult * sqrt(pvar1),
             tlo = fitted - cmult * sqrt(tvar1),
             thi = fitted + cmult * sqrt(tvar1))

    prediction
  }, error = function(e) {
    warning("lmer_prediction failed: ", conditionMessage(e))
    NULL
  })

  return(result)
}

