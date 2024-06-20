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



