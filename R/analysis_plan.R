# data analysis plan
analysis_plan <- list(

  # analysis
  tar_target(
    name = model_response,
    command = big_data |>
      group_by(response) |>
      nest() |>
      mutate(model = map(data, ~lmerTest::lmer(data = ., value_std ~ fg_richness * temperature_degree_std * precipitation_mm_std + (1|siteID))),
             result = map(model, tidy))

  ),

  tar_target(
    name = model_response_out,
    command = model_response |>
      unnest(result) |>
      filter(effect == "fixed") |>
      select(-data, -model, -group, -effect) |>
      mutate(term = case_match(term,
                               "(Intercept)" ~ "Intercept",
                               "fg_richness" ~ "FG",
                               "temperature_degree_std" ~ "T",
                               "precipitation_mm_std" ~ "P",
                               "fg_richness:temperature_degree_std" ~ "FG:T",
                               "fg_richness:precipitation_mm_std" ~ "FG:P",
                               "temperature_degree_std:precipitation_mm_std" ~ "T:P",
                               "fg_richness:temperature_degree_std:precipitation_mm_std" ~ "FG:T:P"))

  )




)
