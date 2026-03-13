# data analysis plan
analysis_plan <- list(

  # check models (turn this off when still playing with models!)
  # tar_quarto(name = model_check,
  #            path = "R/model_checking.qmd")


  #### VARIANCE PARTITIONING ####

  # run vp for responses
  # tar_target(
  #   name = vp_response,
  #   command = make_variance_partioning(dat = big_data,
  #                                      response_var = fg_richness,
  #                                      fg_var = value_std,
  #                                      group = "response")
  # ),

  # run vp for multifunctionality
  # tar_target(
  #   name = vp_multifun,
  #   command = make_variance_partioning(dat = multifunctionality,
  #                                      response_var = fg_richness,
  #                                      fg_var = multifuntionality,
  #                                      group = "level")
  # )

)