### TARGETS PIPELINE

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("here", "tidyverse", "dataDownloader", "dataDocumentation", "scales", "vegan", "ggvegan", "glue", "grid", "ggcorrplot", "ggnewscale", "lme4", "broom.mixed", "lmerTest", "patchwork", "gt", "ggsignif", "performance", "glmm.hp")
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = "R")
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  download_plan,
  transformation_plan,
  multifunctionality_plan,
  new_analysis_plan,
  model_plan,
  # analysis_plan,
  # figure_plan,
  #si_analysis_plan,
  # manuscript_plan,
  si_figure_plan
)
