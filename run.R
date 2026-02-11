library(targets)
source(here::here("libraries.R"))

targets::tar_make()

targets::tar_load_everything()

# if pipeline is interrupted, destroy targets to avoid stale targets:
# tar_destroy(destroy = "meta")
# tar_destroy(destroy = "progress")

# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint


# check for warnings
targets::tar_meta(fields = warnings, complete_only = TRUE)