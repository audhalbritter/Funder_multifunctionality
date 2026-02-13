# Manuscript plan
manuscript_plan <- list(

  # # bibliography
  # tar_target(
  #   name = bibliography,
  #   command = "manuscript/bibliography.bib",
  #   format = "file"
  # ),
  #
  # # add packages to bibliography
  # tar_target(
  #   name = biblio2,
  #   command = rjt.misc::package_citations(
  #     packages = c("targets", "tidyverse", "rmarkdown"),
  #     old_bib = bibliography,
  #     new_bib = "manuscript/bibliography2.bib"),
  #   format = "file"
  # ),

    # manuscript
    tar_quarto(name = manuscript,
               path = "results.qmd")

    # SI
    # tar_quarto(name = si,
    #            path = "SI.qmd")

)
