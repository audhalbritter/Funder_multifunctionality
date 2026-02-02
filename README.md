# Funder multifunctionality

Analysis of how **plant functional group removal** affects **ecosystem multifunctionality** across climate gradients in mountain ecosystems (FunCaB/FUNDER experiment, Western Norway).

## Purpose

This repo downloads, processes, and analyses data from a macroecological experiment that replicates plant functional group removals (graminoids, forbs, bryophytes) across factorial temperature and precipitation gradients (Vestland Climate Grid). It tests whether biodiversity loss reduces multifunctionality and whether these effects depend on climate context. Ecosystem functions include biomass, decomposition, carbon and nutrient stocks/fluxes, and biodiversity of plants, soil fauna, and microbes.

## Workflow

The project uses the [**targets**](https://docs.ropensci.org/targets/) R package for a reproducible pipeline:

1. **Download** (`R/download_plan.R`) — Fetch data from the data repository (biomass, community composition, bryophytes, mesofauna, nematodes, etc.).
2. **Transform** (`R/tranformation_plan.R`) — Curate and standardise data; create site × treatment metadata.
3. **Multifunctionality** (`R/multifunctionality_plan.R`) — Merge functions, standardise to 0–1, compute average multifunctionality.
4. **Analysis** (`R/analysis_plan.R`) — Linear mixed-effects models for effects of functional group richness, treatment identity, temperature, and precipitation.
5. **Figures** (`R/figure_plan.R`) — Outputs for the main results.

Pipeline entry point: `_targets.R`. Run the pipeline with `run.R` or `targets::tar_make()`.

## Results

- **`results.qmd`** — Main results document (figures and model tables): effects of functional group number and identity on single functions and average multifunctionality, and how these vary with climate.
- **`SI.qmd`** — Supplementary information.

## Setup

- R project; dependencies managed via **renv** (`renv::restore()`).
- Key packages: `tidyverse`, `targets`, `dataDownloader`, `dataDocumentation`, `lme4`, `lmerTest`, `vegan`, `patchwork`, `gt`, etc. (see `_targets.R` and `libraries.R`).
