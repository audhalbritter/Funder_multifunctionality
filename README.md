# Funder multifunctionality

Analysis of how **plant functional group removal** affects **ecosystem multifunctionality** across climate gradients in mountain ecosystems (FunCaB/FUNDER experiment, Western Norway).

## Purpose

This repo downloads, processes, and analyses data from a macroecological experiment that replicates plant functional group removals (graminoids, forbs, bryophytes) across factorial temperature and precipitation gradients (Vestland Climate Grid). It tests whether biodiversity loss reduces multifunctionality and whether these effects depend on climate context. Ecosystem functions include biomass, decomposition, carbon and nutrient stocks/fluxes, and biodiversity of plants, soil fauna, and microbes.

## Workflow

The project uses the [**targets**](https://docs.ropensci.org/targets/) R package for a reproducible pipeline:

1. **Download** (`R/download_plan.R`) — Fetch data from the data repository (biomass, community composition, bryophytes, mesofauna, nematodes, etc.).
2. **Transform** (`R/tranformation_plan.R`) — Curate and standardise data; create site × treatment metadata.
3. **Multifunctionality** (`R/multifunctionality_plan.R`) — Merge functions, standardise to 0–1, compute average multifunctionality.
4. **Analysis** (`R/model_plan.R`, `R/pca_plan.R`, `R/si_analysis_plan.R`) — The main analyses encompass:
   - **Single-Function Models**: Linear mixed-effects models evaluating how functional group removals, functional group richness, or functional group-specific biomass affect individual ecosystem functions across temperature and precipitation gradients.
   - **Global Multifunctionality Models**: Testing how treatments and climate interactions affect an averaged ecosystem multifunctionality index.
   - **Multivariate Analysis (PCA & PERMANOVA)**: Assessing overall shifts in the functional composition of the ecosystem in response to treatments, climate, and site effects.
   - Model variants are run to compare the effect sizes of categorical treatment identity vs. continuous total or group-specific biomass (assessed via marginal/conditional $R^2$).
5. **Figures** (`R/figure_plan.R`) — Outputs for the main results.

Pipeline entry point: `_targets.R`. Run the pipeline with `run.R` or `targets::tar_make()`.

## Results

- **`results.qmd`** — Main results document (figures and model tables): effects of functional group number and identity on single functions and average multifunctionality, and how these vary with climate.
- **`results_new.qmd`** — Updated results (PCA, heatmaps, model tables).
- **`SI.qmd`** — Supplementary information.

### Publishing `results_new.qmd` (shareable page)

1. Run the pipeline, then render into `docs/` (output must be basename only; directory via `--output-dir`):
   ```bash
   quarto render results_new.qmd --output-dir docs --output index.html
   ```
   If you get `docs/results_new.html` instead, run `mv docs/results_new.html docs/index.html`.
2. Commit `docs/index.html` and `docs/.nojekyll`, push to `main`.
3. **GitHub Pages**: **Settings → Pages** → deploy from branch **main**, folder **/docs**.  
   Site URL: `https://audhalbritter.github.io/Funder_multifunctionality/`

   Or set Pages source to **GitHub Actions** and run the workflow **Publish results to GitHub Pages** (after `docs/index.html` is committed).

See **`docs/README.md`** for details.

## Setup

- R project; dependencies managed via **renv** (`renv::restore()`).
- Key packages: `tidyverse`, `targets`, `dataDownloader`, `dataDocumentation`, `lme4`, `lmerTest`, `vegan`, `patchwork`, `gt`, etc. (see `_targets.R` and `libraries.R`).
- **GitHub package**: `dataDocumentation` is from GitHub. Install it with `renv::install("audhalbritter/dataDocumentation")` so renv records the source; then `renv::snapshot()` will include it in the lockfile.