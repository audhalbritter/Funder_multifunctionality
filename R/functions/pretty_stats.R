# Fancy stats dictionary
#
#' @param functional_group_as How the model's .functional_group was defined:
#'   - `"treatment"` (default): factorial levels (F, G, B, ...); strips `.functional_group`
#'     prefix from treatment-contrast terms.
#'   - `"richness"`: numeric predictor passed as `.functional_group` in make_model (e.g. fg_richness
#'     renamed); lme4/broom emit terms like `.functional_group`, `.functional_group:temperature_scaled`.
#'     Those are labelled `FR` (functional group richness). No forb/gram/bryo replacements.
fancy_stats <- function(dat, sort = TRUE, functional_group_as = c("treatment", "richness")) {
  functional_group_as <- match.arg(functional_group_as)

  if (functional_group_as == "richness") {
    # make_model renames fg_richness -> .functional_group; tidy() term column uses
    # ".functional_group" and ".functional_group:temperature_scaled" etc. — never "fg_richness".
    # Use str_replace_all so every occurrence is rewritten (e.g. three-way interactions).
    # Coerce to character so factor levels do not block replacements.
    dat <- dat |>
      mutate(
        term = as.character(term),
        term = str_replace_all(term, fixed("(Intercept)"), "Intercept"),
        # Must run before T/P so interaction strings collapse correctly
        term = str_replace_all(term, fixed(".functional_group"), "FR"),
        term = str_replace_all(term, "temperature_scaled", "T"),
        term = str_replace_all(term, "precipitation_scaled", "P"),
        term = str_replace(term, "total_biomass_scaled", "Bio"),
        term = str_replace(term, "total_biomass", "Bio"),
        term = str_replace(term, "biomass_scaled_G", "BioG"),
        term = str_replace(term, "biomass_scaled_F", "BioF"),
        term = str_replace(term, "biomass_scaled_B", "BioB"),
        term = str_replace(term, "biomass_G", "BioG"),
        term = str_replace(term, "biomass_F", "BioF"),
        term = str_replace(term, "biomass_B", "BioB")
      )
    if (sort) {
      term_levels <- c(
        "Intercept",
        "FR", "T", "P",
        "T:P",
        "FR:T", "FR:P",
        "FR:T:P",
        "Bio",
        "BioG", "BioF", "BioB",
        "BioG:T", "BioF:T", "BioB:T",
        "BioG:P", "BioF:P", "BioB:P"
      )
      all_terms <- unique(dat$term)
      ordered_terms <- c(intersect(term_levels, all_terms), setdiff(all_terms, term_levels))
      dat <- dat |>
        mutate(term = factor(term, levels = ordered_terms))
    }
    return(dat)
  }

  # Default: treatment / factorial .functional_group
  dat <- dat %>%
    mutate(
      term = str_replace(term, "treatment", ""),
      term = str_replace(term, "\\(Intercept\\)", "Intercept"),
      term = str_replace(term, "forb", "F"),
      term = str_replace(term, "gram", "G"),
      term = str_replace(term, "bryo", "B"),
      # treatment model: .functional_groupGF -> GF, etc.
      term = str_replace(term, "\\.functional_group", ""),
      term = str_replace(term, "temperature_scaled", "T"),
      term = str_replace(term, "precipitation_scaled", "P"),
      # total_biomass / total_biomass_scaled (treatment model) -> Bio
      term = str_replace(term, "total_biomass_scaled", "Bio"),
      term = str_replace(term, "total_biomass", "Bio"),
      # biomass composition model -> BioG, BioF, BioB (replace after T/P so interactions become BioG:T etc.)
      term = str_replace(term, "biomass_scaled_G", "BioG"),
      term = str_replace(term, "biomass_scaled_F", "BioF"),
      term = str_replace(term, "biomass_scaled_B", "BioB")
    )

  if (sort == TRUE) {
    term_levels <- c(
      "Intercept",
      "T", "P", "GB", "FB", "GF", "B", "G", "F", "C",
      "T:P",
      "GB:T", "FB:T", "GF:T", "B:T", "G:T", "F:T", "C:T",
      "GB:P", "FB:P", "GF:P", "B:P", "G:P", "F:P", "C:P",
      "GB:T:P", "FB:T:P", "GF:T:P", "B:T:P", "G:T:P", "F:T:P", "C:T:P",
      # factorial model (forb, gram, bryo)
      "FG", "FG:T", "FG:P", "FG:T:P",
      "F", "G", "B",
      # biomass model: total_biomass -> Bio; biomass_G/F/B -> BioG, BioF, BioB
      "Bio",
      "BioG", "BioF", "BioB",
      "BioG:T", "BioF:T", "BioB:T",
      "BioG:P", "BioF:P", "BioB:P"
    )
    all_terms <- unique(dat$term)
    ordered_terms <- c(intersect(term_levels, all_terms), setdiff(all_terms, term_levels))
    dat <- dat |>
      mutate(term = factor(term, levels = ordered_terms))
  }

  return(dat)
}


### Stats tables
round_numbers_anova <- function(table){
  table |>
    mutate(sumsq = round(sumsq, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 3),
           p.value = if_else(p.value < 0.001, "<0.001", as.character(p.value))) |>
    select(-names)
}


round_numbers_tidy <- function(table){
  table |>
    mutate(p_raw = p.value,
           estimate = round(estimate, 2),
           std.error = round(std.error, 2),
           statistic = round(statistic, 2),
           df = round(df, 2),
           p.value = round(p.value, 3),
           p.value = if_else(p.value < 0.001, "<0.001", as.character(p.value)))
}


table_style <- function(gt_table, font_size){

  gt_table |>
    tab_options(
      table.font.size = font_size,
      data_row.padding = gt::px(1)
    ) |>
    cols_align(
      align = c("left"),
      columns = term
    ) |>
    cols_align(
      align = c("right"),
      columns = contains("p.value")
    )

}


#' GT table from make_model() output; unnest one of result / result_biomass / result_FG_biomass
#' @param dat Nested tibble from make_model()
#' @param unnest_col Name of list-column to unnest
#' @param extra_drop Extra column names to drop after unnest (e.g. "level_name")
#' @param functional_group_as Passed to [fancy_stats]: `"treatment"` (default) for factorial
#'   treatment contrasts; `"richness"` when `.functional_group` was fg_richness (numeric), terms
#'   labelled `FR`.
model_output_gt <- function(
    dat,
    unnest_col = "result",
    extra_drop = NULL,
    functional_group_as = "treatment"
) {
  drops <- c(
    "data", "model", "model_biomass", "model_FG_biomass", "effect", "group",
    "anova", "anova_tidy", "result", "result_biomass", "anova_biomass", "anova_tidy_biomass",
    "result_FG_biomass", "anova_FG_biomass", "anova_tidy_FG_biomass", extra_drop
  )
  out <- dat |>
    unnest(!!rlang::sym(unnest_col)) |>
    filter(effect == "fixed") |>
    select(-dplyr::any_of(drops)) |>
    fancy_stats(functional_group_as = functional_group_as)
  round_numbers_tidy(out) |>
    gt() |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(
        columns = c(term, estimate, std.error, statistic, df, p.value),
        rows = p_raw <= 0.05
      )
    ) |>
    tab_style(
      style = list(cell_text(style = "italic")),
      locations = cells_body(
        columns = c(term, estimate, std.error, statistic, df, p.value),
        rows = p_raw > 0.05 & p_raw <= 0.07
      )
    ) |>
    cols_hide(p_raw) |>
    table_style(font_size = 11)
}
