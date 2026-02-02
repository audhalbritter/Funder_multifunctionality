### ordination

# calc_comm_metrics <- function(comm){
#
#   comm_resp <- comm %>%
#     group_by(Year, Site, Treatment, PlotID) %>%
#     summarise(Richness = n(),
#               Diversity = diversity(Abundance),
#               Evenness = Diversity/log(Richness),
#
#               # proportions
#               sumAbundance = sum(Abundance),
#               propGraminoid = sum(Abundance[FunctionalGroup %in% c("graminoid")])/sumAbundance,
#               propForb = sum(Abundance[FunctionalGroup %in% c("forb")])/sumAbundance,
#               propShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")])/sumAbundance,
#               propEShrub = sum(Abundance[FunctionalGroup %in% c("eshrub")])/sumAbundance,
#               propDShrub = sum(Abundance[FunctionalGroup %in% c("dshrub")])/sumAbundance,
#               propLichen = sum(Abundance[FunctionalGroup %in% c("lichen")])/sumAbundance,
#               propBryo = sum(Abundance[FunctionalGroup %in% c("moss", "liverwort")])/sumAbundance,
#
#               # abundance
#               totalVascular = sum(Abundance[FunctionalGroup %in% c("graminoid", "forb", "eshrub", "dshrub")]),
#               totalGraminoid = sum(Abundance[FunctionalGroup %in% c("graminoid")]),
#               totalForb = sum(Abundance[FunctionalGroup %in% c("forb")]),
#               totalShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")]),
#               totaleShrub = sum(Abundance[FunctionalGroup %in% c("eshrub")]),
#               totaldShrub = sum(Abundance[FunctionalGroup %in% c("dshrub")])
#     )
#   return(comm_resp)
# }



### MAKE SP PCA
make_sp_pca <- function(cover){

  comm_wide <- cover |>
    filter(!is.na(cover)) |>
    select(-functional_group) |>
    mutate(cover = sqrt(cover)) |>
    pivot_wider(names_from = species, values_from = cover, values_fill = 0)

  comm_sp <- comm_wide |>
    select(-c(year:precipitation_level))

  comm_info <- comm_wide |>
    select(c(year:precipitation_level))


  # make pca
  res <- rda(comm_sp)

  out <- bind_cols(comm_info, fortify(res) |>
                     filter(Score == "sites"))

  sp <- fortify(res) |>
    filter(Score == "species")

  return(list(out, sp, res))

}
