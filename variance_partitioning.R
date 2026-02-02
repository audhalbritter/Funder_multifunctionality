### Variance partitioning

make_variance_partioning <- function(dat, response_var, fg_var, group){

  dat |>
    rename(.response = {{response_var}},
         .functional_group = {{fg_var}}) |>
    group_by(across(all_of({{group}})))|>
    nest() |>
    mutate(vp = map(data, ~{
      # run model
      fit = lme4::lmer(data = ., .response ~ 1 + .functional_group + temperature_scaled:precipitation_scaled + .functional_group:temperature_scaled:precipitation_scaled + (1|siteID))
      # make vp
      varpart = glmm.hp(fit)
      df = varpart$hierarchical.partitioning |>
        as_tibble()
      }
    )
  ) |>
    unnest(vp) |>
    select(-data)
}


# big_data |>
#   group_by(response)|>
#   nest() |>
#   mutate(vp = map(data, ~{
#
#     fit = lme4::lmer(data = ., value_std ~ 1 + fg_richness + temperature_scaled:precipitation_scaled + fg_richness:temperature_scaled:precipitation_scaled + (1|siteID))
#     varpart = glmm.hp(fit)
#
#     }
#   )
# )

library(partR2)
fit <- lme4::lmer(multifuntionality ~ fg_richness * temperature_scaled * precipitation_scaled + (1|siteID), data = multifunctionality)

partR2(fit, partvars = (fg = c("fg_richness", "temperature_scaled", "precipitation_scaled")),
       nboot = NULL)

fit <- lme4::lmer(multifuntionality ~ fg_richness * temperature_scaled + (1|siteID), data = multifunctionality)

res <- partR2(fit, partvars = (fg = c("fg_richness", "temperature_scaled", "fg_richness:temperature_scaled")),
       nboot = NULL)
summary(res)


res <- partR2(fit, partbatch = list(fg = c("fg_richness"),
                                    climate = c("temperature_scaled:precipitation_scaled"),
                                    context = c("fg_richness:temperature_scaled", "fg_richness:precipitation_scaled")),
              nboot = 10)

summary(res)

# library(palmerpenguins)
# penguins |>
#   filter(!is.na(body_mass_g)) |>
#   mutate(level = "global") |>
#   group_by()|>
#   nest() |>
#   mutate(model = map(data, ~{
#
#     fit <- lme4::lmer(data = .x, body_mass_g ~ bill_length_mm + bill_depth_mm + bill_length_mm:bill_depth_mm + (1|species))
#          varpart = glmm.hp(fit)
#          vp <- varpart$hierarchical.partitioning |>
#            as_tibble()
#
#     }
#     )) |>
#   unnest(model)
#
#
fit <- lmer(body_mass_g ~ bill_length_mm + bill_depth_mm + bill_length_mm:bill_depth_mm + (1|species), data = penguins)
varpart <- glmm.hp(fit)
glmm.hp(fit)

bio <- big_data |>
  filter(response == "biomass")

t <- lmer(value_std ~ temperature_scaled + (1|siteID), data = bio)
p <- lmer(value_std ~ precipitation_scaled + (1|siteID), data = bio)
f <- lmer(value_std ~ fg_richness + (1|siteID), data = bio)
tp <- lmer(value_std ~ temperature_scaled:precipitation_scaled + (1|siteID), data = bio)
tf <- lmer(value_std ~ temperature_scaled:fg_richness + (1|siteID), data = bio)
pf <- lmer(value_std ~ precipitation_scaled:fg_richness + (1|siteID), data = bio)
tpf <- lmer(value_std ~ temperature_scaled:precipitation_scaled:fg_richness + (1|siteID), data = bio)

d <- lmer(value_std ~ temperature_scaled:fg_richness + precipitation_scaled:fg_richness + temperature_scaled:precipitation_scaled:fg_richness + (1|siteID), data = bio)

MuMIn::r.squaredGLMM(t)
MuMIn::r.squaredGLMM(p)
MuMIn::r.squaredGLMM(f)
MuMIn::r.squaredGLMM(tp)
MuMIn::r.squaredGLMM(tf)
MuMIn::r.squaredGLMM(pf)
MuMIn::r.squaredGLMM(tpf)
MuMIn::r.squaredGLMM(d)

Climate: tp
or
Climate: t + p - tf - tp - tpf

fit <- lmer(value_std ~ temperature_scaled + fg_richness + temperature_scaled:fg_richness + (1|siteID), data = bio)
glmm.hp(fit)


fit <- lmer(value_std ~ temperature_scaled * fg_richness + (1|siteID), data = bio)
res <- partR2(fit, partvars = c("temperature_scaled", "fg_richness", "temperature_scaled:fg_richness"),
              nboot = NULL)
summary(res)
res2 <- partR2(fit, partbatch = list(Climate = c("temperature_scaled", "temperature_scaled:fg_richness"),
                                     FG = c("fg_richness", "temperature_scaled:fg_richness"),
                                     Context=c("temperature_scaled:fg_richness")),
               nboot=10)
summary(res2)




