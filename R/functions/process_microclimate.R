# microclimate: frost_days, daily_temp_amplitude (top 25%), min_soil_moisture (bottom 25%)

process_microclimate <- function(microclimate_raw, year = 2022) {
  raw <- microclimate_raw |>
    filter(variable %in% c("ground_temperature", "soilmoisture")) |>
    mutate(
      year = year,
      date = as.Date(substr(date_time, 1, 10))
    )

  # 1) frost_days: number of days with ground T < 0°C per plot (0 if no frost days)
  plots <- raw |>
    filter(variable == "ground_temperature") |>
    distinct(year, siteID, blockID, plotID, treatment)
  frost <- raw |>
    filter(variable == "ground_temperature") |>
    summarise(min_T = min(value), .by = c(year, siteID, blockID, plotID, treatment, date)) |>
    filter(min_T < 0) |>
    summarise(value = n(), .by = c(year, siteID, blockID, plotID, treatment))
  frost <- plots |>
    left_join(frost, by = c("year", "siteID", "blockID", "plotID", "treatment")) |>
    mutate(value = replace_na(value, 0L), response = "frost_days", unit = "days")

  # 2) daily_temp_amplitude: mean of top 25% of daily (maxT - minT) per plot (ground temperature)
  temp_amp <- raw |>
    filter(variable == "ground_temperature") |>
    summarise(amplitude = max(value) - min(value), .by = c(year, siteID, blockID, plotID, treatment, date)) |>
    mutate(
      q75 = quantile(amplitude, probs = 0.75, na.rm = TRUE),
      is_extreme = amplitude >= q75,
      .by = c(year, siteID, blockID, plotID, treatment)
    ) |>
    summarise(
      value = mean(amplitude[is_extreme], na.rm = TRUE),
      .by = c(year, siteID, blockID, plotID, treatment)
    ) |>
    mutate(response = "daily_temp_amplitude", unit = "°C")

  # 3) min_soil_moisture: mean of bottom 25% of daily min moisture (driest days) per plot
  moist_extreme <- raw |>
    filter(variable == "soilmoisture") |>
    summarise(daily_min = min(value), .by = c(year, siteID, blockID, plotID, treatment, date)) |>
    mutate(
      q25 = quantile(daily_min, probs = 0.25, na.rm = TRUE),
      is_extreme = daily_min <= q25,
      .by = c(year, siteID, blockID, plotID, treatment)
    ) |>
    summarise(
      value = mean(daily_min[is_extreme], na.rm = TRUE),
      .by = c(year, siteID, blockID, plotID, treatment)
    ) |>
    mutate(response = "min_soil_moisture", unit = "%")

  bind_rows(frost, temp_amp, moist_extreme)
}
