# microclimate: average min/max ground temperature, daily_temp_amplitude (top 25%),
# min_soil_moisture (bottom 25%), mean soil moisture

process_microclimate <- function(microclimate_raw, year = 2022) {
  raw <- microclimate_raw |>
    filter(variable %in% c("ground_temperature", "soilmoisture")) |>
    mutate(
      year = year,
      date = as.Date(substr(date_time, 1, 10))
    )

  # 1) average min ground temperature: mean of daily min ground T per plot
  avg_min_ground_temp <- raw |>
    filter(variable == "ground_temperature") |>
    summarise(daily_min = min(value), .by = c(year, siteID, blockID, plotID, treatment, date)) |>
    summarise(value = mean(daily_min, na.rm = TRUE), .by = c(year, siteID, blockID, plotID, treatment)) |>
    mutate(response = "average min ground temperature", unit = "°C")

  # 2) average max ground temperature: mean of daily max ground T per plot
  avg_max_ground_temp <- raw |>
    filter(variable == "ground_temperature") |>
    summarise(daily_max = max(value), .by = c(year, siteID, blockID, plotID, treatment, date)) |>
    summarise(value = mean(daily_max, na.rm = TRUE), .by = c(year, siteID, blockID, plotID, treatment)) |>
    mutate(response = "average max ground temperature", unit = "°C")

  # 3) daily_temp_amplitude: mean of top 25% of daily (maxT - minT) per plot (ground temperature)
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

  # 4) min_soil_moisture: mean of bottom 25% of daily min moisture (driest days) per plot
  moist_extreme <- raw |>
    filter(variable == "soilmoisture") |>
    summarise(daily_min = min(value), .by = c(year, siteID, blockID, plotID, treatment, date)) |>
    mutate(
      q25 = quantile(daily_min, probs = 0.25, na.rm = TRUE),
      is_extreme = daily_min <= q25,
      .by = c(year, siteID, blockID, plotID, treatment)
    ) |>
    summarise(
      value = mean(daily_min[is_extreme], na.rm = TRUE) * -1,
      .by = c(year, siteID, blockID, plotID, treatment)
    ) |>
    mutate(response = "min_soil_moisture", unit = "%")

  # 5) mean soil moisture: mean soil moisture per plot
  mean_soil_moisture <- raw |>
    filter(variable == "soilmoisture") |>
    summarise(value = mean(value, na.rm = TRUE), .by = c(year, siteID, blockID, plotID, treatment)) |>
    mutate(response = "mean soil moisture", unit = "%")

  bind_rows(avg_min_ground_temp, avg_max_ground_temp, temp_amp, moist_extreme, mean_soil_moisture)
}
