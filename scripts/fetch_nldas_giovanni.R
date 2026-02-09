library(tidyverse)

precip_sites <- tibble::tribble(
  ~id, ~site,                                ~lat,      ~lon,
  "KPD", "Kuhlanapo at Manning Creek Delta", 39.024924, -122.906493,
  "UMC", "Upper Manning Creek at Hwy 175",   38.995516, -122.934703
)

# downloaded in the browser for now
# https://api.giovanni.earthdata.nasa.gov/proxy-timeseries?data=NLDAS_FORA0125_H_2_0_Rainf&location=[39.024924,-122.906493]&time=2025-12-01T00:00:00/2026-02-09T00:00:00&version=2.0
# https://api.giovanni.earthdata.nasa.gov/proxy-timeseries?data=NLDAS_FORA0125_H_2_0_Rainf&location=[38.995516,-122.934703]&time=2025-12-01T00:00:00/2026-02-09T00:00:00&version=2.0

filepaths <- list(
  "KPD" = here::here("data-raw/timeseries_giovanni/timeseries_kpd.txt"),
  "UMC" = here::here("data-raw/timeseries_giovanni/timeseries_umc.txt")
)

# todo - convert to programmatic via
# https://disc.gsfc.nasa.gov/information/documents?title=Giovanni%20In%20The%20Cloud:%20Time%20Series%20Service
# https://disc.gsfc.nasa.gov/information/howto?title=How%20to%20Access%20the%20%22Giovanni%20in%20the%20Cloud:%20Time%20Series%22%20Service%20Using%20Python

precip_ts <- filepaths |> 
  lapply(\(x) read_csv(x, skip = 16, col_names = c("timestamp_utc", "rainf_kg_m2"))) |>
  bind_rows(.id = "site")  |>
  rename(precip_mm = rainf_kg_m2) |>
  mutate(precip_in = precip_mm / 25.4,
         timestamp = timestamp_utc |> with_tz("America/Los_Angeles"))

# precip_ts |> ggplot() + geom_col(aes(x = timestamp, y = precip_in, fill = site))

precip_ts |> saveRDS(here::here("data", "precip_ts.rds"))