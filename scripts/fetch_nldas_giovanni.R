library(tidyverse)
library(lubridate)
library(httr2)
library(readr)

rds_path <- here::here("data/precip_ts.rds")

precip_sites <- tibble::tribble(
  ~id, ~site,                                ~lat,      ~lon,
  "KPD", "Kuhlanapo at Manning Creek Delta", 39.024924, -122.906493,
  "UMC", "Upper Manning Creek at Hwy 175",   38.995516, -122.934703
)

start_time <- "2025-12-01T00:00:00"
end_time   <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")  # "2026-02-09T00:00:00"
data_var   <- "NLDAS_FORA0125_H_2_0_Rainf"
version    <- "2.0"

# downloaded in the browser for now
# https://api.giovanni.earthdata.nasa.gov/proxy-timeseries?data=NLDAS_FORA0125_H_2_0_Rainf&location=[39.024924,-122.906493]&time=2025-12-01T00:00:00/2026-02-09T00:00:00&version=2.0
# https://api.giovanni.earthdata.nasa.gov/proxy-timeseries?data=NLDAS_FORA0125_H_2_0_Rainf&location=[38.995516,-122.934703]&time=2025-12-01T00:00:00/2026-02-09T00:00:00&version=2.0

USE_LOCAL <- F

if(USE_LOCAL) {
  
  filepaths <- list(
    "KPD" = here::here("data-raw/timeseries_giovanni/timeseries_kpd.txt"),
    "UMC" = here::here("data-raw/timeseries_giovanni/timeseries_umc.txt")
  )
  
  
  precip_ts <- filepaths |> 
    lapply(\(x) read_csv(x, skip = 16, col_names = c("timestamp_utc", "rainf_kg_m2"))) |>
    bind_rows(.id = "site")  |>
    rename(precip_mm = rainf_kg_m2) |>
    mutate(precip_in = precip_mm / 25.4,
           timestamp = timestamp_utc |> with_tz("America/Los_Angeles"))
  
  precip_ts |> saveRDS(rds_path)

} else {
  
  bearer_token <- Sys.getenv("EARTHDATA_TOKEN")
  if (bearer_token == "") stop("EARTHDATA_TOKEN not set in environment!")
  
  # todo - convert to programmatic via
  # https://disc.gsfc.nasa.gov/information/documents?title=Giovanni%20In%20The%20Cloud:%20Time%20Series%20Service
  # https://disc.gsfc.nasa.gov/information/howto?title=How%20to%20Access%20the%20%22Giovanni%20in%20the%20Cloud:%20Time%20Series%22%20Service%20Using%20Python
  
  pull_giovanni_timeseries <- function(lat, lon, start_time, end_time) {
    url <- paste0(
      "https://api.giovanni.earthdata.nasa.gov/timeseries?",
      "data=", data_var,
      "&location=[", lat, ",", lon, "]",
      "&time=", start_time, "/", end_time,
      "&version=", version
    )
    
    req <- request(url) |>
      req_headers(authorization = paste("Bearer", bearer_token))
    
    resp <- req_perform(req)
    if (resp_status(resp) != 200) {
      stop("Failed to fetch data: ", resp_status(resp))
    }
    
    ctype <- resp_header(resp, "Content-Type")
    if (!grepl("text/csv|application/json", ctype)) {
      stop("Unexpected content type: ", ctype, "\nResponse may be HTML, not data")
    }
    
    resp_body_string(resp) |>
      read_csv(skip = 16, col_names = c("timestamp_utc", "precip_mm"))
  }
  
  precip_ts <- precip_sites |>
    mutate(data = pmap(list(lat, lon), ~ pull_giovanni_timeseries(..1, ..2, start_time, end_time))) |>
    select(id, data) |>
    unnest(data) |>
    rename(site = id) |>
    mutate(
      precip_in = precip_mm / 25.4,
      timestamp = timestamp_utc |> with_tz("America/Los_Angeles")
    )
  
  if (file.exists(rds_path)) {
    existing <- readRDS(rds_path)
    combined <- bind_rows(existing, precip_ts) |> distinct()
  } else {
    combined <- precip_ts
  }
  
  combined |> saveRDS(rds_path)
  
}
