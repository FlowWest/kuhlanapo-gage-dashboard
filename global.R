library(tidyverse)
library(janitor)

sites <- tribble(
  ~category,    ~code,   ~site,                 ~site_label,                  ~site_descrip,                    ~twg_elev,
  "Stage Gage", "MC-01", "Lower Manning Creek", "Manning Creek West Branch",  "5400 ft DS Soda Bay Rd",         1327.83,   
  "Stage Gage", "MC-03", "Upper Manning Creek", "Manning Creek Mainstem",     "3800 ft DS Soda Bay Rd",         1329.39,   
  "Stage Gage", "MC-02", "Secondary Channel",   "Secondary Channel",          "4300 ft DS Soda Bay Rd",         1331.89,   
  "Piezometer", "PZ-A1", "Piezometer A1",       "Piezometer A1",              "E, 300 ft S of Clear Lake",   NA,        
  "Piezometer", "PZ-A2", "Piezometer A2",       "Piezometer A2",              "E, 1000 ft S of Clear Lake",  NA,        
  "Piezometer", "PZ-A3", "Piezometer A3",       "Piezometer A3",              "E, 2000 ft S of Clear Lake",  NA,        
  "Piezometer", "PZ-B1", "Piezometer B1",       "Piezometer B1",              "W, 800 ft S of Clear Lake",   NA,        
  "Piezometer", "PZ-B2", "Piezometer B2",       "Piezometer B2",              "W, 1400 ft S of Clear Lake",  NA,        
  "Piezometer", "PZ-B3", "Piezometer B3",       "Piezometer B3",              "W, 2600 ft S of Clear Lake",  NA,        
  "Piezometer", "PZ-B4", "Piezometer B4",       "Piezometer B4",              "W, 3500 ft S of Clear Lake",  NA,        
  "Piezometer", "PZ-C1", "Piezometer C1",       "Piezometer C1",              "W, 1000 ft W of Transect B",  NA,        
  "Piezometer", "PZ-C2", "Piezometer C2",       "Piezometer C2",              "W, 1500 ft W of Transect B",  NA,        
  "Piezometer", "PZ-C3", "Piezometer C3",       "Piezometer C3",              "W, 2400 ft W of Transect B",  NA,        
) |>
  mutate(category = as_factor(category))

sensors <- tribble(
  ~code,   ~name,           ~type,
  "MC-01", "2025SGMC01",    "troll",
  "MC-01", "2025SGMC01_VL", "vulink",
  "MC-03", "2025SGMC03",    "troll",
  "MC-03", "2025SGMC03_VL", "vulink",
  "MC-02", "2025SGMC02",    "troll",
  "MC-02", "2025SGMC02_VL", "vulink",
  "PZ-A1", "2025PZA01",     "troll",
  "PZ-A2", "2025PZA02",     "troll",
  "PZ-A3", "2025PZA03",     "troll",
  "PZ-B1", "2025PZB01",     "troll",
  "PZ-B2", "2025PZB02",     "troll",
  "PZ-B3", "2025PZB03",     "troll",
  "PZ-B4", "2025PZB04",     "troll",
  "PZ-C1", "2025PZC01",     "troll",
  "PZ-C2", "2025PZC02",     "troll",
  "PZ-C3", "2025PZC03",     "troll",
  "PZ-B4", "2025BAROK01",   "barotroll"
) |> 
  mutate(across(everything(), as.character)) |>
  inner_join(sites |>
               select(code, category, site), 
             by = join_by(code))

gages <- sensors |>
  filter(category == "Stage Gage") 

piezos <- sensors |>
  filter(category == "Piezometer") 

site_labels <- sites |>
  select(code, site_label) |>
  deframe()

site_descrips <- sites |>
  select(code, site_descrip) |>
  deframe()

sites_stage <- sites |> filter(category == "Stage Gage")

site_colors <- RColorBrewer::brewer.pal(n = length(sites_stage$code), name = "Paired")
names(site_colors) <- sites_stage$code

sites_piezo <- sites |> filter(category == "Piezometer")

piezo_colors <- tribble(
  ~code, ~color,
  "PZ-A1", "#c90074", 
  "PZ-A2", "#ff731c", 
  "PZ-A3", "#ffd28f", 
  "PZ-B1", "#001284", 
  "PZ-B2", "#007793", 
  "PZ-B3", "#55abf2", 
  "PZ-B4", "#d2e0ff", 
  "PZ-C1", "#47d09c", 
  "PZ-C2", "#acf186", 
  "PZ-C3", "#e2e592", 
) |> deframe()

piezo_meta <- tribble(
  # effective_date: first date (America/Los_Angeles) a row applies. NA = applies
  # from the beginning of record. Readings are never retroactively re-referenced.
  ~name,        ~gse_ft_navd88,  ~tdx_ft_navd88, ~effective_date,
  "2025PZA01",  1329.250,        1323.550,       NA,
  "2025PZA02",  1326.214,        1321.964,       NA,
  "2025PZA02",  1326.254,        1322.024,       ymd("2026-11-17"),
  "2025PZA03",  1328.249,        1321.604,       NA,
  "2025PZA03",  1328.319,        1321.789,       ymd("2026-11-17"),
  "2025PZB01",  1326.417,        1322.147,       NA,
  "2025PZB02",  1326.019,        1323.079,       NA,
  "2025PZB03",  1329.669,        1322.189,       NA,
  "2025PZB04",  1331.618,        1322.918,       NA,
  "2025PZC01",  1330.484,        1322.609,       NA,
  "2025PZC02",  1328.072,        1323.287,       NA,
  "2025PZC03",  1333.552,        1327.207,       NA,
) |>
  mutate(effective_date = as.Date(effective_date),
         # start of the effective day, local time; -Inf = start of record
         effective_from = if_else(is.na(effective_date),
                                  as.POSIXct(-Inf, tz = "America/Los_Angeles"),
                                  force_tz(as.POSIXct(effective_date), "America/Los_Angeles")))

# Elevation (ft NAVD88) of each barometric-reference Troll, used to adjust baro
# to each transducer's elevation (see submerged_depth_ft()). NA means no
# elevation correction is applied.
# 2025BAROK01 hangs at the PZ-B4 site, ~3 ft above B4's ground surface
# (1331.618 ft). APPROXIMATE -- replace with the surveyed elevation. The
# transducer elevations (tdx_ft_navd88 in piezo_meta, 1321.6-1327.2) are
# ~7-13 ft below that, i.e. a correction of about 0.01 ft of water.
baro_meta <- tribble(
  ~name,         ~z_baro_ft_navd88,
  "2025BAROK01", 1331.618 + 3,
)

# Attach the piezometer survey values in effect at each reading's timestamp.
# Needs `name` and `timestamp` columns; adds gse_ft_navd88 and tdx_ft_navd88.
join_piezo_meta <- function(df) {
  df |>
    left_join(piezo_meta |> select(name, gse_ft_navd88, tdx_ft_navd88, effective_from),
              by = join_by(name, closest(timestamp >= effective_from))) |>
    select(-effective_from)
}

match_lab_lightness <- function(in_color,
                                template_color = NULL,
                                out_L = NULL) {
  
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Package 'colorspace' is required.")
  }
  
  # Input validation
  if (is.null(template_color) && is.null(out_L)) {
    stop("Supply either 'template_color' or 'out_L'.")
  }
  if (!is.null(template_color) && !is.null(out_L)) {
    stop("Supply only one of 'template_color' or 'out_L', not both.")
  }
  
  # Convert input color to LAB
  lab_in <- as(
    colorspace::hex2RGB(in_color),
    "LAB"
  )@coords
  
  # Determine target L*
  if (!is.null(template_color)) {
    lab_template <- as(
      colorspace::hex2RGB(template_color),
      "LAB"
    )@coords
    target_L <- lab_template[1]
  } else {
    target_L <- out_L
  }
  
  # Replace L*, preserve a* and b*
  lab_out <- lab_in
  lab_out[1] <- target_L
  
  # Convert back to hex
  hex_out <- colorspace::hex(
    colorspace::LAB(
      L = lab_out[1],
      A = lab_out[2],
      B = lab_out[3]
    )
  )
  
  return(hex_out)
}

interpolate_idw_at_time <- function(
    idw_obj,
    ts_data,
    t0,
    return_matrix = TRUE,
    clip_distance = 1000
) {
  sites_df <- idw_obj$sites
  site_ids <- as.character(sites_df$id)
  weights_raw <- idw_obj$weights_raw
  dims <- idw_obj$dims
  
  # ---- 1. Extract last observation per site ----
  last_obs <- ts_data |>
    filter(code %in% site_ids, timestamp <= t0) |>
    arrange(code, desc(timestamp)) |>
    group_by(code) |>
    slice_head(n = 1) |>
    ungroup()
  
  # initialize vectors
  z_last <- setNames(rep(NA_real_, length(site_ids)), site_ids)
  age_minutes <- setNames(rep(NA_real_, length(site_ids)), site_ids)
  
  if (nrow(last_obs) > 0) {
    z_last[last_obs$code] <- last_obs$value
    age_minutes[last_obs$code] <-
      as.numeric(difftime(t0, last_obs$timestamp, units = "mins"))
  }
  
  # ---- 2. Age-weight computation ----
  tau <- sites_df$tau_minutes
  max_age <- sites_df$max_age_minutes
  
  age_weight <- rep(0, length(site_ids))
  valid_age <- !is.na(age_minutes)
  
  age_weight[valid_age] <- exp(-age_minutes[valid_age] / tau[valid_age])
  age_weight[valid_age & age_minutes > max_age] <- 0
  
  # invalidate missing values
  invalid <- is.na(z_last) | age_weight == 0
  if (all(invalid)) {
    out <- rep(NA_real_, nrow(weights_raw))
    if (return_matrix) {
      return(matrix(out, nrow = dims[1], ncol = dims[2], byrow = FALSE))
    }
    return(out)
  }
  
  # ---- 3. Apply age-weighted IDW ----
  # scale columns of raw weights by age weights
  weights_eff <- sweep(weights_raw, 2, age_weight, `*`)
  weights_eff[, invalid] <- 0
  
  denom <- rowSums(weights_eff)
  denom[denom == 0] <- NA_real_
  
  z_eff <- z_last
  z_eff[invalid] <- 0
  
  numer <- as.vector(weights_eff %*% z_eff)
  z_grid <- numer / denom
  z_grid[is.nan(z_grid)] <- NA_real_
  
  z_grid[idw_obj$min_dist > clip_distance] <- NA_real_
  
  # ---- 4. Return ----
  if (return_matrix) {
    matrix(z_grid, nrow = dims[1], ncol = dims[2], byrow = FALSE)
  } else {
    z_grid
  }
}

# Mask piezometer readings taken while the transducer was not submerged -- dry
# well or troll pulled to the surface; both become NA, so they aren't told apart.
#
# `pressure_psi` is the troll's baromerged (gauge) pressure and `baro_psi` the
# barometric pressure it was merged against. diff_ft = (P_trans - P_baro_adj) /
# 2.99 kPa/ft is the water depth over the sensor plus a sensor-specific zero
# offset (`air_zero`), which is estimated empirically per call (i.e. per
# sensor) as the mode of the near-zero cluster -- not the median, which gets
# dragged up by genuine shallow water.
#
# Elevation correction: baro is taken at z_baro_ft and the transducer sits at
# z_trans_ft (same vertical datum). Baro is adjusted to the transducer's
# elevation, P_baro_adj = P_baro * exp(g dz / (R T)), dz = z_baro - z_trans (m).
# This assumes the baromerge itself applied NO elevation term (it subtracts
# baro as logged). A constant elevation difference is also absorbed by
# air_zero, so with both active the explicit term only matters for the part
# air_zero can't see (poorly constrained air_zero, or z_trans changing between
# deployments). If z_baro_ft is NA no elevation correction is applied.
#
# Returns water depth above the sensor (ft), NA wherever not submerged.
# Inputs must be ordered by time within a single sensor.
submerged_depth_ft <- function(pressure_psi, baro_psi, z_baro_ft, z_trans_ft,
                               wet_thresh_ft = 0.15, min_wet_run = 4,
                               air_window_ft = 0.15, min_air_n = 30,
                               kpa_per_ft = 2.99, air_temp_k = 288.15) {
  psi_to_kpa <- 6.894757
  p_gauge <- pressure_psi * psi_to_kpa
  p_baro  <- baro_psi * psi_to_kpa

  # Task 2: adjust baro to the transducer's elevation (skip if unknown)
  dz_m <- (z_baro_ft - z_trans_ft) * 0.3048
  adj  <- ifelse(is.na(dz_m) | is.na(p_baro), 0,
                 p_baro * (exp(9.80665 * dz_m / (287.05 * air_temp_k)) - 1))
  # (P_trans_abs - P_baro_adj) with P_trans_abs = P_gauge + P_baro
  diff_ft <- (p_gauge - adj) / kpa_per_ft

  # air zero. Preferred: readings taken at the surface -- a sudden drop to
  # ~0 from over 1 ft of water within the previous 3 readings (troll pulled).
  # These are unambiguous even for a well that never dries out, where the
  # near-zero cluster is real shallow water, not the instrument's air offset.
  # Fallback: mode of the density of near-zero readings (a dry well's stable
  # plateau). If neither is available, no offset is removed.
  near_zero <- !is.na(diff_ft) & abs(diff_ft) <= air_window_ft
  prev_hi <- pmax(dplyr::lag(diff_ft, 1), dplyr::lag(diff_ft, 2),
                  dplyr::lag(diff_ft, 3), na.rm = TRUE)
  surface <- diff_ft[near_zero & !is.na(prev_hi) & prev_hi > 1]
  near <- diff_ft[near_zero]
  air_zero <- if (length(surface) >= 1) {
    stats::median(surface)
  } else if (length(near) >= min_air_n) {
    dens <- stats::density(near, bw = 0.01, from = -air_window_ft, to = air_window_ft)
    dens$x[which.max(dens$y)]
  } else 0

  depth_ft <- diff_ft - air_zero
  wet_raw  <- !is.na(depth_ft) & depth_ft > wet_thresh_ft

  # drop short wet blips (shorter than min_wet_run consecutive readings);
  # short dry runs stay dry -- those are real air readings
  r <- rle(wet_raw)
  run_len <- rep(r$lengths, r$lengths)
  wet <- wet_raw & run_len >= min_wet_run

  if_else(wet, depth_ft, NA_real_)
}

# Null out piezometer depth readings that don't line up with their nearest
# in-time neighbor -- caused by trolls being pulled from the well for
# periodic data download/recalibration, which shows up as either a single
# round-trip dip/spike or a step change with one bad transition reading.
# A neighbor only counts if it's within `max_gap_min`, so genuine drift
# across a real data gap (sensor offline, not just misreading) isn't flagged.
#
# A neighbor is only trusted as a reference if it's itself corroborated by
# the reading one step further out (e.g. lag_d vs lag(depth, 2)) -- without
# this, two consecutive bad readings that happen to agree with each other
# (common right after a troll is redeployed: a reading or two of transition
# noise before the signal stabilizes) pass the single-neighbor check and
# neither gets flagged. If that corroborating point is unavailable, the
# neighbor is NOT trusted (errs toward nulling at a gap boundary rather
# than letting an unconfirmed transition reading through).
# `timestamp`/`depth` must be ordered by time within one piezometer's series.
clean_piezo_depth <- function(timestamp, depth, hard_max = 18,
                               jump_thresh = 1, max_gap_min = 60) {
  dt_lag  <- as.numeric(difftime(timestamp, lag(timestamp),  units = "mins"))
  dt_lead <- as.numeric(difftime(lead(timestamp), timestamp, units = "mins"))
  lag_d   <- lag(depth)
  lead_d  <- lead(depth)

  dt_lag2  <- as.numeric(difftime(timestamp, lag(timestamp, 2),  units = "mins"))
  dt_lead2 <- as.numeric(difftime(lead(timestamp, 2), timestamp, units = "mins"))
  lag_d2   <- lag(depth, 2)
  lead_d2  <- lead(depth, 2)

  close_lag  <- !is.na(dt_lag)  & dt_lag  <= max_gap_min
  close_lead <- !is.na(dt_lead) & dt_lead <= max_gap_min

  lag_corroborated  <- !is.na(dt_lag2)  & dt_lag2  <= max_gap_min & !is.na(lag_d2)  & abs(lag_d  - lag_d2)  <= jump_thresh
  lead_corroborated <- !is.na(dt_lead2) & dt_lead2 <= max_gap_min & !is.na(lead_d2) & abs(lead_d - lead_d2) <= jump_thresh

  ok_via_lag  <- close_lag  & !is.na(lag_d)  & lag_d  <= hard_max & abs(depth - lag_d)  <= jump_thresh & lag_corroborated
  ok_via_lead <- close_lead & !is.na(lead_d) & lead_d <= hard_max & abs(depth - lead_d) <= jump_thresh & lead_corroborated
  no_close_neighbor <- !close_lag & !close_lead

  case_when(
    depth > hard_max ~ NA_real_,
    no_close_neighbor ~ depth,
    !ok_via_lag & !ok_via_lead ~ NA_real_,
    TRUE ~ depth
  )
}

# Rename pre-rhydrovu HydroVu columns (id, parameterId, unitId, parm_name) to
# the standardized rhydrovu names. A no-op on data already in the new schema;
# only needed while the deployed app may still see an old-schema gage_data.rds
# (upstream on the data branch, or in its on-disk cache).
standardize_hydrovu_cols <- function(df) {
  if (is.null(df) || "location_id" %in% names(df)) return(df)
  df |>
    rename(any_of(c(location_id = "id", parameter_id = "parameterId",
                    unit_id = "unitId", parameter_name = "parm_name"))) |>
    mutate(across(any_of("location_id"), \(x) sprintf("%.0f", x)))
}

# Build the cleaned, pivoted gage/piezometer time series used by both app.R
# and data-raw/report_figures.Rmd. Keeping this in one place means the two
# can never drift out of sync the way the piezometer depth cleaning did.
# `ts_data`/`ll_data` must already be resolved data frames (call reactives
# like `ts_data()` before passing them in).
build_df_pivot <- function(ts_data, ll_data) {

  # 2025BAROK01 (the BaroTroll) is the shared, gap-free atmospheric Baro
  # reference for all piezometers (not the SGMC stage gages). Pulled out as
  # its own lookup -- rather than left in the main per-device pivot below --
  # since it shares a `code`/`site` with PZ-B4's primary troll and would
  # otherwise collide with that device's own readings at pivot_wider().
  # It logs on its own :05/:20/:35/:50 schedule rather than the piezometers'
  # :00/:15/:30/:45 one, so its timestamps are rounded onto their grid before
  # the join below -- an exact-timestamp join against the raw schedule would
  # almost never match.
  barotroll_baro <- ts_data |>
    filter(name == "2025BAROK01", parameter_name == "Baro") |>
    mutate(timestamp = lubridate::round_date(timestamp, "15 minutes")) |>
    summarise(baro_fallback = mean(value), .by = timestamp)

  ts_data |>
    filter(name != "2025BAROK01") |>
    inner_join(sites |> select(code, category)) |>
    filter(parameter_name %in% c("Depth", "Temperature", "Baro", "Pressure")) |>
    mutate(parameter_name_modified = case_when(
      parameter_name == "Temperature" & type == "vulink" ~ "Air Temperature",
      parameter_name == "Temperature" ~ "Water Temperature",
      TRUE ~ parameter_name
    )) |>
    # convert units. also, depth readings less than zero are invalid
    mutate(value = case_when(
      parameter_name == "Depth" ~ if_else(value > 0, value / 0.3048, 0),
      parameter_name == "Temperature" ~ value * 9 / 5 + 32,
      parameter_name == "Baro" ~ value,
      # baromerged (gauge) pressure and baro both stay in psi
      parameter_name == "Pressure" ~ value
    )) |>
    select(category, code, site, timestamp, parameter_name_modified, value) |>
    pivot_wider(names_from = parameter_name_modified, values_from = value) |>
    clean_names() |>
    # pivot_wider() orders rows by first appearance, so a timestamp only one
    # device logged (e.g. a VuLink reading with no matching troll reading)
    # lands at the end of its site's rows. Re-sort so the lag()/lead() checks
    # below see neighbors in time order and plotly doesn't draw a line from
    # the series end back to that orphan point.
    arrange(category, code, site, timestamp) |>
    # fill gaps in a piezometer's own Baro reading from the BaroTroll, the
    # source of truth for all piezometers -- so a gap in one device's own
    # Baro channel (e.g. PZ-C3's 2026-02-05 to 2026-03-15 gap) doesn't read
    # as missing data for that date. Stage gages keep their own Baro as-is.
    left_join(barotroll_baro, by = "timestamp") |>
    mutate(baro = if_else(category == "Piezometer", coalesce(baro, baro_fallback), baro)) |>
    select(-baro_fallback) |>
    # if troll is freezing, depth reading is invalid
    group_by(category, code, site) |>
    mutate(depth = if_else((water_temperature > 32) &
                             coalesce(lag(water_temperature) > 32, TRUE),
                           depth, NA)) |>
    ungroup() |>
    # don't show troll temp if there is no water
    mutate(water_temperature = if_else(depth > 0, water_temperature, NA)) |>
    mutate(site = factor(site, levels = unique(sensors$site))) |>
    mutate(timestamp = with_tz(timestamp, "America/Los_Angeles")) |>
    #############
    # LAKE LEVELS
    left_join(ll_data |> select(timestamp, lake_level = value), by = join_by(timestamp)) |>
    ##############################
    # GAGE WATER SURFACE ELEVATION
    left_join(sites |> select(code, twg_elev), by = join_by(code)) |>
    mutate(wse_ft_navd88 = if_else(depth > 0, depth + twg_elev, NA)) |>
    #################################
    # GROUNDWATER DEPTH AND ELEVATION
    # correct piezometer for well depth and calculate piezometer GWE
    inner_join(sensors |> filter(type == "troll") |> select(code, name), by = join_by(code)) |>
    join_piezo_meta() |>
    # mask readings taken while a transducer wasn't submerged (dry well, or
    # pulled to the surface) and re-reference depth to the sensor's air zero
    # with baro adjusted to the transducer's elevation
    mutate(z_baro_ft = baro_meta$z_baro_ft_navd88[1]) |>
    arrange(category, name, timestamp) |>
    mutate(depth_wet = if_else(category == "Piezometer",
                               submerged_depth_ft(pressure, baro, z_baro_ft, tdx_ft_navd88),
                               depth),
           .by = c(category, name)) |>
    mutate(depth = depth_wet,
           water_temperature = if_else(category == "Piezometer" & is.na(depth_wet),
                                       NA, water_temperature)) |>
    select(-depth_wet, -z_baro_ft, -pressure) |>
    # remove piezometer depth readings caused by trolls being pulled from
    # the well for maintenance
    group_by(category, site) |>
    mutate(depth = if_else(category == "Piezometer",
                           clean_piezo_depth(timestamp, depth),
                           depth)) |>
    ungroup() |>
    # calculate groundwater elevation
    # depth of 0 means the water is below the transducer (dry well), so the
    # true groundwater elevation is unknown -- show a gap, not a flatline at
    # the transducer elevation
    mutate(gwe_ft_navd88 = if_else(category == "Piezometer" & depth > 0,
                                   tdx_ft_navd88 + depth,
                                   NA),
           gw_depth_ft = if_else(category == "Piezometer",
                                 gse_ft_navd88 - gwe_ft_navd88,
                                 NA)
           ) |>
    select(-name, -gse_ft_navd88, -tdx_ft_navd88)
}

