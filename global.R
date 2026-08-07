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
  ~name,        ~gse_ft_navd88,  ~tdx_ft_navd88,
  "2025PZA01",  1329.250,        1323.550,
  "2025PZA02",  1326.214,        1321.964,
  "2025PZA03",  1328.249,        1321.604,
  "2025PZB01",  1326.417,        1322.147,
  "2025PZB02",  1326.019,        1323.079,
  "2025PZB03",  1329.669,        1322.189,
  "2025PZB04",  1331.618,        1322.918,
  "2025PZC01",  1330.484,        1322.609,
  "2025PZC02",  1328.072,        1323.287,
  "2025PZC03",  1333.552,        1327.207,
) 

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

# Null out piezometer depth readings that don't line up with their nearest
# in-time neighbor -- caused by trolls being pulled from the well for
# periodic data download/recalibration, which shows up as either a single
# round-trip dip/spike or a step change with one bad transition reading.
# A neighbor only counts if it's within `max_gap_min`, so genuine drift
# across a real data gap (sensor offline, not just misreading) isn't flagged.
# `timestamp`/`depth` must be ordered by time within one piezometer's series.
clean_piezo_depth <- function(timestamp, depth, hard_max = 18,
                               jump_thresh = 1, max_gap_min = 60) {
  dt_lag  <- as.numeric(difftime(timestamp, lag(timestamp),  units = "mins"))
  dt_lead <- as.numeric(difftime(lead(timestamp), timestamp, units = "mins"))
  lag_d   <- lag(depth)
  lead_d  <- lead(depth)

  close_lag  <- !is.na(dt_lag)  & dt_lag  <= max_gap_min
  close_lead <- !is.na(dt_lead) & dt_lead <= max_gap_min

  ok_via_lag  <- close_lag  & !is.na(lag_d)  & lag_d  <= hard_max & abs(depth - lag_d)  <= jump_thresh
  ok_via_lead <- close_lead & !is.na(lead_d) & lead_d <= hard_max & abs(depth - lead_d) <= jump_thresh
  no_close_neighbor <- !close_lag & !close_lead

  case_when(
    depth > hard_max ~ NA_real_,
    no_close_neighbor ~ depth,
    !ok_via_lag & !ok_via_lead ~ NA_real_,
    TRUE ~ depth
  )
}

# Build the cleaned, pivoted gage/piezometer time series used by both app.R
# and data-raw/report_figures.Rmd. Keeping this in one place means the two
# can never drift out of sync the way the piezometer depth cleaning did.
# `ts_data`/`ll_data` must already be resolved data frames (call reactives
# like `ts_data()` before passing them in).
build_df_pivot <- function(ts_data, ll_data) {
  ts_data |>
    inner_join(sites |> select(code, category)) |>
    filter(parm_name %in% c("Depth", "Temperature")) |>
    mutate(parm_name_modified = case_when(
      parm_name == "Temperature" & type == "vulink" ~ "Air Temperature",
      parm_name == "Temperature" ~ "Water Temperature",
      TRUE ~ parm_name
    )) |>
    # convert units. also, depth readings less than zero are invalid
    mutate(value = case_when(
      parm_name == "Depth" ~ if_else(value > 0, value / 0.3048, 0),
      parm_name == "Temperature" ~ value * 9 / 5 + 32
    )) |>
    select(category, code, site, timestamp, parm_name_modified, value) |>
    pivot_wider(names_from = parm_name_modified, values_from = value) |>
    clean_names() |>
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
    left_join(piezo_meta |> select(name, gse_ft_navd88, tdx_ft_navd88), by = join_by(name)) |>
    # remove piezometer depth readings caused by trolls being pulled from
    # the well for maintenance
    group_by(category, site) |>
    mutate(depth = if_else(category == "Piezometer",
                           clean_piezo_depth(timestamp, depth),
                           depth)) |>
    ungroup() |>
    # calculate groundwater elevation
    mutate(gwe_ft_navd88 = if_else(category == "Piezometer",
                                   tdx_ft_navd88 + depth,
                                   NA),
           gw_depth_ft = if_else(category == "Piezometer",
                                 gse_ft_navd88 - gwe_ft_navd88,
                                 NA)
           ) |>
    select(-name, -gse_ft_navd88, -tdx_ft_navd88)
}

