library(tidyverse)

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

