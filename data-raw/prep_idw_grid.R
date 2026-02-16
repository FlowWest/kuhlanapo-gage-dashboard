# prep_idw_grid.R
library(terra)
library(tidyverse)
library(sf)
source(here::here("global.R"))
# STEP 1 PREP IDW GRID

prep_idw_grid <- function(
    sites_df,
    nx = 100,
    ny = 100,
    power = 3,
    pad = 0,
    radius = 2000
) {
  stopifnot(all(c("id","x","y","type") %in% names(sites_df)))
  sites_df$id <- as.character(sites_df$id)
  if (!"weight" %in% names(sites_df)) sites_df$weight <- 1
  
  # set per-site tau / max-age defaults if not present
  if (!"tau_minutes" %in% names(sites_df)) sites_df$tau_minutes <- default_tau_minutes
  if (!"max_age_minutes" %in% names(sites_df)) sites_df$max_age_minutes <- default_max_age_minutes
  
  # Example sane defaults for sw_gage if not set:
  sw_idx <- which(sites_df$type == "sw_gage")
  if (length(sw_idx) > 0) {
    if (all(is.na(sites_df$tau_minutes[sw_idx]))) sites_df$tau_minutes[sw_idx] <- 90
    if (all(is.na(sites_df$max_age_minutes[sw_idx]))) sites_df$max_age_minutes[sw_idx] <- 3*60
  }
  
  # bounding box with padding
  xr <- range(sites_df$x, na.rm = TRUE)
  yr <- range(sites_df$y, na.rm = TRUE)
  xr <- xr + diff(xr) * c(-pad, pad)
  yr <- yr + diff(yr) * c(-pad, pad)
  
  gx <- seq(xr[1], xr[2], length.out = nx)
  gy <- seq(yr[1], yr[2], length.out = ny)
  grid <- expand.grid(x = gx, y = gy)
  
  # distances (n_grid x n_sites_df)
  dx <- outer(grid$x, sites_df$x, `-`)
  dy <- outer(grid$y, sites_df$y, `-`)
  dist <- sqrt(dx^2 + dy^2)
  dist[dist == 0] <- 1e-6
  
  # raw inverse-distance weights (apply per-site weight multiplier)
  # raw inverse-distance weights
  w_raw <- 1 / (dist^power)
  
  radial_decay <- pmax(0, 1 - dist / radius)
  radial_decay[dist > radius] <- 0
  
  w_raw <- w_raw * radial_decay
  
  # sample terrain at grid points (bilinear)
  # ex <- terra::extract(terrain_raster, grid[, c("x","y")], method = "bilinear")
  # if (is.data.frame(ex)) {
  #   terrain_z <- as.numeric(ex[[ncol(ex)]])
  # } else {
  #   terrain_z <- as.numeric(ex)
  # }
  
  min_dist <- apply(dist, 1, min)
  
  idw_obj <- list(
    grid = grid,
    x = gx,
    y = gy,
    weights_raw = w_raw,   # keep raw (not normalized) so we can scale columns at runtime
    # terrain_z = terrain_z,
    dims = c(nx, ny),
    sites = sites_df,
    power = power,
    min_dist = min_dist
  )
  
  idw_obj
}

sites_df <- tribble(~code,     ~lat,       ~lon
                    , "PZ-A1", 39.024116 , -122.900865
                    , "PZ-A2", 39.021917 , -122.900821
                    , "PZ-A3", 39.019087 , -122.900779
                    , "PZ-B1", 39.026017 , -122.905737
                    , "PZ-B2", 39.024199 , -122.905617
                    , "PZ-B3", 39.021014 , -122.905264
                    , "PZ-B4", 39.018534 , -122.905164
                    , "PZ-C1", 39.023875 , -122.908935
                    , "PZ-C2", 39.023570 , -122.910722
                    , "PZ-C3", 39.022957 , -122.913734
) |> 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") |>
  st_transform("EPSG:6418") %>%
  bind_cols(st_coordinates(.$geometry)) |>
  st_drop_geometry() |>
  transmute(id = code, x = X, y = Y, type = "piezometer")

idw_obj <- prep_idw_grid(sites_df)

saveRDS(idw_obj, here::here("data-raw", "idw_precomputed.rds"))

# GET TS DATA

df_pivot <- gage_data |>
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
  janitor::clean_names() |>
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
  # GAGE WATER SURFACE ELEVATION
  left_join(sites |> select(code, twg_elev), by=join_by(code)) |>
  mutate(wse_ft_navd88 = if_else(depth > 0, depth + twg_elev, NA)) |>
  #################################
# GROUNDWATER DEPTH AND ELEVATION
# correct piezometer for well depth and calculate piezometer GWE
inner_join(sensors |> filter(type == "troll") |> select(code, name), by = join_by(code)) |>
  left_join(piezo_meta |> select(name, gse_ft_navd88, tdx_ft_navd88), by = join_by(name)) |>
  # smooth spikes of length one in groundwater depth, eliminate other spikes
  group_by(category, site) |>
  mutate(depth = if_else(category == "Piezometer",
                         case_when((timestamp == min(timestamp)) ~ lead(depth),
                                   (abs(depth - lag(depth)) > 3) & (abs(depth - lead(depth)) > 3) ~ (lag(depth) + lead(depth)) / 2,
                                   (depth > 18) ~ NA,
                                   TRUE ~ depth),
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

ts_data <- df_pivot |>
  filter(category == "Piezometer") |>
  transmute(code, timestamp, value = gwe_ft_navd88)

# preview ts data
ts_data |> ggplot() + geom_line(aes(x = timestamp, y = value, color = code))

# STEP 2 INTERPOLATION

out <- interpolate_idw_at_time(idw_obj, 
                               ts_data, 
                               as.POSIXct("2025-12-31 24:00", tz = "America/Los_Angeles"),
                               return_matrix = F) 

out_grid <- idw_obj$grid |>
  mutate(value = as.numeric(out))

ggplot(out_grid, aes(x = x, y = y)) +
  # geom_raster(aes(fill = value), interpolate = TRUE) +
  geom_contour(aes(z = value, color = after_stat(level)),
               linewidth = 1,
               binwidth = 1,
               na.rm = TRUE)  +
  metR::geom_label_contour(aes(z = value, label = after_stat(level), color = after_stat(level)),
               binwidth = 1,
               skip = 0,
               size = 8 / .pt,
               na.rm = TRUE)  +
  geom_point(data=sites_df, aes(fill = id), size = 6, shape = 21) +
  geom_text(data=sites_df, aes(label = substr(id, 4, 5)), size = 8 / .pt) +
  coord_equal() +
  #scale_fill_continuous(
  #  name = "Interpolated\nValue",
  #  na.value = "transparent"
  #) +
  scale_fill_manual(name = "", values = piezo_colors) +
  scale_color_viridis_c(name = "ft NAVD88") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

bnd <- read_sf(here::here("data-raw", "kuulanapo_bnd.shp.zip")) 
st_coordinates(bnd$geometry)[, c("X", "Y")] |> 
  saveRDS(here::here("data", "kuhlanapo_bnd.rds"))
