# prep_idw_grid.R
library(terra)

prep_idw_grid <- function(
    sites,              # data.frame with id, x, y, type (piezometer|sw_gage), optional weight
    terrain_raster,     # terra raster in same CRS as x,y
    nx = 100,
    ny = 100,
    power = 2,
    pad = 0.02,
    default_tau_minutes = 180,      # default e-folding time (3 hr)
    default_max_age_minutes = 12*60 # default max age (12 hr)
) {
  stopifnot(all(c("id","x","y","type") %in% names(sites)))
  sites$id <- as.character(sites$id)
  if (!"weight" %in% names(sites)) sites$weight <- 1
  
  # set per-site tau / max-age defaults if not present
  if (!"tau_minutes" %in% names(sites)) sites$tau_minutes <- default_tau_minutes
  if (!"max_age_minutes" %in% names(sites)) sites$max_age_minutes <- default_max_age_minutes
  
  # Example sane defaults for sw_gage if not set:
  sw_idx <- which(sites$type == "sw_gage")
  if (length(sw_idx) > 0) {
    if (all(is.na(sites$tau_minutes[sw_idx]))) sites$tau_minutes[sw_idx] <- 90
    if (all(is.na(sites$max_age_minutes[sw_idx]))) sites$max_age_minutes[sw_idx] <- 3*60
  }
  
  # bounding box with padding
  xr <- range(sites$x, na.rm = TRUE)
  yr <- range(sites$y, na.rm = TRUE)
  xr <- xr + diff(xr) * c(-pad, pad)
  yr <- yr + diff(yr) * c(-pad, pad)
  
  gx <- seq(xr[1], xr[2], length.out = nx)
  gy <- seq(yr[1], yr[2], length.out = ny)
  grid <- expand.grid(x = gx, y = gy)
  
  # distances (n_grid x n_sites)
  dx <- outer(grid$x, sites$x, `-`)
  dy <- outer(grid$y, sites$y, `-`)
  dist <- sqrt(dx^2 + dy^2)
  dist[dist == 0] <- 1e-6
  
  # raw inverse-distance weights (apply per-site weight multiplier)
  w_raw <- 1 / (dist^power)
  w_raw <- w_raw * matrix(rep(sites$weight, each = nrow(w_raw)),
                          nrow = nrow(w_raw), ncol = nrow(sites))
  
  # sample terrain at grid points (bilinear)
  ex <- terra::extract(terrain_raster, grid[, c("x","y")], method = "bilinear")
  if (is.data.frame(ex)) {
    terrain_z <- as.numeric(ex[[ncol(ex)]])
  } else {
    terrain_z <- as.numeric(ex)
  }
  
  idw_obj <- list(
    grid = grid,
    x = gx,
    y = gy,
    weights_raw = w_raw,   # keep raw (not normalized) so we can scale columns at runtime
    terrain_z = terrain_z,
    dims = c(nx, ny),
    sites = sites,
    power = power
  )
  
  idw_obj
}

# Example:
# sites <- data.frame(id=c("p1","p2","sw1"), x=c(...), y=c(...), type=c("piezometer","piezometer","sw_gage"))
# r <- rast("terrain.tif")
# idw_obj <- prep_idw_grid(sites, r, nx=120, ny=100)
# saveRDS(idw_obj, "idw_precomputed.rds")
