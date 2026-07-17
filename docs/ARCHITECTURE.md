# Kuhlanapo Gage Dashboard — Architecture

## Overview

This Shiny dashboard visualizes stream stage, groundwater, and precipitation data for the Kuhlanapo (Big Valley Rancheria) wetland restoration site near Clear Lake, CA. Data comes from three sources:

| Source | Description | Update cadence |
|--------|-------------|----------------|
| HydroVu API | 3 stream stage gages + 11 piezometers | Daily ~07:00 PT |
| USGS NWIS | Clear Lake level (site 11450000) | Daily ~07:30 PT |
| NASA Giovanni / NLDAS | Hourly precipitation at 2 grid cells | Manual / ad-hoc |

---

## Repository Structure

```
kuhlanapo-gage-dashboard/
├── app.R                         Main Shiny application
├── global.R                      Site/sensor metadata, color palettes
│
├── scripts/
│   ├── fetch_hydrovu.R           HydroVu API → database
│   ├── fetch_usgs_lake_level.R   USGS NWIS API → database
│   └── fetch_nldas_giovanni.R    NASA Giovanni API → database
│
├── sql/
│   └── schema.sql                DDL for all three database tables
│
├── data/                         (legacy) RDS files used before DB migration
│   ├── gage_data.rds
│   ├── usgs_lake_level_11450000.rds
│   ├── precip_ts.rds
│   └── basin.geojson             Static basin boundary (still used)
│
├── data-raw/
│   ├── report_figures.Rmd        Ad-hoc reporting notebook
│   └── timeseries_giovanni/      Local CSV fallback for NLDAS data
│
└── .github/workflows/
    ├── hydrovu_refresh.yml       Daily HydroVu sync job
    ├── usgs_lake_level_refresh.yml  Daily USGS sync job
    └── refresh_nldas_giovanni.yml   Manual NLDAS sync job
```

---

## Data Pipeline

### Current (RDS-file based)
```
GitHub Actions (daily cron)
  └─ fetch_*.R scripts
       ├─ Pull data from API
       ├─ Merge with existing data/gage_data.rds
       ├─ Deduplicate
       └─ saveRDS() → commit to repo

Shiny app (shinyapps.io)
  └─ Reads .rds files from:
       1. local data/ (FORCE_LOCAL)
       2. .cache/ directory (24h TTL)
       3. GitHub raw URL fallback
```

### Planned (PostgreSQL / AWS RDS)
```
GitHub Actions (daily cron)
  └─ fetch_*.R scripts
       ├─ Pull data from API
       ├─ hv_upsert() / hv_sync() → AWS RDS PostgreSQL
       └─ No git commit of data files

Shiny app (shinyapps.io or Posit Connect)
  └─ pool::dbPool() → AWS RDS PostgreSQL
       ├─ Query gage_readings (filtered by date range)
       ├─ Query usgs_lake_level
       └─ Query nldas_precip
```

---

## Database Schema

See [`sql/schema.sql`](../sql/schema.sql) for the full DDL.

### `hydrovu_locations` (metadata)
| Column | Type | Notes |
|--------|------|-------|
| location_id | TEXT PK | HydroVu numeric ID (as text) |
| name | TEXT | HydroVu sensor name, e.g. "2025SGMC01" |

Upserted by `fetch_hydrovu.R` on every run via `hv_locations()`. The Shiny app reads
this table at startup to build the `location_id → name → code` lookup without needing
a live HydroVu API connection.

### `gage_readings` (HydroVu)
| Column | Type | Notes |
|--------|------|-------|
| location_id | TEXT PK | HydroVu numeric ID (as text) |
| parameter_id | TEXT PK | HydroVu parameter ID; see parameter lookup below |
| unit_id | TEXT | HydroVu unit ID |
| timestamp | TIMESTAMPTZ PK | UTC |
| value | DOUBLE PRECISION | Raw sensor reading in HydroVu native units |

Managed by `hydrovur::hv_ensure_table()` / `hydrovur::hv_sync()`.

**Parameter lookup** (from `hv_parameters(con)` or hardcoded in global.R):
- `"1"` → Pressure
- `"2"` → Temperature (°C)
- `"4"` → Depth (m)
- `"17"` → Barometric Pressure

**Location lookup**: `fetch_hydrovu.R` upserts all HydroVu locations into the `hydrovu_locations` DB table each run. The Shiny app queries this table at startup to map `location_id` → `name` (e.g., `"2025SGMC01"`), then joins with `sensors` in `global.R` to get `code` (e.g., `"MC-01"`). No hardcoding or live API call needed in the app.

### `usgs_lake_level` (USGS NWIS)
| Column | Type | Notes |
|--------|------|-------|
| site_id | TEXT PK | `"USGS 11450000"` |
| parm_name | TEXT PK | `"Lake Level"` |
| timestamp | TIMESTAMPTZ PK | In Pacific time, stored as UTC |
| value | DOUBLE PRECISION | Lake surface elevation, ft NAVD88 |

### `nldas_precip` (NASA Giovanni)
| Column | Type | Notes |
|--------|------|-------|
| site | TEXT PK | `"KPD"` or `"UMC"` |
| timestamp | TIMESTAMPTZ PK | UTC |
| precip_mm | DOUBLE PRECISION | kg/m² from NLDAS FORA 2.0 |
| precip_in | DOUBLE PRECISION | precip_mm / 25.4 |

---

## Sensor / Site Metadata (global.R)

All sensor metadata is maintained in `global.R`. The `sensors` tibble maps HydroVu
sensor names to site codes and categories:

```
HydroVu name  → code  → category
2025SGMC01    → MC-01 → Stage Gage   (Lower Manning Creek, elev 1327.83 ft)
2025SGMC03    → MC-03 → Stage Gage   (Upper Manning Creek, elev 1329.39 ft)
2025SGMC02    → MC-02 → Stage Gage   (Secondary Channel,   elev 1331.89 ft)
2025PZA01..03 → PZ-A1..A3 → Piezometer (Transect A)
2025PZB01..04 → PZ-B1..B4 → Piezometer (Transect B)
2025PZC01..03 → PZ-C1..C3 → Piezometer (Transect C)
2025BAROK01   → PZ-B4 → Barotroll (barometric pressure compensation)
```

Each piezometer also has:
- `gse_ft_navd88`: ground surface elevation (for GW depth calculation)
- `tdx_ft_navd88`: transducer elevation (for GW elevation calculation)

---

## Shiny App (app.R)

### Data views
- **Depth**: Raw sensor depth (ft)
- **Water Surface**: WSE = TDX elevation + depth (ft NAVD88)
- **GW Elev**: Groundwater elevation = same calculation, piezometers only
- **GW Depth**: Ground surface elevation − GW elevation (ft)

### Plot layout
- Y-axis 1: Selected metric (stage gages or piezometers, by transect)
- Y-axis 2: Water temperature + air temperature (°C → °F)
- Y-axis 3: Hourly precipitation bar chart (right side)
- Lake level reference band (1320.74–1328.30 ft NAVD88) overlaid on WSE/GW views

### Caching
Currently: file-based cache in `.cache/` with 24h TTL, async refresh via `future`.
After migration: DB queries with session-level reactive caching (no file cache needed).

---

## Environment Variables

### For fetch scripts (GitHub Actions secrets)
| Variable | Used by | Description |
|----------|---------|-------------|
| `HYDROVU_CLIENT_ID` | fetch_hydrovu.R | HydroVu OAuth2 client ID |
| `HYDROVU_CLIENT_SECRET` | fetch_hydrovu.R | HydroVu OAuth2 client secret |
| `EARTHDATA_TOKEN` | fetch_nldas_giovanni.R | NASA Earthdata Bearer token |
| `DB_HOST` | all fetch scripts | PostgreSQL host (AWS RDS endpoint) |
| `DB_PORT` | all fetch scripts | PostgreSQL port (default 5432) |
| `DB_NAME` | all fetch scripts | Database name |
| `DB_USER` | all fetch scripts | Database user |
| `DB_PASSWORD` | all fetch scripts | Database password |

### For Shiny app (deployment environment)
Same `DB_*` variables as above. Set in shinyapps.io environment or Posit Connect vars.

### Optional (AWS Secrets Manager path)
If `SECRET_MANAGER_SECRET_NAME` is set, the hydrovur `sync-job.R` pattern can be used
to fetch DB credentials from AWS Secrets Manager instead of individual env vars.

---

## hydrovur Package

Used for all HydroVu-related operations. Install from GitHub:
```r
remotes::install_github("FlowWest/hydrovur")
```

Key functions used by this project:
| Function | Where used | Purpose |
|----------|-----------|---------|
| `hv_connect()` | fetch_hydrovu.R, app.R | OAuth2 auth + cache locations/params |
| `hv_locations()` | fetch_hydrovu.R, app.R | Location ID → name lookup |
| `hv_parameters()` | app.R | Parameter ID → name lookup |
| `hv_readings()` | fetch_hydrovu.R | Fetch sensor readings |
| `hv_ensure_table()` | fetch_hydrovu.R | Create gage_readings table |
| `hv_compute_windows()` | fetch_hydrovu.R | Incremental fetch windows from DB |
| `hv_sync()` | fetch_hydrovu.R | Validate + upsert readings |
| `hv_upsert()` | fetch_usgs_lake_level.R, fetch_nldas_giovanni.R | Upsert arbitrary table |
| `hv_last_timestamps()` | fetch_usgs_lake_level.R, fetch_nldas_giovanni.R | Last timestamp per source |
