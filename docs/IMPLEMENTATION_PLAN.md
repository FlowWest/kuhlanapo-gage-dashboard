# Implementation Plan: RDS → PostgreSQL Migration + hydrovur Integration

## Goals

1. Replace raw httr HydroVu API calls in `fetch_hydrovu.R` with the `hydrovur` package
2. Replace all RDS file storage with upserts into an AWS RDS PostgreSQL database
3. Update the Shiny app to read from the database instead of RDS files
4. Update GitHub Actions workflows to push to the database and drop the git-commit-data pattern

---

## Prerequisites

Before executing, confirm:
- [ ] AWS RDS PostgreSQL instance exists and is accessible from GitHub Actions runners and Shiny deployment
- [ ] DB credentials are available (`DB_HOST`, `DB_PORT`, `DB_NAME`, `DB_USER`, `DB_PASSWORD`)
- [ ] GitHub Secrets are set for each workflow environment (see Step 4)
- [ ] Schema initialized: run `sql/schema.sql` against the target DB once
- [ ] `hydrovur` package is installable from `FlowWest/hydrovur`

---

## Step 1 — Rewrite `scripts/fetch_hydrovu.R`

**Goal:** Replace all manual httr/OAuth2/pagination code with `hydrovur` package functions. Push directly to `gage_readings` DB table via `hv_sync()`.

**New script structure:**
```r
# scripts/fetch_hydrovu.R
library(hydrovur)
library(dplyr)
library(DBI)
library(RPostgres)
library(pool)

source(here::here("global.R"))   # sensors, gages, piezos tables

# -- DB connection ---
db <- pool::dbPool(
  drv      = RPostgres::Postgres(),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode  = "require"
)
on.exit(pool::poolClose(db), add = TRUE)

# -- HydroVu connection ---
con <- hv_connect()
all_locs <- hv_locations(con)

# -- Cache location metadata in DB (location_id ↔ name) ---
# This lets the Shiny app resolve IDs without a live HydroVu API call.
locs_for_db <- all_locs |> transmute(location_id, name)
hv_upsert(db, locs_for_db, table = "hydrovu_locations",
           unique_cols = "location_id", quiet = TRUE)

# Filter to sensors defined in global.R
# UPDATE_PIEZOMETERS flag controls whether piezos are included
UPDATE_PIEZOMETERS <- as.logical(Sys.getenv("UPDATE_PIEZOMETERS", "FALSE"))
target_names <- if (UPDATE_PIEZOMETERS) sensors$name else gages$name
sync_locs <- all_locs |> filter(name %in% target_names)

# -- Compute incremental fetch windows from DB state ---
windows <- hv_compute_windows(db, sync_locs,
                               table            = "gage_readings",
                               overlap_days     = 2,
                               fresh_start_days = 14,
                               start_fresh      = as.logical(Sys.getenv("START_FRESH", "FALSE")))

# -- Fetch ---
readings <- hv_readings(con,
                         location_ids = windows$location_id,
                         start        = windows$start_ts,
                         end          = windows$end_ts)

# -- Push to DB ---
hv_sync(db, readings, table = "gage_readings")
```

**What is removed:**
- `get_access_token()`, `isi_paginate()`, `get_locations()`, `get_parameter_names()`, `get_readings()` — all replaced by hydrovur functions
- `existing_data <- readRDS(DATA_FILE)` / `saveRDS()` — no more RDS
- Manual deduplication (`distinct()`) — handled by `hv_sync()` → `hv_validate_readings()`
- `locs_with_window` manual window computation — replaced by `hv_compute_windows()`

**What is kept:**
- `source(here::here("global.R"))` for sensor name lists
- `UPDATE_PIEZOMETERS` flag (converted to env var so GitHub Actions can set it)
- `START_FRESH` flag (also env var)

**New dependencies:** `hydrovur`, `DBI`, `RPostgres`, `pool`
**Removed dependencies:** `httr`, `jsonlite`, `tidyr`, `purrr`, `tibble`, `stringr`

---

## Step 2 — Update `scripts/fetch_usgs_lake_level.R`

**Goal:** Keep existing USGS fetch logic. Replace `saveRDS()` with `hv_upsert()` into `usgs_lake_level` table.

**Changes to the script:**

Add at top (after existing library() calls):
```r
library(DBI)
library(RPostgres)
library(pool)
library(hydrovur)
```

Replace the existing "load from cache" + window computation block with a DB-based window:
```r
# -- DB connection ---
db <- pool::dbPool(
  drv      = RPostgres::Postgres(),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode  = "require"
)
on.exit(pool::poolClose(db), add = TRUE)

# Ensure table exists
DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS usgs_lake_level (
    site_id   TEXT             NOT NULL,
    parm_name TEXT             NOT NULL,
    timestamp TIMESTAMPTZ      NOT NULL,
    value     DOUBLE PRECISION,
    PRIMARY KEY (site_id, parm_name, timestamp)
  )
")

# Compute fetch window from DB state
last_ts_row <- hv_last_timestamps(db, table = "usgs_lake_level",
                                   location_ids = "USGS 11450000")
# Note: hv_last_timestamps uses `location_id` column; remap to site_id in SQL
# Alternative: query directly
last_ts <- tryCatch(
  DBI::dbGetQuery(db, "SELECT MAX(timestamp) AS last_ts FROM usgs_lake_level
                        WHERE site_id = 'USGS 11450000'")$last_ts,
  error = function(e) NULL
)

end_ts <- with_tz(Sys.time(), DATA_TZ)
start_ts <- if (!is.null(last_ts) && !is.na(last_ts)) {
  max(as.POSIXct(last_ts, tz="UTC") - days(OVERLAP_DAYS), MIN_START_DATE)
} else {
  MIN_START_DATE
}
```

Replace `saveRDS()` at the end with:
```r
# Transmute to DB schema (site_id instead of name)
new_data_db <- new_data |>
  transmute(
    site_id   = name,       # "USGS 11450000"
    parm_name = parm_name,  # "Lake Level"
    timestamp = with_tz(timestamp, "UTC"),
    value     = value
  )

hv_upsert(db,
           df          = new_data_db,
           table       = "usgs_lake_level",
           unique_cols = c("site_id", "parm_name", "timestamp"))

message("Upserted ", nrow(new_data_db), " lake level records to DB")
```

**Note on `hv_last_timestamps`:** That function queries by `location_id` column name.
Since `usgs_lake_level` uses `site_id`, query the window directly with `DBI::dbGetQuery()` 
rather than `hv_last_timestamps()`. Alternatively, add a thin wrapper or just inline the SQL.

**What is removed:** `existing_data <- readRDS()`, `bind_rows()` merge, `saveRDS()`

---

## Step 3 — Update `scripts/fetch_nldas_giovanni.R`

**Goal:** Keep existing NASA Giovanni fetch logic. Replace `saveRDS()` with `hv_upsert()` into `nldas_precip` table.

**Changes:**

Add at top:
```r
library(DBI)
library(RPostgres)
library(pool)
library(hydrovur)
```

Add DB connection and table setup (after existing library/config section):
```r
db <- pool::dbPool(
  drv      = RPostgres::Postgres(),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode  = "require"
)
on.exit(pool::poolClose(db), add = TRUE)

DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS nldas_precip (
    site      TEXT             NOT NULL,
    timestamp TIMESTAMPTZ      NOT NULL,
    precip_mm DOUBLE PRECISION,
    precip_in DOUBLE PRECISION,
    PRIMARY KEY (site, timestamp)
  )
")
```

Replace `start_time` computation to use DB state:
```r
last_ts <- DBI::dbGetQuery(db, "SELECT MAX(timestamp) AS last_ts FROM nldas_precip")$last_ts
start_time <- if (!is.null(last_ts) && !is.na(last_ts)) {
  format(as.POSIXct(last_ts, tz = "UTC") - days(2), "%Y-%m-%dT%H:%M:%S")
} else {
  "2025-12-01T00:00:00"
}
```

Replace final `saveRDS()` block with:
```r
precip_db <- precip_ts |>
  transmute(
    site      = site,
    timestamp = with_tz(timestamp, "UTC"),
    precip_mm = precip_mm,
    precip_in = precip_in
  )

hv_upsert(db,
           df          = precip_db,
           table       = "nldas_precip",
           unique_cols = c("site", "timestamp"))

message("Upserted ", nrow(precip_db), " precip records to DB")
```

**What is removed:** `rds_path`, `existing <- readRDS()`, `bind_rows()` merge, `saveRDS()`

---

## Step 4 — Update GitHub Actions Workflows

All three workflows need:
1. New DB secrets added (either a new shared `database` environment or inline secrets)
2. New R package installation (hydrovur, DBI, RPostgres, pool)
3. **Remove** the "Commit updated data" step entirely

### Recommended: Create a shared `database` GitHub environment with secrets:
```
DB_HOST       = <rds-endpoint>.rds.amazonaws.com
DB_PORT       = 5432
DB_NAME       = <dbname>
DB_USER       = <username>
DB_PASSWORD   = <password>
```

### `hydrovu_refresh.yml` changes:
- Add `database` to `environment:` (alongside or replacing `hydrovu`)
  - Or keep `hydrovu` environment and add DB secrets there too
- Replace R packages block:
  ```yaml
  - name: Install R packages
    uses: r-lib/actions/setup-r-dependencies@v2
    with:
      packages: |
        any::remotes
        dplyr
        lubridate
        DBI
        RPostgres
        pool
  - name: Install hydrovur
    run: Rscript -e 'remotes::install_github("FlowWest/hydrovur")'
  ```
- Add DB env vars to the Rscript step:
  ```yaml
  - name: Fetch HydroVu data
    env:
      HYDROVU_CLIENT_ID: ${{ secrets.HYDROVU_CLIENT_ID }}
      HYDROVU_CLIENT_SECRET: ${{ secrets.HYDROVU_CLIENT_SECRET }}
      DB_HOST: ${{ secrets.DB_HOST }}
      DB_PORT: ${{ secrets.DB_PORT }}
      DB_NAME: ${{ secrets.DB_NAME }}
      DB_USER: ${{ secrets.DB_USER }}
      DB_PASSWORD: ${{ secrets.DB_PASSWORD }}
    run: Rscript scripts/fetch_hydrovu.R
  ```
- **Remove** the entire "Commit updated data" step

### `usgs_lake_level_refresh.yml` changes:
- Add DB env vars to the Rscript step (same pattern)
- Add DBI/RPostgres/pool/hydrovur to R packages
- **Remove** "Commit updated data" step

### `refresh_nldas_giovanni.yml` changes:
- Same pattern
- Consider enabling the schedule (currently commented out)
- **Remove** "Commit updated data" step

### System dependencies:
All three workflows will need PostgreSQL client libraries. Add to the "Install system deps" step:
```yaml
- name: Install system deps
  run: sudo apt-get install -y libcurl4-openssl-dev libpq-dev
```
(`libpq-dev` is required for RPostgres to compile)

---

## Step 5 — Update Shiny App (`app.R` + `global.R`)

**Goal:** Replace all file-based data loading with DB queries. Remove `.cache/` directory approach.

### 5a — Create `db.R` (new file)
```r
# db.R — database connection pool for Shiny app
# Sourced from app.R at startup. Exported: `db` (pool::Pool)

suppressPackageStartupMessages({
  library(pool)
  library(RPostgres)
  library(DBI)
})

db <- pool::dbPool(
  drv      = RPostgres::Postgres(),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode  = "require",
  minSize  = 1,
  maxSize  = 5
)

# Graceful pool shutdown when Shiny session ends
shiny::onStop(function() pool::poolClose(db))
```

### 5b — Update `global.R`
Add a `parameter_lookup` tibble for parameter ID → name mapping (static, IDs don't change):

```r
# Parameter ID → parameter name (from hv_parameters() — IDs are stable)
parameter_lookup <- tribble(
  ~parameter_id, ~parm_name,
  "1",           "Pressure",
  "2",           "Temperature",
  "4",           "Depth",
  "17",          "Baro",
)
```

The `location_lookup` (HydroVu `location_id` → sensor `name` → `code`/metadata) is built
at Shiny startup by querying the `hydrovu_locations` DB table — see Step 5c. No hardcoding
and no HydroVu API call needed in the app.

### 5c — Update `app.R` data loading

Replace the current `ts_data()`, `ll_data()`, and `precip_data()` reactive logic.

**Remove:**
- All `.cache/` file logic (`cache_dir`, `cache_file`, `is_stale`, `lock_file`, etc.)
- `FORCE_LOCAL` flag and GitHub URL fallback
- `future::future()` async refresh
- `readRDS()` calls

**At startup (global scope, outside server function), build the location lookup from DB:**

```r
# At top of app.R, source db.R and global.R
source("global.R")
source("db.R")

# Build location_lookup once at startup: DB hydrovu_locations + sensors from global.R
# fetch_hydrovu.R keeps hydrovu_locations current, so no HydroVu API call needed here.
location_lookup <- DBI::dbGetQuery(db,
  "SELECT location_id, name FROM hydrovu_locations"
) |>
  as_tibble() |>
  inner_join(sensors |> select(name, code, type, site), by = "name")
```

**Replace with DB queries inside reactives:**

```r
# -- HydroVu readings --
ts_data <- reactive({
  date_range <- input$date_range   # c(start_date, end_date)
  
  DBI::dbGetQuery(db, "
    SELECT location_id, parameter_id, unit_id, timestamp, value
    FROM gage_readings
    WHERE timestamp >= $1 AND timestamp <= $2
  ", params = list(
    as.POSIXct(date_range[1], tz = "UTC"),
    as.POSIXct(date_range[2], tz = "UTC") + days(1)
  )) |>
    as_tibble() |>
    inner_join(location_lookup, by = "location_id") |>
    inner_join(parameter_lookup, by = "parameter_id") |>
    mutate(timestamp = with_tz(timestamp, "America/Los_Angeles"))
})

# -- USGS lake level --
ll_data <- reactive({
  date_range <- input$date_range
  
  DBI::dbGetQuery(db, "
    SELECT site_id AS name, timestamp, parm_name, value
    FROM usgs_lake_level
    WHERE timestamp >= $1 AND timestamp <= $2
  ", params = list(
    as.POSIXct(date_range[1], tz = "UTC"),
    as.POSIXct(date_range[2], tz = "UTC") + days(1)
  )) |>
    as_tibble() |>
    mutate(timestamp = with_tz(timestamp, "America/Los_Angeles"))
})

# -- Precipitation --
precip_data <- reactive({
  date_range <- input$date_range
  
  DBI::dbGetQuery(db, "
    SELECT site, timestamp, precip_mm, precip_in
    FROM nldas_precip
    WHERE timestamp >= $1 AND timestamp <= $2
  ", params = list(
    as.POSIXct(date_range[1], tz = "UTC"),
    as.POSIXct(date_range[2], tz = "UTC") + days(1)
  )) |>
    as_tibble() |>
    mutate(timestamp = with_tz(timestamp, "America/Los_Angeles"))
})
```

**Column name reconciliation:**

The `df_pivot()` reactive and downstream plot code currently uses column names from the
old RDS schema. After the DB migration the column names will be slightly different.
The joins above restore compatibility, but verify these mappings:

| Old column (RDS) | New source | Notes |
|-----------------|-----------|-------|
| `name` | `sensors$name` via `location_lookup` | HydroVu sensor name e.g. "2025SGMC01" |
| `code` | `sensors$code` via `location_lookup` | Short code e.g. "MC-01" |
| `type` | `sensors$type` via `location_lookup` | "troll", "vulink", "barotroll" |
| `site` | `sensors$site` via `location_lookup` | Human-readable site name |
| `parameterId` | `parameter_id` from DB | Rename: `parameter_id → parameterId` |
| `unitId` | `unit_id` from DB | Rename: `unit_id → unitId` |
| `parm_name` | from `parameter_lookup` join | "Depth", "Temperature", etc. |
| `timestamp` | from DB, converted to PT | Same semantics |
| `value` | from DB | Same semantics |

Add `rename(parameterId = parameter_id, unitId = unit_id)` after the join, or update
all downstream references in `df_pivot()` to use snake_case.

---

## Step 6 — Update Deployment Environment

### shinyapps.io / Posit Connect
Set the DB environment variables in the deployment settings:
- `DB_HOST`
- `DB_PORT`
- `DB_NAME`
- `DB_USER`
- `DB_PASSWORD`

### R package requirements for Shiny app
Add to the deployment requirements: `pool`, `RPostgres`, `DBI`, `hydrovur`

---

## Step 7 — Cleanup

After verifying the DB pipeline is working:
- Remove `data/gage_data.rds`, `data/usgs_lake_level_11450000.rds`, `data/precip_ts.rds`
  from the repo (add to `.gitignore`)
- Remove `.cache/` directory from `.gitignore` (or keep for local dev)
- Remove `FORCE_LOCAL` flag from `app.R` (or repurpose as a local SQLite dev mode)

---

## Testing Checklist

- [ ] Run `fetch_hydrovu.R` locally with DB env vars set → verify rows in `gage_readings`
- [ ] Run `fetch_usgs_lake_level.R` locally → verify rows in `usgs_lake_level`
- [ ] Run `fetch_nldas_giovanni.R` locally → verify rows in `nldas_precip`
- [ ] Run each workflow via `workflow_dispatch` → verify DB updated, no git commit
- [ ] Run Shiny app locally with DB env vars → verify all three plots render
- [ ] Verify idempotency: run each script twice → row count unchanged (upsert works)
- [ ] Verify incremental sync: check that `hv_compute_windows` respects existing DB state

---

## Risk Notes

1. **Column name breakage in app.R**: The `df_pivot()` reactive and all `ggplot`/`plotly`
   calls use column names from the old schema. Carefully map each usage when updating app.R.
   The most likely breakage points: `parameterId`, `unitId`, `name` (sensor name vs site name).

2. **`location_id` is numeric in DB, text in R**: `hv_ensure_table` stores `location_id` as
   TEXT. The actual values are numeric strings (e.g., `"12345"`). Joins must coerce types.

3. **Timezone handling**: DB stores UTC; app displays Pacific. The `with_tz()` conversion
   must happen after the DB query, not before writing.

4. **First-run bootstrap**: On a fresh DB, `hv_compute_windows()` will use `fresh_start_days = 14`
   for all locations. This means only the last 14 days are fetched on the first run.
   To load full history from the existing RDS files into the DB, write a one-time migration
   script that reads `data/gage_data.rds` and runs `hv_sync()` on it.

5. **inldata dependency (USGS script)**: The `inldata` package is installed from a USGS
   git server. This step is slow (~2 min) in GitHub Actions. Consider caching with
   `actions/cache` keyed on the package version.

---

## One-Time Data Migration (Optional)

To backfill history from existing RDS files into the DB before switching over:

```r
# migrate_rds_to_db.R  (run once locally, not checked in)
library(hydrovur)
library(dplyr)
library(DBI)
library(RPostgres)
library(pool)

db <- pool::dbPool(
  drv      = RPostgres::Postgres(),
  host     = Sys.getenv("DB_HOST"),
  ...
)

# -- HydroVu history --
# Note: old RDS uses 'id' not 'location_id', and includes extra columns
# hv_sync() / hv_validate_readings() will project to the right schema
old_gage <- readRDS("data/gage_data.rds") |>
  transmute(
    location_id  = as.character(id),
    parameter_id = as.character(parameterId),
    unit_id      = as.character(unitId),
    timestamp    = with_tz(timestamp, "UTC"),
    value        = value
  )
hv_sync(db, old_gage, table = "gage_readings", batch_size = 5000L)

# -- USGS history --
old_usgs <- readRDS("data/usgs_lake_level_11450000.rds") |>
  transmute(
    site_id   = name,
    parm_name = parm_name,
    timestamp = with_tz(timestamp, "UTC"),
    value     = value
  )
hv_upsert(db, old_usgs, "usgs_lake_level", c("site_id", "parm_name", "timestamp"))

# -- NLDAS history --
old_precip <- readRDS("data/precip_ts.rds") |>
  transmute(
    site      = site,
    timestamp = with_tz(timestamp, "UTC"),
    precip_mm = precip_mm,
    precip_in = precip_in
  )
hv_upsert(db, old_precip, "nldas_precip", c("site", "timestamp"))
```
