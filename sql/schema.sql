-- kuhlanapo-gage-dashboard database schema
-- PostgreSQL / AWS RDS
--
-- Four tables:
--   hydrovu_locations  HydroVu location metadata (ID ↔ name, updated each fetch)
--   gage_readings      HydroVu sensor data (managed via hydrovur::hv_ensure_table)
--   usgs_lake_level    USGS NWIS lake level for site 11450000
--   nldas_precip       NASA NLDAS hourly precipitation (Giovanni API)
--
-- All timestamps stored as TIMESTAMPTZ (UTC).
-- Run this script once against the target database to initialize.

-- ── HydroVu location metadata ────────────────────────────────────────────────
-- Populated (upserted) by scripts/fetch_hydrovu.R each run via hv_locations().
-- Lets the Shiny app resolve location_id → name without a live HydroVu API call.
-- Updated automatically whenever new sensors are added to the HydroVu account.
--
-- location_id: HydroVu numeric ID (as TEXT) — matches gage_readings.location_id
-- name       : HydroVu sensor name, e.g. "2025SGMC01" — cross-ref with global.R sensors

CREATE TABLE IF NOT EXISTS hydrovu_locations (
  location_id TEXT NOT NULL PRIMARY KEY,
  name        TEXT NOT NULL
);

-- ── HydroVu sensor readings ───────────────────────────────────────────────────
-- Populated by scripts/fetch_hydrovu.R via hydrovur::hv_sync()
-- hydrovur::hv_ensure_table() creates this table if it does not exist,
-- so this DDL is kept here for reference and manual bootstrapping only.
--
-- location_id : HydroVu numeric location ID (as TEXT)
-- parameter_id: HydroVu parameter ID (as TEXT); join with hv_parameters()
--               for human-readable names (Depth, Temperature, Pressure, Baro)
-- unit_id     : HydroVu unit ID (as TEXT)

CREATE TABLE IF NOT EXISTS gage_readings (
  location_id  TEXT             NOT NULL,
  parameter_id TEXT             NOT NULL,
  unit_id      TEXT,
  timestamp    TIMESTAMPTZ      NOT NULL,
  value        DOUBLE PRECISION,
  PRIMARY KEY (location_id, parameter_id, timestamp)
);

CREATE INDEX IF NOT EXISTS gage_readings_ts_idx
  ON gage_readings (timestamp DESC);

CREATE INDEX IF NOT EXISTS gage_readings_loc_ts_idx
  ON gage_readings (location_id, timestamp DESC);

-- ── USGS lake level ───────────────────────────────────────────────────────────
-- Populated by scripts/fetch_usgs_lake_level.R via hydrovur::hv_upsert()
--
-- site_id  : USGS site identifier string, e.g. "USGS 11450000"
-- parm_name: USGS parameter name, e.g. "Lake Level"
-- value    : lake surface elevation (ft NAVD88)

CREATE TABLE IF NOT EXISTS usgs_lake_level (
  site_id   TEXT             NOT NULL,
  parm_name TEXT             NOT NULL,
  timestamp TIMESTAMPTZ      NOT NULL,
  value     DOUBLE PRECISION,
  PRIMARY KEY (site_id, parm_name, timestamp)
);

CREATE INDEX IF NOT EXISTS usgs_lake_level_ts_idx
  ON usgs_lake_level (timestamp DESC);

-- ── NLDAS / NASA Giovanni precipitation ──────────────────────────────────────
-- Populated by scripts/fetch_nldas_giovanni.R via hydrovur::hv_upsert()
--
-- site     : Short site code, "KPD" or "UMC"
--            KPD = Kuhlanapo at Manning Creek Delta  (39.024924, -122.906493)
--            UMC = Upper Manning Creek at Hwy 175    (38.995516, -122.934703)
-- precip_mm: Hourly precipitation in millimetres (kg/m² from NLDAS FORA 2.0)
-- precip_in: Same converted to inches (precip_mm / 25.4)

CREATE TABLE IF NOT EXISTS nldas_precip (
  site      TEXT             NOT NULL,
  timestamp TIMESTAMPTZ      NOT NULL,
  precip_mm DOUBLE PRECISION,
  precip_in DOUBLE PRECISION,
  PRIMARY KEY (site, timestamp)
);

CREATE INDEX IF NOT EXISTS nldas_precip_ts_idx
  ON nldas_precip (timestamp DESC);
