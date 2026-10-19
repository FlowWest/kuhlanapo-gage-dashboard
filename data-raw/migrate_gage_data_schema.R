# data-raw/migrate_gage_data_schema.R
#
# One-off migration of data/gage_data.rds from the hand-rolled HydroVu schema
# to the standardized hydrovur schema used by scripts/fetch_hydrovu.R:
#
#   id          (dbl) -> location_id    (chr)
#   parameterId       -> parameter_id
#   unitId            -> unit_id
#   parm_name         -> parameter_name
#
# The location metadata columns (description, gps.latitude, gps.longitude)
# are dropped -- hydrovur doesn't carry them and nothing downstream uses them.
# Safe to re-run: a file already in the new schema is left untouched.
library(dplyr)

DATA_FILE <- here::here("data/gage_data.rds")

SCHEMA_COLS <- c("location_id", "name", "code", "site", "type", "category",
                 "parameter_id", "parameter_name", "unit_id", "timestamp", "value")

old <- readRDS(DATA_FILE)

if ("location_id" %in% names(old)) {
  message("Already in hydrovur schema; nothing to do.")
} else {
  # HydroVu location ids are < 2^53, so the double -> character conversion is
  # exact; "%.0f" avoids the scientific notation as.character() would give
  stopifnot(all(old$id < 2^53))

  new <- old |>
    mutate(location_id = sprintf("%.0f", id),
           timestamp   = lubridate::with_tz(timestamp, "UTC")) |>
    rename(parameter_id = parameterId, unit_id = unitId, parameter_name = parm_name) |>
    select(all_of(SCHEMA_COLS)) |>
    arrange(name, timestamp)

  hydrovur::hv_validate_readings(new, schema_cols = SCHEMA_COLS)
  stopifnot(nrow(new) == nrow(old),
            identical(new$value, arrange(old, name, timestamp)$value))

  saveRDS(new, DATA_FILE)
  message("Migrated ", format(nrow(new), big.mark = ","), " rows to hydrovur schema.")
}
