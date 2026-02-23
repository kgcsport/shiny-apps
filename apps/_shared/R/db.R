# apps/_shared/R/db.R
# Shared SQLite connector and schema initialiser.

db_connect_sqlite <- function(path) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    warning("db_connect_sqlite: DBI/RSQLite not available. Returning NULL.")
    return(NULL)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(RSQLite::SQLite(), path)
}

db_init_schema <- function(con) {
  if (is.null(con)) return(invisible(NULL))

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sessions (
      session_id    TEXT PRIMARY KEY,
      game_id       TEXT,
      created_at    TEXT,
      status        TEXT,
      settings_json TEXT
    );
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS players (
      player_id    TEXT PRIMARY KEY,
      session_id   TEXT,
      display_name TEXT,
      created_at   TEXT
    );
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS events (
      event_id     INTEGER PRIMARY KEY AUTOINCREMENT,
      ts           TEXT,
      session_id   TEXT,
      player_id    TEXT,
      event_type   TEXT,
      payload_json TEXT
    );
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS wallet_ledger (
      tx_id      INTEGER PRIMARY KEY AUTOINCREMENT,
      ts         TEXT,
      player_id  TEXT,
      delta_half INTEGER,
      kind       TEXT,
      ref_type   TEXT,
      ref_id     TEXT,
      note       TEXT
    );
  ")

  invisible(con)
}
