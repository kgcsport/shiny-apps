connect_sqlite <- function(path, ...) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path, ...)
  harden_sqlite_connection(con)
  con
}

harden_sqlite_connection <- function(con) {
  # WAL allows concurrent readers while one writer is active. The busy timeout
  # makes brief write contention wait instead of immediately failing.
  try(DBI::dbExecute(con, "PRAGMA journal_mode = WAL;"), silent = TRUE)
  try(DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;"), silent = TRUE)
  try(DBI::dbExecute(con, "PRAGMA synchronous = NORMAL;"), silent = TRUE)
  try(DBI::dbExecute(con, "PRAGMA foreign_keys = ON;"), silent = TRUE)
  invisible(con)
}
