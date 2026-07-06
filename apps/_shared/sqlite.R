connect_sqlite <- function(path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(path))) {
    stop("SQLite directory does not exist or cannot be created: ", dirname(path))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path, ...)
  harden_sqlite_connection(con)
  con
}

appdata_root <- function(default = getwd()) {
  configured <- Sys.getenv("CONNECT_CONTENT_DIR", "")
  if (nzchar(configured)) return(configured)
  docker_appdata <- "/srv/shiny-server/appdata"
  if (dir.exists(docker_appdata)) return(docker_appdata)
  default
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
