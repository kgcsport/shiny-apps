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

# Central shared DB — class-job-market owns the users and token_ledger tables.
# All other apps that need auth or participation tokens use this path.
# On Connect/Shiny Server the env-var/fixed path wins; on local dev we walk up
# to the sibling class-job-market directory.
# When DEMO_MODE=1 the "-demo" variant is used so sandbox data never touches
# the production database.
shared_db_path <- function() {
  demo <- identical(Sys.getenv("DEMO_MODE"), "1")
  db   <- if (demo) "class-job-market-demo.sqlite" else "class-job-market.sqlite"
  r <- Sys.getenv("CONNECT_CONTENT_DIR", "")
  if (nzchar(r)) return(file.path(r, "data", db))
  docker_appdata <- "/srv/shiny-server/appdata"
  if (dir.exists(docker_appdata))
    return(file.path(docker_appdata, "data", db))
  file.path(dirname(normalizePath(getwd())), "class-job-market", "data", db)
}

auction_db_path <- function() {
  demo <- identical(Sys.getenv("DEMO_MODE"), "1")
  db   <- if (demo) "auction-demo.sqlite" else "auction.sqlite"
  r <- Sys.getenv("CONNECT_CONTENT_DIR", "")
  if (nzchar(r)) return(file.path(r, "data", db))
  docker_appdata <- "/srv/shiny-server/appdata"
  if (dir.exists(docker_appdata))
    return(file.path(docker_appdata, "data", db))
  file.path(dirname(normalizePath(getwd())), "supply-auction-game", "data", db)
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
