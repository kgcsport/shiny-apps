# apps/core-arcade/R/db.R
# App-level DB connector: resolves path, connects, initialises schema.

core_db_connect <- function() {
  shared_r <- here_core_arcade("../_shared/R")
  source(file.path(shared_r, "utils.R"), local = FALSE)
  source(file.path(shared_r, "db.R"),    local = FALSE)

  path <- db_file_path("core-arcade.sqlite")
  message("core_db_connect: DB path = ", path)

  con <- db_connect_sqlite(path)
  if (is.null(con)) {
    warning("core_db_connect: connection failed; running without DB.")
    return(NULL)
  }

  db_init_schema(con)
  con
}
