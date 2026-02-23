# apps/core-arcade/R/sessions.R
# Game session lifecycle helpers.

session_create <- function(con, game_id, settings = list()) {
  if (is.null(con)) return(NA_character_)
  session_id <- paste0(game_id, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
                       substr(digest::digest(runif(1)), 1, 6))
  DBI::dbExecute(con, "
    INSERT INTO sessions(session_id, game_id, created_at, status, settings_json)
    VALUES(?, ?, ?, 'open', ?);
  ", list(
    session_id,
    as.character(game_id),
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    jsonlite::toJSON(settings, auto_unbox = TRUE)
  ))
  session_id
}

session_list_open <- function(con) {
  if (is.null(con)) return(data.frame())
  DBI::dbGetQuery(con,
    "SELECT session_id, game_id, created_at, status FROM sessions WHERE status != 'archived' ORDER BY created_at DESC;")
}

session_get <- function(con, session_id) {
  if (is.null(con) || is.na(session_id)) return(NULL)
  row <- DBI::dbGetQuery(con,
    "SELECT * FROM sessions WHERE session_id = ?;", list(session_id))
  if (nrow(row) == 0) NULL else row
}

session_set_status <- function(con, session_id, status) {
  if (is.null(con) || is.na(session_id)) return(invisible(NULL))
  DBI::dbExecute(con,
    "UPDATE sessions SET status = ? WHERE session_id = ?;",
    list(as.character(status), as.character(session_id)))
  invisible(NULL)
}
