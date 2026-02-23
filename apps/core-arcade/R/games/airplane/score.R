# apps/core-arcade/R/games/airplane/score.R
# Scoring summary for the Paper Aeroplane Production game.

airplane_score <- function(con, session_id) {
  if (is.null(con) || is.na(session_id)) {
    return(data.frame(player_id = character(), total_approved = integer()))
  }

  rows <- DBI::dbGetQuery(con, "
    SELECT player_id, payload_json
    FROM events
    WHERE session_id = ? AND event_type = 'production'
    ORDER BY ts;
  ", list(as.character(session_id)))

  if (nrow(rows) == 0) {
    return(data.frame(player_id = character(), total_approved = integer()))
  }

  counts <- vapply(rows$payload_json, function(j) {
    parsed <- tryCatch(jsonlite::fromJSON(j), error = function(e) list(approved = 0L))
    as.integer(parsed$approved %||% 0L)
  }, integer(1))

  agg <- aggregate(counts, by = list(player_id = rows$player_id), FUN = sum)
  names(agg)[2] <- "total_approved"
  agg
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
