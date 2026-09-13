"%||%" <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
}

normalize_poll_id <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-+|-+$)", "", x)
}

normalize_poll_response <- function(x) {
  x <- enc2utf8(trimws(as.character(x %||% "")))
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("\\s+", " ", x)
  tolower(trimws(x))
}

validate_poll_response <- function(x, max_words = 4L, max_chars = 42L) {
  clean <- trimws(as.character(x %||% ""))
  if (!nzchar(clean)) return("Enter a response before submitting.")
  if (!nzchar(normalize_poll_response(clean))) {
    return("Enter words rather than punctuation.")
  }
  if (nchar(clean, type = "chars") > max_chars) {
    return(paste0("Keep the response under ", max_chars, " characters."))
  }
  words <- strsplit(clean, "\\s+")[[1]]
  if (length(words) > max_words) {
    return(paste0("Use ", max_words, " words or fewer."))
  }
  NULL
}

initialize_live_poll_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS live_polls (
      poll_id       TEXT PRIMARY KEY,
      prompt        TEXT NOT NULL,
      instructions  TEXT NOT NULL DEFAULT '',
      max_words     INTEGER NOT NULL DEFAULT 4,
      is_open       INTEGER NOT NULL DEFAULT 1,
      created_at    TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at    TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS live_poll_responses (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      poll_id       TEXT NOT NULL,
      client_token  TEXT NOT NULL,
      response      TEXT NOT NULL,
      response_norm TEXT NOT NULL,
      created_at    TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at    TEXT DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(poll_id, client_token)
    );
  ")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_live_poll_response
    ON live_poll_responses(poll_id, response_norm);
  ")
  invisible(TRUE)
}

seed_live_poll <- function(con, poll_id, prompt, instructions = "", max_words = 4L) {
  DBI::dbExecute(
    con,
    "INSERT OR IGNORE INTO live_polls
       (poll_id, prompt, instructions, max_words, is_open)
     VALUES (?, ?, ?, ?, 1);",
    params = list(
      normalize_poll_id(poll_id),
      trimws(prompt),
      trimws(instructions),
      as.integer(max_words)
    )
  )
}

save_live_poll <- function(con, poll_id, prompt, instructions = "",
                           max_words = 4L, is_open = TRUE) {
  poll_id <- normalize_poll_id(poll_id)
  prompt <- trimws(as.character(prompt %||% ""))
  if (!nzchar(poll_id)) stop("A URL key is required.")
  if (!nzchar(prompt)) stop("A question is required.")
  max_words <- max(1L, min(8L, as.integer(max_words)))
  DBI::dbExecute(
    con,
    "INSERT INTO live_polls
       (poll_id, prompt, instructions, max_words, is_open)
     VALUES (?, ?, ?, ?, ?)
     ON CONFLICT(poll_id) DO UPDATE SET
       prompt = excluded.prompt,
       instructions = excluded.instructions,
       max_words = excluded.max_words,
       is_open = excluded.is_open,
       updated_at = CURRENT_TIMESTAMP;",
    params = list(
      poll_id,
      prompt,
      trimws(as.character(instructions %||% "")),
      max_words,
      as.integer(isTRUE(is_open))
    )
  )
  poll_id
}

get_live_poll <- function(con, poll_id) {
  DBI::dbGetQuery(
    con,
    "SELECT poll_id, prompt, instructions, max_words, is_open
     FROM live_polls
     WHERE poll_id = ?;",
    params = list(normalize_poll_id(poll_id))
  )
}

list_live_polls <- function(con) {
  DBI::dbGetQuery(
    con,
    "SELECT poll_id, prompt, instructions, max_words, is_open
     FROM live_polls
     ORDER BY updated_at DESC, poll_id;"
  )
}

upsert_live_poll_response <- function(con, poll_id, client_token, response) {
  normalized <- normalize_poll_response(response)
  DBI::dbExecute(
    con,
    "INSERT INTO live_poll_responses
       (poll_id, client_token, response, response_norm)
     VALUES (?, ?, ?, ?)
     ON CONFLICT(poll_id, client_token) DO UPDATE SET
       response = excluded.response,
       response_norm = excluded.response_norm,
       updated_at = CURRENT_TIMESTAMP;",
    params = list(normalize_poll_id(poll_id), client_token, trimws(response), normalized)
  )
}

live_poll_counts <- function(con, poll_id) {
  DBI::dbGetQuery(
    con,
    "SELECT response_norm, MIN(response) AS label, COUNT(*) AS n
     FROM live_poll_responses
     WHERE poll_id = ?
     GROUP BY response_norm
     ORDER BY n DESC, response_norm ASC;",
    params = list(normalize_poll_id(poll_id))
  )
}

clear_live_poll_responses <- function(con, poll_id) {
  DBI::dbExecute(
    con,
    "DELETE FROM live_poll_responses WHERE poll_id = ?;",
    params = list(normalize_poll_id(poll_id))
  )
}

cloud_term_specs <- function(dat) {
  stopifnot(all(c("label", "n") %in% names(dat)))
  if (!nrow(dat)) return(dat)
  dat <- dat[order(-dat$n, tolower(dat$label)), , drop = FALSE]
  max_n <- max(dat$n)
  dat$font_rem <- pmin(3.8, 1.1 + 0.9 * log2(dat$n + 1))
  dat$opacity <- 0.72 + 0.28 * sqrt(dat$n / max_n)
  palette <- c("#7A1731", "#0072B2", "#D55E00", "#009E73", "#6A3D9A", "#8C510A")
  dat$colour <- palette[(seq_len(nrow(dat)) - 1L) %% length(palette) + 1L]
  dat
}
