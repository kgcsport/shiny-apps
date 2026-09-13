library(DBI)
library(RSQLite)

source("apps/live-wordcloud/helpers.R")

stopifnot(identical(normalize_poll_response("  Land-value TAX! "), "land value tax"))
stopifnot(is.null(validate_poll_response("land value")))
stopifnot(!is.null(validate_poll_response("")))
stopifnot(!is.null(validate_poll_response("one two three four five")))

specs <- cloud_term_specs(data.frame(
  label = c("income", "land value"),
  n = c(1L, 4L)
))
stopifnot(specs$label[1] == "land value")
stopifnot(specs$font_rem[1] > specs$font_rem[2])

db <- tempfile(fileext = ".sqlite")
con <- dbConnect(SQLite(), db)
on.exit({
  dbDisconnect(con)
  unlink(db)
}, add = TRUE)

dbExecute(con, "
  CREATE TABLE live_poll_responses (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    poll_id TEXT NOT NULL,
    client_token TEXT NOT NULL,
    response TEXT NOT NULL,
    response_norm TEXT NOT NULL,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP,
    updated_at TEXT DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(poll_id, client_token)
  );
")

upsert <- function(token, response) {
  dbExecute(
    con,
    "INSERT INTO live_poll_responses
       (poll_id, client_token, response, response_norm)
     VALUES ('test', ?, ?, ?)
     ON CONFLICT(poll_id, client_token) DO UPDATE SET
       response = excluded.response,
       response_norm = excluded.response_norm,
       updated_at = CURRENT_TIMESTAMP;",
    params = list(token, response, normalize_poll_response(response))
  )
}

upsert("student-a", "Land value")
upsert("student-b", "land value!")
upsert("student-a", "Income")

counts <- dbGetQuery(
  con,
  "SELECT response_norm, COUNT(*) n
   FROM live_poll_responses
   WHERE poll_id='test'
   GROUP BY response_norm
   ORDER BY response_norm;"
)

stopifnot(nrow(counts) == 2L)
stopifnot(counts$n[counts$response_norm == "income"] == 1L)
stopifnot(counts$n[counts$response_norm == "land value"] == 1L)

cat("live-wordcloud smoke test passed\n")
