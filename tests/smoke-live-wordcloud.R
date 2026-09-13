library(DBI)
library(RSQLite)

source("apps/live-wordcloud/helpers.R")

stopifnot(identical(normalize_poll_id(" Tariff Welfare?! "), "tariff-welfare"))
stopifnot(identical(normalize_poll_response("  Land-value TAX! "), "land value tax"))
stopifnot(is.null(validate_poll_response("land value")))
stopifnot(!is.null(validate_poll_response("")))
stopifnot(!is.null(validate_poll_response("!!!")))
stopifnot(!is.null(validate_poll_response("one two three four five")))
stopifnot(
  live_poll_submission_token("browser-a", "session-a", 1L) !=
    live_poll_submission_token("browser-a", "session-a", 2L)
)

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

initialize_live_poll_schema(con)
seed_live_poll(con, "tax-base", "What should we tax?", "Use a few words.", 4L)
save_live_poll(con, "tariff-welfare", "What disappears after a tariff?", "", 5L, TRUE)

polls <- list_live_polls(con)
stopifnot(nrow(polls) == 2L)
stopifnot(get_live_poll(con, "tax-base")$max_words[1] == 4L)

upsert_live_poll_response(con, "tax-base", "student-a", "Land value")
upsert_live_poll_response(con, "tax-base", "student-b", "land value!")
upsert_live_poll_response(con, "tax-base", "student-a", "Income")
upsert_live_poll_response(con, "tariff-welfare", "student-a", "Import varieties")

tax_counts <- live_poll_counts(con, "tax-base")
tariff_counts <- live_poll_counts(con, "tariff-welfare")

stopifnot(nrow(tax_counts) == 2L)
stopifnot(sum(tax_counts$n) == 2L)
stopifnot(tax_counts$n[tax_counts$response_norm == "income"] == 1L)
stopifnot(tax_counts$n[tax_counts$response_norm == "land value"] == 1L)
stopifnot(nrow(tariff_counts) == 1L)
stopifnot(tariff_counts$response_norm[1] == "import varieties")

clear_live_poll_responses(con, "tax-base")
stopifnot(sum(live_poll_counts(con, "tax-base")$n) == 0L)
stopifnot(sum(live_poll_counts(con, "tariff-welfare")$n) == 1L)

cat("live-wordcloud smoke test passed\n")
