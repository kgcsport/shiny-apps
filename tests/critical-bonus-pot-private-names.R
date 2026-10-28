#!/usr/bin/env Rscript
# Critical contract: Reveal permits aggregate results, but names remain private
# unless the explicit public-names setting is enabled.
app_path <- if (file.exists("apps/coordination-games/app.R")) "apps/coordination-games/app.R" else "/srv/shiny-server/coordination-games/app.R"
source_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
list_start <- regexpr("output\\$public_contribution_list <- renderUI\\(\\{", source_text, perl = TRUE)
if (list_start[[1]] < 1L) stop("Could not locate public contribution list.")
list_source <- substr(source_text, list_start[[1]], list_start[[1]] + 3500L)
required <- c("visible <- isTRUE(as.integer(o$show_contributions[1] %||% 0L) == 1L)", "Show student names and contributions publicly", "Named contributions are opt-in per round", "updateCheckboxInput(session, \"adm_show_contributions\", value = FALSE)")
missing <- required[!vapply(required, grepl, logical(1), x = source_text, fixed = TRUE)]
if (length(missing)) stop("Private-name Bonus Pot contract missing: ", paste(missing, collapse = " | "))
if (grepl("round_status[1] %||% \"\"), \"revealed\") ||", list_source, fixed = TRUE)) stop("Reveal alone must not expose the named contribution list.")
if (!grepl("bids_visible <- identical(status, \"revealed\") || show_live_bids", source_text, fixed = TRUE)) stop("Reveal must continue to expose aggregate Bonus Pot results.")
cat("critical Bonus Pot private names: OK\n")
