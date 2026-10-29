app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
source_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

required <- c(
  "CREATE TABLE IF NOT EXISTS gradebook_exam_drops",
  "sync_cloudflare_gradebook <- function(only_assignment = \"\")",
  "sync_cloudflare_assignment",
  "Sync selected",
  "Student exam-drop adjustment",
  "save_exam_drop_btn",
  "delete_exam_drop_btn",
  "Add a brief note for this exam-drop adjustment.",
  "all_items$item_weight[to_idx]",
  "student_detail <- tags$details("
)
missing <- required[!vapply(required, grepl, logical(1), x = source_text, fixed = TRUE)]
if (length(missing)) stop("Gradebook UX contract missing: ", paste(missing, collapse = " | "))

filter_pos <- regexpr("if (nzchar(selected_key) && !identical(worker_key, selected_key))", source_text, fixed = TRUE)[1]
write_pos <- regexpr("upsert_student_grade(roster$user_id[hit[1]]", source_text, fixed = TRUE)[1]
if (filter_pos < 1L || write_pos < 1L || filter_pos > write_pos)
  stop("Selected Cloudflare assignment must be filtered before any grade write.")

cat("critical gradebook UX contract: OK\n")
