# tests/setup/fresh_semester_db.R
#
# Backup the current class-job-market SQLite files and move them out of the way
# so the app recreates a clean, current-schema database on next startup.
#
# Usage:
#   Rscript tests/setup/fresh_semester_db.R --yes
#   CONNECT_CONTENT_DIR=/srv/shiny-server/appdata Rscript tests/setup/fresh_semester_db.R --yes
#
# Add --demo to target class-job-market-demo.sqlite instead of production.

args <- commandArgs(trailingOnly = TRUE)
if (!"--yes" %in% args) {
  stop("Refusing to reset without --yes. This backs up and removes the active DB files.")
}

demo <- "--demo" %in% args || identical(Sys.getenv("DEMO_MODE"), "1")
db_name <- if (demo) "class-job-market-demo.sqlite" else "class-job-market.sqlite"

data_dir <- function() {
  root <- Sys.getenv("CONNECT_CONTENT_DIR", "")
  if (nzchar(root)) return(file.path(root, "data"))
  docker <- "/srv/shiny-server/appdata/data"
  if (dir.exists(docker)) return(docker)
  script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) ".")
  file.path(script_dir, "..", "..", "apps", "class-job-market", "data")
}

dir <- normalizePath(data_dir(), mustWork = FALSE)
dir.create(dir, recursive = TRUE, showWarnings = FALSE)
backup_dir <- file.path(dir, "backups", format(Sys.time(), "%Y%m%d-%H%M%S"))
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

targets <- file.path(dir, paste0(db_name, c("", "-wal", "-shm")))
found <- targets[file.exists(targets)]

if (!length(found)) {
  cat("No existing DB files found for ", db_name, " in ", dir, ".\n", sep = "")
  cat("Start the app and it will create a fresh database.\n")
  quit(save = "no")
}

for (src in found) {
  dest <- file.path(backup_dir, basename(src))
  ok <- file.rename(src, dest)
  if (!ok) stop("Could not move ", src, " to ", dest, ". Stop the running app/container and retry.")
  cat("Backed up ", src, " -> ", dest, "\n", sep = "")
}

cat("Fresh semester reset staged for ", db_name, ".\n", sep = "")
cat("Restart the app/container; startup will recreate the DB with the current schema.\n")
