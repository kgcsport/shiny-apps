# apps/_shared/R/utils.R
# Path utilities shared across all core-arcade-family apps.

is_shiny_server <- function() {
  dir.exists("/srv/shiny-server")
}

appdata_dir <- function() {
  if (is_shiny_server()) {
    d <- "/srv/shiny-server/appdata"
  } else {
    # When run via R -e "shiny::runApp('apps/core-arcade')" the working directory
    # is apps/core-arcade, so go up two levels to repo root then into appdata.
    wd <- getwd()
    candidate <- file.path(wd, "appdata")
    # If wd looks like it's already inside apps/<name>, use repo-root appdata
    if (grepl("/apps/[^/]+$", wd)) {
      candidate <- file.path(dirname(dirname(wd)), "appdata")
    }
    d <- candidate
  }
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  normalizePath(d, mustWork = FALSE)
}

db_file_path <- function(db_name = "core-arcade.sqlite") {
  file.path(appdata_dir(), db_name)
}

safe_source <- function(path) {
  if (file.exists(path)) {
    source(path, local = FALSE)
  } else {
    warning("safe_source: file not found: ", path)
  }
}

# Returns an absolute path to a file relative to apps/core-arcade/.
# Works both locally (where getwd() IS apps/core-arcade) and on the server
# (where the app root is /srv/shiny-server/core-arcade).
here_core_arcade <- function(...) {
  if (is_shiny_server()) {
    root <- "/srv/shiny-server/core-arcade"
  } else {
    # Assume wd is apps/core-arcade when runApp() is called
    root <- getwd()
  }
  normalizePath(file.path(root, ...), mustWork = FALSE)
}
