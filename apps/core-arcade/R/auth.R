# apps/core-arcade/R/auth.R
# Authentication stubs — no login required for now.

auth_mode <- function() "no_login"

# Returns a persistent player_id for this browser session.
# Stored in session$userData so it survives re-renders.
auth_get_player_id <- function(session) {
  if (is.null(session$userData$player_id)) {
    session$userData$player_id <- paste0("guest_", substr(digest::digest(runif(1)), 1, 8))
  }
  session$userData$player_id
}

auth_get_display_name <- function() "Guest"
