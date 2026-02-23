# apps/core-arcade/R/registry.R
# Game registry: enumerate available games and load their modules.

list_games <- function() {
  data.frame(
    game_id     = "airplane",
    label       = "Paper Aeroplane Production",
    path        = "R/games/airplane",
    stringsAsFactors = FALSE
  )
}

load_game <- function(game_id) {
  games <- list_games()
  row   <- games[games$game_id == game_id, , drop = FALSE]
  if (nrow(row) == 0) stop("load_game: unknown game_id '", game_id, "'")

  game_dir <- here_core_arcade(row$path[1])

  env <- new.env(parent = globalenv())
  for (f in c("ui.R", "server.R", "score.R")) {
    p <- file.path(game_dir, f)
    if (file.exists(p)) source(p, local = env) else warning("load_game: missing ", p)
  }

  list(
    ui     = if (exists("airplane_ui",     envir = env)) env$airplane_ui     else NULL,
    server = if (exists("airplane_server", envir = env)) env$airplane_server else NULL,
    score  = if (exists("airplane_score",  envir = env)) env$airplane_score  else NULL
  )
}
