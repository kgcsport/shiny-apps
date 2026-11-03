# Price War is a separate route with the same shared authentication, database,
# and player mechanics as Bonus Pot. The shared player switches its allowed
# game and copy according to this mode before constructing the Shiny app.
Sys.setenv(COORDINATION_GAME_MODE = "price_war")
source(file.path(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE)), "..", "bonus-pot", "app.R"), local = FALSE)$value
