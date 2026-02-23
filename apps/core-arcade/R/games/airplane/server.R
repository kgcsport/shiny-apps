# apps/core-arcade/R/games/airplane/server.R
# Shiny module server for the Paper Aeroplane Production game.

airplane_server <- function(id, con, session_id, player_id) {
  shiny::moduleServer(id, function(input, output, session) {

    last_msg <- shiny::reactiveVal("")

    shiny::observeEvent(input$submit, {
      count <- as.integer(input$approved %||% 0)

      # Record production event
      if (!is.null(con)) {
        DBI::dbExecute(con, "
          INSERT INTO events(ts, session_id, player_id, event_type, payload_json)
          VALUES(?, ?, ?, 'production', ?);
        ", list(
          format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
          as.character(session_id()),
          as.character(player_id()),
          jsonlite::toJSON(list(approved = count), auto_unbox = TRUE)
        ))

        # Payout: +1 half-unit per submission as a stub reward
        wallet_post(con, player_id(), delta_half = 1L, kind = "game_payout",
                    ref_type = "airplane", ref_id = as.character(session_id()))
      }

      last_msg(sprintf("Submitted: %d approved plane(s) at %s",
                       count, format(Sys.time(), "%H:%M:%S")))
    })

    output$confirmation <- shiny::renderText(last_msg())
  })
}

# Null-safe fallback (same %||% used in app.R)
`%||%` <- function(a, b) if (!is.null(a)) a else b
