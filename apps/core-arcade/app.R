# apps/core-arcade/app.R
# CORE Arcade — stub skeleton.
#
# Run locally:
#   R -e "shiny::runApp('apps/core-arcade')"
# On server: deployed to /srv/shiny-server/core-arcade/ by Docker COPY.

library(shiny)
library(DBI)
library(RSQLite)
library(jsonlite)
library(digest)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# Source shared utilities (path relative to this file's directory)
# ---------------------------------------------------------------------------
.app_dir    <- if (nzchar(Sys.getenv("SHINY_PORT"))) {
  "/srv/shiny-server/core-arcade"
} else {
  normalizePath(getwd(), mustWork = FALSE)
}
.shared_r   <- normalizePath(file.path(.app_dir, "../_shared/R"), mustWork = FALSE)

source(file.path(.shared_r, "utils.R"))    # appdata_dir, db_file_path, here_core_arcade
source(file.path(.shared_r, "db.R"))       # db_connect_sqlite, db_init_schema
source(file.path(.shared_r, "google_backup.R"))  # stubs

# ---------------------------------------------------------------------------
# Source core-arcade R modules
# ---------------------------------------------------------------------------
for (.f in c("R/auth.R", "R/wallet.R", "R/sessions.R", "R/registry.R")) {
  source(here_core_arcade(.f))
}
# db.R last so it can call shared helpers already loaded
source(here_core_arcade("R/db.R"))

# ---------------------------------------------------------------------------
# DB connection (once, at startup)
# ---------------------------------------------------------------------------
CON <- tryCatch(core_db_connect(), error = function(e) {
  warning("app.R: DB init failed: ", conditionMessage(e))
  NULL
})

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("CORE Arcade (stub)"),

  fluidRow(
    # Left column: session management
    column(4,
      wellPanel(
        h4("Session"),
        selectInput("game_select", "Game:",
                    choices = setNames(list_games()$game_id, list_games()$label)),
        actionButton("new_session_btn", "Create new session", class = "btn-success"),
        tags$hr(),
        h5("Join existing session"),
        uiOutput("open_sessions_ui"),
        actionButton("join_session_btn", "Join selected session", class = "btn-secondary")
      ),
      wellPanel(
        h4("Player"),
        verbatimTextOutput("player_info"),
        verbatimTextOutput("wallet_display")
      )
    ),

    # Right column: game UI
    column(8,
      uiOutput("game_panel")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # --- Player identity (no-login) ---
  player_id <- reactive(auth_get_player_id(session))

  output$player_info <- renderText({
    sprintf("Player: %s\nName: %s", player_id(), auth_get_display_name())
  })

  output$wallet_display <- renderText({
    bal <- wallet_balance(CON, player_id())
    sprintf("Wallet: %s flex pass(es)", format_flex(bal))
  })

  # --- Session management ---
  current_session_id <- reactiveVal(NA_character_)

  observeEvent(input$new_session_btn, {
    sid <- session_create(CON, game_id = input$game_select)
    current_session_id(sid)
    showNotification(sprintf("Created session: %s", sid), type = "message")
  })

  open_sessions <- reactive({
    input$new_session_btn  # refresh after creation
    session_list_open(CON)
  })

  output$open_sessions_ui <- renderUI({
    df <- open_sessions()
    if (!is.data.frame(df) || nrow(df) == 0)
      return(p(em("No open sessions.")))
    selectInput("join_session_select", NULL,
                choices = setNames(df$session_id,
                                   paste0(df$game_id, " — ", df$session_id)))
  })

  observeEvent(input$join_session_btn, {
    sid <- input$join_session_select %||% NA_character_
    if (!is.na(sid) && nzchar(sid)) {
      current_session_id(sid)
      showNotification(sprintf("Joined session: %s", sid), type = "message")
    }
  })

  # --- Load and render game module ---
  loaded_game <- reactive({
    gid <- input$game_select %||% "airplane"
    tryCatch(load_game(gid), error = function(e) {
      warning("load_game failed: ", conditionMessage(e)); NULL
    })
  })

  output$game_panel <- renderUI({
    game <- loaded_game()
    sid  <- current_session_id()
    if (is.null(game) || is.null(game$ui)) {
      return(wellPanel(p(em("Select a game and create/join a session to begin."))))
    }
    if (is.na(sid)) {
      return(wellPanel(p(em("Create or join a session to start playing."))))
    }
    wellPanel(
      game$ui("game_module")
    )
  })

  observe({
    game <- loaded_game()
    sid  <- current_session_id()
    if (!is.null(game) && !is.null(game$server) && !is.na(sid)) {
      game$server("game_module",
                  con        = CON,
                  session_id = current_session_id,
                  player_id  = player_id)
    }
  })
}

shinyApp(ui, server)
