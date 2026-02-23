# apps/core-arcade/app.R
# CORE Arcade — stub skeleton with per-course DB routing.
#
# Run locally:
#   R -e "shiny::runApp('apps/core-arcade')"
# On server: deployed to /srv/shiny-server/core-arcade/ by Docker COPY.
#
# Course selection priority:
#   1. ?course=ECON-102 query parameter  (used immediately, no UI shown)
#   2. Interactive selectInput           (shown until instructor confirms)
# Once locked for the session the selector disappears and the DB connection
# is opened at /srv/shiny-server/appdata/core-arcade-<COURSE>.sqlite
# (locally: <repo-root>/appdata/core-arcade-<COURSE>.sqlite).

library(shiny)
library(DBI)
library(RSQLite)
library(jsonlite)
library(digest)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# Source shared utilities
# ---------------------------------------------------------------------------
.app_dir  <- if (nzchar(Sys.getenv("SHINY_PORT"))) {
  "/srv/shiny-server/core-arcade"
} else {
  normalizePath(getwd(), mustWork = FALSE)
}
.shared_r <- normalizePath(file.path(.app_dir, "../_shared/R"), mustWork = FALSE)

source(file.path(.shared_r, "utils.R"))          # appdata_dir, db_file_path, here_core_arcade
source(file.path(.shared_r, "db.R"))             # db_connect_sqlite, db_init_schema
source(file.path(.shared_r, "google_backup.R"))  # stubs

# ---------------------------------------------------------------------------
# Source core-arcade R modules
# ---------------------------------------------------------------------------
for (.f in c("R/auth.R", "R/wallet.R", "R/sessions.R", "R/registry.R")) {
  source(here_core_arcade(.f))
}
source(here_core_arcade("R/db.R"))  # core_db_connect (not used directly here, kept for reference)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
COURSE_CHOICES <- c("ECON-102", "ECON-342")

# Sanitise a course key coming from a URL parameter so it can't inject path
# components.  Keeps letters, digits, hyphens, underscores only.
sanitise_course <- function(x) gsub("[^A-Za-z0-9_-]", "_", trimws(x))

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("CORE Arcade (stub)"),
  uiOutput("course_ui"),   # selector — disappears once course is locked
  uiOutput("main_ui")      # rest of the app — appears once course is locked
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # -------------------------------------------------------------------------
  # Course selection
  # -------------------------------------------------------------------------

  course_key <- reactiveVal(NULL)

  # Priority 1: ?course= query parameter (fires once at session start)
  observeEvent(session$clientData$url_search, {
    if (!is.null(course_key())) return()   # already locked
    ck_raw <- parseQueryString(session$clientData$url_search)[["course"]] %||% ""
    if (!nzchar(ck_raw)) return()
    ck <- sanitise_course(ck_raw)
    session$userData$course_key <- ck
    course_key(ck)
  }, once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Priority 2: manual confirmation via button
  observeEvent(input$course_confirm, {
    if (!is.null(course_key())) return()   # already locked
    ck <- sanitise_course(input$course_select %||% "")
    if (!nzchar(ck)) return()
    session$userData$course_key <- ck
    course_key(ck)
  })

  # Show selector only while course is not yet locked
  output$course_ui <- renderUI({
    if (!is.null(course_key())) return(NULL)
    wellPanel(
      h4("Select course"),
      p(em("Choose the course for this session. This cannot be changed once confirmed.")),
      selectInput("course_select", NULL, choices = COURSE_CHOICES),
      actionButton("course_confirm", "Confirm course", class = "btn-primary")
    )
  })

  # -------------------------------------------------------------------------
  # Per-course DB connection  (reactive on course_key)
  # -------------------------------------------------------------------------

  con <- reactiveVal(NULL)

  # Open a new connection whenever course_key is set or changes.
  # Closes any previous connection first so we never leak handles.
  observe({
    ck <- course_key()
    req(!is.null(ck))

    old <- isolate(con())
    if (!is.null(old) && tryCatch(DBI::dbIsValid(old), error = function(e) FALSE)) {
      try(DBI::dbDisconnect(old), silent = TRUE)
    }

    db_name <- paste0("core-arcade-", ck, ".sqlite")
    path    <- db_file_path(db_name)
    new_con <- tryCatch({
      nc <- db_connect_sqlite(path)
      db_init_schema(nc)
      nc
    }, error = function(e) {
      warning("DB connect failed for course '", ck, "': ", conditionMessage(e))
      NULL
    })
    con(new_con)
  })

  # Disconnect on session end
  session$onSessionEnded(function() {
    c <- isolate(con())
    if (!is.null(c) && tryCatch(DBI::dbIsValid(c), error = function(e) FALSE)) {
      try(DBI::dbDisconnect(c), silent = TRUE)
    }
  })

  # -------------------------------------------------------------------------
  # Player identity  (no-login)
  # -------------------------------------------------------------------------

  player_id <- reactive(auth_get_player_id(session))

  # -------------------------------------------------------------------------
  # Session management
  # -------------------------------------------------------------------------

  current_session_id <- reactiveVal(NA_character_)

  # Reset open game session when course changes (new DB = new context)
  observeEvent(course_key(), {
    current_session_id(NA_character_)
  }, ignoreNULL = TRUE)

  observeEvent(input$new_session_btn, {
    req(con())
    sid <- session_create(con(), game_id = input$game_select)
    current_session_id(sid)
    showNotification(sprintf("Created session: %s", sid), type = "message")
  })

  open_sessions <- reactive({
    req(con())
    input$new_session_btn   # invalidate after creating a session
    session_list_open(con())
  })

  observeEvent(input$join_session_btn, {
    sid <- input$join_session_select %||% NA_character_
    if (!is.na(sid) && nzchar(sid)) {
      current_session_id(sid)
      showNotification(sprintf("Joined session: %s", sid), type = "message")
    }
  })

  # -------------------------------------------------------------------------
  # Game module wiring
  # -------------------------------------------------------------------------

  loaded_game <- reactive({
    gid <- input$game_select %||% "airplane"
    tryCatch(load_game(gid), error = function(e) {
      warning("load_game failed: ", conditionMessage(e)); NULL
    })
  })

  # Call the game module server once each time a valid session becomes active.
  # We isolate(con()) so the module captures the connection at the moment the
  # session is joined/created; course switching resets current_session_id to
  # NA first, ensuring a fresh module call with the new connection.
  observeEvent(current_session_id(), {
    game <- loaded_game()
    sid  <- current_session_id()
    req(!is.null(game), !is.null(game$server), !is.na(sid), !is.null(con()))
    game$server("game_module",
                con        = isolate(con()),
                session_id = current_session_id,
                player_id  = player_id)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # -------------------------------------------------------------------------
  # Outputs
  # -------------------------------------------------------------------------

  # Main layout — only rendered after course is locked
  output$main_ui <- renderUI({
    req(course_key())
    fluidRow(
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
        ),
        wellPanel(
          h5("Debug"),
          verbatimTextOutput("db_path_debug")
        )
      ),
      column(8,
        uiOutput("game_panel")
      )
    )
  })

  output$player_info <- renderText({
    req(course_key())
    sprintf("Player:  %s\nName:    %s\nCourse:  %s",
            player_id(), auth_get_display_name(), course_key())
  })

  output$wallet_display <- renderText({
    req(con())
    bal <- wallet_balance(con(), player_id())
    sprintf("Wallet: %s flex pass(es)", format_flex(bal))
  })

  # Debug panel: shows resolved DB path and connection status
  output$db_path_debug <- renderText({
    ck <- course_key()
    if (is.null(ck)) return("No course selected yet.")
    db_name  <- paste0("core-arcade-", ck, ".sqlite")
    path     <- db_file_path(db_name)
    live_con <- con()
    connected <- !is.null(live_con) &&
      tryCatch(DBI::dbIsValid(live_con), error = function(e) FALSE)
    sprintf("Course:    %s\nDB file:   %s\nConnected: %s",
            ck, path, connected)
  })

  output$open_sessions_ui <- renderUI({
    df <- open_sessions()
    if (!is.data.frame(df) || nrow(df) == 0)
      return(p(em("No open sessions.")))
    selectInput("join_session_select", NULL,
                choices = setNames(df$session_id,
                                   paste0(df$game_id, " \u2014 ", df$session_id)))
  })

  output$game_panel <- renderUI({
    req(con())
    game <- loaded_game()
    sid  <- current_session_id()
    if (is.null(game) || is.null(game$ui))
      return(wellPanel(p(em("Select a game and create/join a session to begin."))))
    if (is.na(sid))
      return(wellPanel(p(em("Create or join a session to start playing."))))
    wellPanel(game$ui("game_module"))
  })
}

shinyApp(ui, server)
