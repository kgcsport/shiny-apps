# _shared/demo_login.R
#
# Shared helpers for demo/sandbox mode.
#
# Exports:
#   demo_login_ui      — quick-login panel; include inside login_ui
#   demo_banner_ui(is_demo, is_admin) — top-of-page banner with Enter/Exit button
#   demo_session_db(session) — picks the right DB path for this session
#   demo_server_init(session, DB_PATH, ...) — call at top of server(); returns list

# ── Quick-login panel ─────────────────────────────────────────────────────────
# Shown on the login screen. Visible when:
#   • DEMO_MODE=1 env var (docker shiny-demo service)
#   • ?demo=1 or ?demo_db=1 in URL (instructor switching modes)

demo_mode   <- identical(Sys.getenv("DEMO_MODE"), "1")

demo_login_ui <- tagList(
  tags$div(
    id    = "demo-login-panel",
    style = paste0(
      "display:", if (demo_mode) "block" else "none", ";",
      "margin-top:14px;padding:10px 12px;",
      "background:#fff8e1;border:1px solid #ffe082;border-radius:4px;"
    ),
    tags$p(
      tags$strong("Test users"),
      tags$span(" — sandbox database, safe to mess around in"),
      style = "margin:0 0 8px;font-size:13px;"
    ),
    tags$button("Alice (S01)",  onclick = "demoLogin('alice','test123')",       class = "btn btn-xs btn-default", style = "margin:2px;"),
    tags$button("Bob (S01)",    onclick = "demoLogin('bob','test123')",         class = "btn btn-xs btn-default", style = "margin:2px;"),
    tags$button("Carol (S01)",  onclick = "demoLogin('carol','test123')",       class = "btn btn-xs btn-default", style = "margin:2px;"),
    tags$button("Dan (S02)",    onclick = "demoLogin('dan','test123')",         class = "btn btn-xs btn-default", style = "margin:2px;"),
    tags$button("Eve (S02)",    onclick = "demoLogin('eve','test123')",         class = "btn btn-xs btn-default", style = "margin:2px;"),
    tags$button("Instructor",   onclick = "demoLogin('instructor','admin123')", class = "btn btn-xs btn-warning", style = "margin:2px;")
  ),
  tags$script(HTML('
    (function () {
      function demoLogin(user, pass) {
        var u = document.getElementById("login_user");
        var p = document.getElementById("login_pw");
        if (u) { u.value = user;  u.dispatchEvent(new Event("input",  {bubbles: true})); }
        if (p) { p.value = pass;  p.dispatchEvent(new Event("input",  {bubbles: true})); }
        setTimeout(function () {
          var btn = document.getElementById("login_btn");
          if (btn) btn.click();
        }, 60);
      }
      window.demoLogin = demoLogin;

      function revealIfDemo() {
        var params = new URLSearchParams(window.location.search);
        if (params.get("demo") === "1" || params.get("demo_db") === "1") {
          var panel = document.getElementById("demo-login-panel");
          if (panel) panel.style.display = "block";
        }
      }
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", revealIfDemo);
      } else {
        revealIfDemo();
      }
      document.addEventListener("shiny:value", revealIfDemo);
    })();
  '))
)

# ── Top-of-page demo banner ───────────────────────────────────────────────────
# Call from output$demo_banner <- renderUI(demo_banner_ui(.is_demo, is_admin_flag))
demo_banner_ui <- function(is_demo, is_admin = FALSE) {
  if (is_demo) {
    tags$div(
      style = paste0(
        "background:#b71c1c;color:#fff;padding:7px 16px;",
        "font-weight:600;display:flex;align-items:center;gap:12px;"
      ),
      tags$span("⚠️  DEMO DATABASE — nothing here affects your real class."),
      tags$button("Exit Demo Mode",
        onclick = "window.location.href = window.location.pathname;",
        class   = "btn btn-sm",
        style   = "background:#fff;color:#b71c1c;font-weight:600;border:none;")
    )
  } else if (is_admin) {
    tags$div(
      style = "background:#fff3e0;padding:5px 16px;font-size:13px;",
      "Want to test without affecting real data?  ",
      tags$button("Enter Demo Mode",
        onclick = "window.location.href = '?demo_db=1';",
        class   = "btn btn-xs btn-warning")
    )
  }
}

# ── Session DB initialiser ────────────────────────────────────────────────────
# Call at the very top of server() before any db_exec/db_query calls.
# Returns list(is_demo, db_path, db_exec, db_query) — assign the last two
# to shadow the globals.
#
# Usage:
#   dm <- demo_server_init(session, DB_PATH)
#   db_exec  <- dm$db_exec
#   db_query <- dm$db_query
#   .is_demo <- dm$is_demo
#
demo_server_init <- function(session, prod_db_path,
                              auction_prod = NULL) {
  qs       <- parseQueryString(isolate(session$clientData$url_search))
  is_demo  <- identical(qs[["demo_db"]], "1") || demo_mode
  sess_db  <- if (is_demo) shared_db_path(demo = TRUE) else prod_db_path

  auc_db <- if (!is.null(auction_prod)) {
    if (is_demo) auction_db_path(demo = TRUE) else auction_prod
  } else NULL

  local_con <- NULL
  get_lcon  <- function() {
    if (is.null(local_con) || !DBI::dbIsValid(local_con)) {
      dir.create(dirname(sess_db), recursive = TRUE, showWarnings = FALSE)
      local_con <<- connect_sqlite(sess_db)
    }
    local_con
  }

  session$onSessionEnded(function() {
    if (!is.null(local_con) && DBI::dbIsValid(local_con)) {
      try(DBI::dbDisconnect(local_con), silent = TRUE)
    }
  })

  list(
    is_demo  = is_demo,
    db_path  = sess_db,
    auc_path = auc_db,
    db_exec  = function(sql, params = NULL) DBI::dbExecute(get_lcon(), sql, params = params),
    db_query = function(sql, params = NULL) DBI::dbGetQuery(get_lcon(), sql, params = params)
  )
}
