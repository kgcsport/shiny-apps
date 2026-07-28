# _shared/demo_login.R
#
# Shared helpers for demo/sandbox mode.
#
# Exports:
#   demo_login_ui           -- quick-login buttons on the login screen
#   demo_banner_ui(is_demo) -- red top bar shown when in demo mode
#   demo_settings_panel(is_demo) -- full panel for class-job-market Settings tab
#   demo_server_init(session, DB_PATH, ...) -- call at top of server()

# ── Quick-login panel ─────────────────────────────────────────────────────────
# Shown on the login screen. Visible when DEMO_MODE=1 env var OR ?demo=1 /
# ?demo_db=1 in URL. Buttons fill credentials and click the login button.

demo_mode <- identical(Sys.getenv("DEMO_MODE"), "1")

demo_login_ui <- tagList(
  tags$div(
    id    = "demo-login-panel",
    style = paste0(
      "display:", if (demo_mode) "block" else "none", ";",
      "margin-top:14px;padding:10px 12px;",
      "background:#fff8e1;border:1px solid #ffe082;border-radius:4px;"
    ),
    tags$p(
      tags$strong("Sandbox demo"),
      tags$span(" — separate database, nothing here is real"),
      style = "margin:0 0 8px;font-size:13px;"
    ),
    tags$button("Demo (Admin)",   onclick = "demoLogin('instructor','admin123')", class = "btn btn-sm btn-warning", style = "margin:2px;"),
    tags$button("Demo (Student)", onclick = "demoLogin('alice','test123')",       class = "btn btn-sm btn-default", style = "margin:2px;")
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
# Shows only when in demo mode. Entry point is class-job-market Settings tab.
demo_banner_ui <- function(is_demo, is_admin = FALSE) {
  if (!is_demo) return(NULL)
  tags$div(
    style = paste0(
      "background:#b71c1c;color:#fff;padding:7px 16px;",
      "font-weight:600;display:flex;align-items:center;gap:12px;"
    ),
    tags$span("DEMO DATABASE -- nothing here affects your real class."),
    tags$button("Exit Demo Mode",
      onclick = "window.location.href = window.location.pathname;",
      class   = "btn btn-sm",
      style   = "background:#fff;color:#b71c1c;font-weight:600;border:none;")
  )
}

# ── Demo settings panel ───────────────────────────────────────────────────────
# Embedded in class-job-market Settings > Demo / Testing.
# Shows mode toggle + quick-open links for all other apps.
demo_settings_panel <- function(is_demo) {
  other_apps <- list(
    list(label = "Coordination Games", path = "../coordination-games/"),
    list(label = "Review Quiz",        path = "../review-quiz/"),
    list(label = "Supply Auction",     path = "../supply-auction-game/"),
    list(label = "Price Index",        path = "../price-index/"),
    list(label = "Job Picker",         path = "../class-job-picker/")
  )

  mode_section <- if (is_demo) {
    div(
      style = "background:#ffebee;border:1px solid #ef9a9a;border-radius:4px;padding:12px;margin-bottom:12px;",
      tags$p(tags$strong("You are in DEMO MODE."),
        " All data goes to the sandbox database. Your real class is unaffected.",
        style = "margin:0 0 10px;"),
      tags$button("Exit Demo Mode",
        onclick = "window.location.href = window.location.pathname;",
        class   = "btn btn-danger btn-sm")
    )
  } else {
    div(
      style = "background:#f1f8e9;border:1px solid #aed581;border-radius:4px;padding:12px;margin-bottom:12px;",
      tags$p(tags$strong("Live mode."),
        " Enter Demo Mode to test everything safely in a sandbox without touching real data.",
        style = "margin:0 0 10px;"),
      tags$button("Enter Demo Mode",
        onclick = "window.location.href = '?demo_db=1';",
        class   = "btn btn-warning btn-sm")
    )
  }

  link_buttons <- lapply(other_apps, function(a) {
    url <- if (is_demo) paste0(a$path, "?demo_db=1") else a$path
    tags$a(href = url, target = "_blank", class = "btn btn-default btn-sm",
           style = "margin:3px;", a$label)
  })

  tagList(
    tags$h5("Demo / Testing"),
    tags$p(class = "helptext",
      "Uses a separate sandbox database (", tags$code("*-demo.sqlite"), ").",
      " Reset anytime: ", tags$code("./scripts/rtest.sh reset-demo")),
    mode_section,
    div(
      tags$h6(if (is_demo) "Open other apps in demo mode" else "Open other apps"),
      div(link_buttons)
    )
  )
}

# ── Session DB initialiser ────────────────────────────────────────────────────
# Call at the very top of server() before any db_exec/db_query calls.
# Returns list with is_demo, db_path, db_exec, db_query.
#
# Usage:
#   dm       <- demo_server_init(session, DB_PATH)
#   db_exec  <- dm$db_exec
#   db_query <- dm$db_query
#   .is_demo <- dm$is_demo
#
demo_server_init <- function(session, prod_db_path, auction_prod = NULL) {
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
    if (!is.null(local_con) && DBI::dbIsValid(local_con))
      try(DBI::dbDisconnect(local_con), silent = TRUE)
  })

  list(
    is_demo  = is_demo,
    db_path  = sess_db,
    auc_path = auc_db,
    db_exec  = function(sql, params = NULL) DBI::dbExecute(get_lcon(), sql, params = params),
    db_query = function(sql, params = NULL) DBI::dbGetQuery(get_lcon(), sql, params = params)
  )
}
