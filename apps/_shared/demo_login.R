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
  tags$details(
    id    = "demo-login-panel",
    class = "login-howto",
    style = paste0(
      "display:block;",
      "margin-top:10px;"
    ),
    tags$summary("Sandbox Demo"),
    tags$div(
      style = paste0(
        "margin-top:8px;padding:10px 12px;",
        "background:#fff8e1;border:1px solid #ffe082;border-radius:4px;"
      ),
      tags$p(
        tags$strong("Separate sandbox database."),
        tags$span(" Nothing here is real."),
        style = "margin:0 0 8px;font-size:13px;"
      ),
      tags$button("Demo (Admin)",   onclick = "window.location.href='?demo_db=1&demo_as=teacher'", class = "btn btn-sm btn-warning", style = "margin:2px;"),
      tags$button("Demo (Student)", onclick = "window.location.href='?demo_db=1&demo_as=student'", class = "btn btn-sm btn-default", style = "margin:2px;")
    ),
  ),
  tags$script(HTML('
    (function () {
      function demoLogin(user, pass) {
        // If the password form is collapsed inside <details>, open it first
        var details = document.querySelector(".admin-login-toggle");
        if (details) details.open = true;
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

      var _autoLoginDone = false;
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

      // Auto-login via ?demo_as=student|teacher — poll until form is in DOM
      (function () {
        var params = new URLSearchParams(window.location.search);
        var demoAs = params.get("demo_as");
        if (!demoAs) return;
        var user = demoAs === "student" ? "alice" : "instructor";
        var pass = demoAs === "student" ? "test123" : "admin123";
        var attempts = 0;
        function tryLogin() {
          if (_autoLoginDone) return;
          var u = document.getElementById("login_user");
          var p = document.getElementById("login_pw");
          if (u && p) {
            _autoLoginDone = true;
            demoLogin(user, pass);
          } else if (attempts < 40) {
            attempts++;
            setTimeout(tryLogin, 250);
          }
        }
        tryLogin();
      })();
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

# ── Demo DB bootstrapper ──────────────────────────────────────────────────────
# Called the first time a demo session connects. Copies the full schema from
# the production DB so every table/index exists, then seeds test users.
# Safe to call repeatedly — CREATE IF NOT EXISTS / INSERT OR IGNORE are idempotent.
demo_db_bootstrap <- function(demo_con, prod_path) {
  tryCatch({
    prod_con <- DBI::dbConnect(RSQLite::SQLite(), prod_path)
    on.exit(try(DBI::dbDisconnect(prod_con), silent = TRUE), add = TRUE)

    # Copy all table schemas from production
    schema <- DBI::dbGetQuery(prod_con,
      "SELECT sql FROM sqlite_master WHERE type IN ('table','index') AND sql IS NOT NULL;")
    for (sql in schema$sql)
      try(DBI::dbExecute(demo_con, sql), silent = TRUE)

    # Copy app config / settings so the app starts with sane defaults
    for (tbl in c("labor_settings", "arcade_config")) {
      rows <- tryCatch(DBI::dbGetQuery(prod_con, sprintf("SELECT * FROM %s;", tbl)),
                       error = function(e) data.frame())
      if (nrow(rows))
        for (i in seq_len(nrow(rows)))
          try(DBI::dbExecute(demo_con,
            sprintf("INSERT OR IGNORE INTO %s(key,value) VALUES(?,?);", tbl),
            list(rows$key[i], rows$value[i])), silent = TRUE)
    }

    # Copy arcade_state singleton
    tryCatch({
      st <- DBI::dbGetQuery(prod_con, "SELECT * FROM arcade_state WHERE id=1;")
      if (nrow(st))
        DBI::dbExecute(demo_con,
          "INSERT OR IGNORE INTO arcade_state(id, active_game, assignments_revealed) VALUES(?,?,?);",
          list(1L, st$active_game[1], as.integer(st$assignments_revealed[1] %||% 0L)))
      else
        DBI::dbExecute(demo_con,
          "INSERT OR IGNORE INTO arcade_state(id, active_game, assignments_revealed) VALUES(1,NULL,0);")
    }, error = function(e) NULL)

    copy_table <- function(tbl) {
      rows <- tryCatch(DBI::dbGetQuery(prod_con, sprintf("SELECT * FROM %s;", tbl)),
                       error = function(e) data.frame())
      if (!length(names(rows))) return(invisible(FALSE))
      try(DBI::dbExecute(demo_con, sprintf("DELETE FROM %s;", tbl)), silent = TRUE)
      if (nrow(rows))
        try(DBI::dbWriteTable(demo_con, tbl, rows, append = TRUE, row.names = FALSE), silent = TRUE)
      invisible(TRUE)
    }

    # Mirror the live course job market into sandbox on each demo startup.
    # Demo users and scoring remain separate; jobs/round setup match production.
    for (tbl in c("job_categories", "job_templates", "weekly_rounds", "job_posts"))
      copy_table(tbl)

    # Seed test users (INSERT OR IGNORE so re-runs are safe)
    hash_pw <- if (requireNamespace("bcrypt", quietly = TRUE)) bcrypt::hashpw
               else function(p) p
    test_users <- list(
      list(id = "instructor", name = "Dr. Instructor", admin = 1L, pw = "admin123", sec = NA_character_),
      list(id = "alice",      name = "Alice",           admin = 0L, pw = "test123",  sec = "S01"),
      list(id = "bob",        name = "Bob",             admin = 0L, pw = "test123",  sec = "S01"),
      list(id = "carol",      name = "Carol",           admin = 0L, pw = "test123",  sec = "S01"),
      list(id = "dan",        name = "Dan",             admin = 0L, pw = "test123",  sec = "S02"),
      list(id = "eve",        name = "Eve",             admin = 0L, pw = "test123",  sec = "S02")
    )
    for (u in test_users)
      try(DBI::dbExecute(demo_con,
        "INSERT OR IGNORE INTO users(user_id,display_name,is_admin,pw_hash,section,active,is_demo)
         VALUES(?,?,?,?,?,1,0);",
        list(u$id, u$name, u$admin, hash_pw(u$pw), u$sec)), silent = TRUE)

  }, error = function(e) message("demo_db_bootstrap: ", e$message))
  invisible(demo_con)
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
      if (is_demo) demo_db_bootstrap(local_con, prod_db_path)
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
    db_exec  = function(sql, params = NULL)
      tryCatch(DBI::dbExecute(get_lcon(), sql, params = params),
               error = function(e) { message("demo db_exec: ", e$message); -1L }),
    db_query = function(sql, params = NULL)
      tryCatch(DBI::dbGetQuery(get_lcon(), sql, params = params),
               error = function(e) { message("demo db_query: ", e$message); data.frame() })
  )
}
