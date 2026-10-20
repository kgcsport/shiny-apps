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
      tags$button("Demo (Student)", onclick = "window.location.href='?demo_db=1&demo_as=student'", class = "btn btn-sm btn-default", style = "margin:2px;"),
      tags$details(
        style = "margin-top:8px;font-size:12px;color:#6b5b22;",
        tags$summary(style = "cursor:pointer;font-weight:600;", "Manual demo credentials"),
        tags$div(style = "margin-top:4px;",
          tags$code("instructor / admin123"), tags$br(),
          tags$code("alice / test123")
        )
      )
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

# CREATE TABLE IF NOT EXISTS does not migrate an existing sandbox. Bring the
# job-market tables up to the columns used by the current Settings and bidding
# paths before attempting to seed or edit fake jobs.
reconcile_demo_job_schema <- function(demo_con) {
  DBI::dbExecute(demo_con, "CREATE TABLE IF NOT EXISTS class_wage_snapshots(
    round_id INTEGER NOT NULL, snapshot_key TEXT NOT NULL, job_post_id INTEGER,
    category_id INTEGER, wage REAL NOT NULL, source TEXT,
    snapshotted_at TEXT DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (round_id, snapshot_key));")
  required <- list(
    job_categories = c(
      "default_wage REAL DEFAULT 10", "description TEXT",
      "display_order INTEGER DEFAULT 99", "voluntary INTEGER DEFAULT 0",
      "in_draw INTEGER DEFAULT 1", "selection_time TEXT",
      "contribution_type TEXT", "purpose TEXT", "expected_output TEXT",
      "completion_criterion TEXT"
    ),
    weekly_rounds = c(
      "assignment_mode TEXT DEFAULT 'random'", "bidding_enabled INTEGER DEFAULT 1",
      "bid_open_date TEXT", "bid_close_date TEXT",
      "tickets_per_student INTEGER DEFAULT 10", "class_date TEXT", "tokens_revealed INTEGER DEFAULT 1",
      "tiebreak_method TEXT DEFAULT 'weighted_lottery'",
      "allow_multiple_jobs INTEGER DEFAULT 1",
      "wage_pricing_rule TEXT DEFAULT 'pay_as_bid'"
    ),
    job_posts = c(
      "job_name TEXT", "category_id INTEGER", "slots INTEGER DEFAULT 1",
      "wage_override REAL", "active INTEGER DEFAULT 1",
      "display_order INTEGER DEFAULT 99", "voluntary INTEGER DEFAULT 0",
      "in_draw INTEGER DEFAULT 1", "selection_time TEXT", "description TEXT",
      "created_at TEXT"
    ),
    job_templates = c(
      "category_id INTEGER", "slots INTEGER DEFAULT 1", "suggested_wage REAL",
      "active INTEGER DEFAULT 1", "selection_time TEXT",
      "voluntary INTEGER DEFAULT 0", "in_draw INTEGER DEFAULT 1",
      "display_order INTEGER DEFAULT 99", "description TEXT", "created_at TEXT"
    ),
    job_assignments = c(
      "job_post_id INTEGER", "assigned_wage REAL", "assignment_mode TEXT",
      "status TEXT DEFAULT 'assigned'", "outcome TEXT",
      "tokens_awarded INTEGER DEFAULT 0", "updated_at TEXT",
      "tokens_credited INTEGER DEFAULT 1", "created_at TEXT",
      "scheduled_date TEXT", "display_on_today INTEGER DEFAULT 1"
    ),
    wage_bids = c("min_wage REAL", "submitted_at TEXT"),
    job_wage_bids = c("round_id INTEGER", "job_post_id INTEGER", "user_id TEXT",
                      "min_wage REAL", "submitted_at TEXT"),
    application_bids = c("tickets INTEGER DEFAULT 0", "submitted_at TEXT")
  )

  for (table_name in names(required)) {
    existing <- DBI::dbGetQuery(
      demo_con, sprintf("PRAGMA table_info(%s);", table_name))$name
    if (!length(existing))
      stop(sprintf("Demo schema is missing required table %s.", table_name))
    for (column_def in required[[table_name]]) {
      column_name <- strsplit(column_def, "\\s+")[[1]][1]
      if (!column_name %in% existing) {
        DBI::dbExecute(
          demo_con,
          sprintf("ALTER TABLE %s ADD COLUMN %s;", table_name, column_def))
        existing <- c(existing, column_name)
      }
    }
  }
  tryCatch(
    DBI::dbExecute(demo_con,
      "UPDATE weekly_rounds SET bidding_enabled=0 WHERE assignment_mode='random' AND COALESCE(bidding_enabled,0)<>0;"),
    error = function(e) NULL)
  tryCatch(
    DBI::dbExecute(demo_con,
      "UPDATE weekly_rounds SET class_date=substr(COALESCE(created_at,CURRENT_TIMESTAMP),1,10)
       WHERE class_date IS NULL OR trim(class_date)='';"),
    error = function(e) NULL)
  invisible(TRUE)
}

# ── Synthetic job market for the sandbox ─────────────────────────────────────
# Seed once per disposable demo database. Subsequent browser sessions must not
# reset these tables or they would erase another student's rehearsal bids.
seed_demo_job_market <- function(demo_con) {
  marker <- tryCatch(DBI::dbGetQuery(
    demo_con,
    "SELECT value FROM labor_settings WHERE key='demo_fake_job_market_v1';"),
    error = function(e) data.frame())
  if (nrow(marker)) return(invisible(FALSE))

  DBI::dbWithTransaction(demo_con, {
    fake_jobs <- list(
      list(category="Recap & Synthesis", job="Opening Recap", timing="start", wage=2,
           description="Give a two-minute summary of the previous class and one unresolved question."),
      list(category="Notes & Records", job="Class Note Taker", timing="end", wage=3,
           description="Capture the main claims, graphs, and questions from today's class."),
      list(category="Examples & Evidence", job="Policy Example Scout", timing="end", wage=2,
           description="Find one real-world example or source connected to today's topic."),
      list(category="Critique & Questions", job="Critic / Skeptic", timing="end", wage=2,
           description="Identify one assumption worth questioning and explain why it matters.")
    )

    category_ids <- integer(length(fake_jobs))
    for (i in seq_along(fake_jobs)) {
      job <- fake_jobs[[i]]
      existing <- DBI::dbGetQuery(demo_con,
        "SELECT id FROM job_categories WHERE lower(name)=lower(?) ORDER BY id LIMIT 1;",
        list(job$category))
      if (nrow(existing)) {
        category_ids[i] <- existing$id[1]
      } else {
        DBI::dbExecute(demo_con,
          "INSERT INTO job_categories(name,default_wage,description,display_order,voluntary,in_draw)
           VALUES(?,?,?,?,0,1);",
          list(job$category, job$wage, job$description, i))
        category_ids[i] <- DBI::dbGetQuery(demo_con, "SELECT last_insert_rowid() AS id;")$id[1]
      }
    }

    DBI::dbExecute(demo_con,
      "INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled,bid_open_date,
                                  bid_close_date,tickets_per_student,tokens_revealed,tiebreak_method)
       VALUES('Demo Practice',date('now','localtime'),'application_bidding',1,NULL,NULL,10,0,'weighted_lottery');")
    round_id <- DBI::dbGetQuery(demo_con, "SELECT last_insert_rowid() AS id;")$id[1]

    for (i in seq_along(fake_jobs)) {
      job <- fake_jobs[[i]]
      existing_template <- DBI::dbGetQuery(demo_con,
        "SELECT id FROM job_templates WHERE lower(name)=lower(?) ORDER BY id LIMIT 1;",
        list(job$job))
      if (!nrow(existing_template)) {
        DBI::dbExecute(demo_con,
          "INSERT INTO job_templates(name,category_id,slots,suggested_wage,active,
                                      selection_time,voluntary,in_draw,display_order,description)
           VALUES(?,?,1,?,1,?,0,1,?,?);",
          list(job$job, category_ids[i], job$wage, job$timing, i, job$description))
      }
      DBI::dbExecute(demo_con,
        "INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,active,
                               display_order,selection_time,voluntary,in_draw,description)
         VALUES(?,?,?,1,?,1,?,?,0,1,?);",
        list(round_id, job$job, category_ids[i], job$wage, i,
             job$timing, job$description))
    }

    for (setting in list(
      c("demo_fake_job_market_v1", "1"),
      c("bid_lock_enabled", "0"),
      c("active_course", "DEMO 101"),
      c("active_section", "S01")
    )) {
      DBI::dbExecute(demo_con,
        "INSERT INTO labor_settings(key,value) VALUES(?,?)
         ON CONFLICT(key) DO UPDATE SET value=excluded.value;",
        as.list(setting))
    }
  })
  invisible(TRUE)
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

    reconcile_demo_job_schema(demo_con)

    # CREATE TABLE IF NOT EXISTS does not upgrade an older sandbox schema.
    # Reconcile the login columns explicitly before resetting canonical users;
    # otherwise both the upsert and subsequent login SELECT fail and look like
    # a bad password.
    demo_user_columns <- c(
      "display_name TEXT",
      "pw_hash TEXT",
      "is_admin INTEGER DEFAULT 0",
      "course TEXT",
      "section TEXT",
      "active INTEGER DEFAULT 1",
      "is_demo INTEGER DEFAULT 0"
    )
    existing_user_columns <- DBI::dbGetQuery(demo_con, "PRAGMA table_info(users);")$name
    for (column_def in demo_user_columns) {
      column_name <- strsplit(column_def, "\\s+")[[1]][1]
      if (!column_name %in% existing_user_columns) {
        DBI::dbExecute(demo_con, sprintf("ALTER TABLE users ADD COLUMN %s;", column_def))
        existing_user_columns <- c(existing_user_columns, column_name)
      }
    }

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

    # Add a synthetic bidding round once. Unlike the previous live-catalog
    # mirror, this does not expose real course setup or reset bids when another
    # browser joins the same rehearsal.
    seed_demo_job_market(demo_con)

    # Restore canonical sandbox users on every startup. The demo database is
    # disposable, and preserving an old/corrupt password hash can permanently
    # lock both quick-login buttons.
    hash_pw <- if (requireNamespace("bcrypt", quietly = TRUE)) bcrypt::hashpw
               else function(p) p
    test_users <- list(
      list(id = "instructor", name = "Dr. Instructor", admin = 1L, pw = "admin123", course = "DEMO 101", sec = "S01"),
      list(id = "alice",      name = "Alice",           admin = 0L, pw = "test123",  course = "DEMO 101", sec = "S01"),
      list(id = "bob",        name = "Bob",             admin = 0L, pw = "test123",  course = "DEMO 101", sec = "S01"),
      list(id = "carol",      name = "Carol",           admin = 0L, pw = "test123",  course = "DEMO 101", sec = "S01"),
      list(id = "dan",        name = "Dan",             admin = 0L, pw = "test123",  course = "DEMO 101", sec = "S02"),
      list(id = "eve",        name = "Eve",             admin = 0L, pw = "test123",  course = "DEMO 101", sec = "S02")
    )
    for (u in test_users) {
      DBI::dbExecute(demo_con,
        "INSERT INTO users(user_id,display_name,is_admin,pw_hash,course,section,active,is_demo)
         VALUES(?,?,?,?,?,?,1,0)
         ON CONFLICT(user_id) DO UPDATE SET
           display_name=excluded.display_name,
           is_admin=excluded.is_admin,
           pw_hash=excluded.pw_hash,
           course=excluded.course,
           section=excluded.section,
           active=1,
           is_demo=0;",
        list(u$id, u$name, u$admin, hash_pw(u$pw), u$course, u$sec))
    }

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
