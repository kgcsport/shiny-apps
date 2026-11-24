#!/usr/bin/env Rscript

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
source_text <- paste(readLines(app_path, warn=FALSE), collapse="\n")
required <- c(
  '"Class day" = "round_setup"', '"Tokens & spending" = "token_admin"',
  '"Policy project" = "policy_rubrics"', '"Data & tools" = "exports"',
  '"Pass insurance"="insurance"',
  '"Assignment extensions"="extensions"',
  '"Exam questions"="flex_questions"',
  '"Grade reweighting"="reweight"',
  'sprintf("%s on hand · %s gross"',
  'observeEvent(input$save_insurance_product_btn',
  'observeEvent(input$save_extension_pricing_btn',
  'observeEvent(input$save_flex_cost_btn',
  'observeEvent(input$save_rw_setup_btn'
)
missing <- required[!vapply(required, grepl, logical(1), x=source_text, fixed=TRUE)]
if (length(missing)) stop("Settings organization contract missing: ", paste(missing, collapse=" | "))

# The four systems no longer occupy separate top-level dropdown entries.
top_select <- regmatches(source_text, regexpr(
  'SETTINGS_PAGES <- c\\([\\s\\S]{1,600}?\\)\n',
  source_text, perl=TRUE))
if (!length(top_select) || !nzchar(top_select)) stop("Could not find Settings section selector.")
for (legacy in c('"Insurance"="insurance"', '"Extensions"="extensions"',
                 '"Flex Questions"="flex_questions"')) {
  if (grepl(legacy, top_select, fixed=TRUE)) stop("Legacy top-level Settings entry remains: ", legacy)
}

if (!requireNamespace("shiny",quietly=TRUE) || !requireNamespace("DBI",quietly=TRUE) ||
    !requireNamespace("RSQLite",quietly=TRUE)) {
  cat("critical Settings organization: source contract OK\n"); quit(status=0)
}

scratch <- tempfile("settings-organization-")
dir.create(file.path(scratch,"data"), recursive=TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset=NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR=scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR=old_content)
  unlink(scratch,recursive=TRUE)
}, add=TRUE)
app <- new.env(parent=globalenv())
source(app_path, local=app)
app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo) VALUES('admin@test.edu','Instructor','',1,'ECON 342','01',1,0);")
app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo) VALUES('student@test.edu','Student A','',0,'ECON 342','01',1,0);")
app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES('student@test.edu','ECON 342','01');")
app$db_exec("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES('student@test.edu','Student A','job',3,1,'gross fixture');")
app$db_exec("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES('student@test.edu','Student A','extension_purchase',-1,0,'spend fixture');")

shiny::testServer(app$server, {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
  rv$course <- "ECON 342"; rv$section <- "01"; rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"; rv$active_course <- "ECON 342"
  rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE

  session$setInputs(config_action="students"); session$flushReact()
  students_html <- paste(as.character(output$config_panel),collapse="")
  for (label in c("Student Roster", "Student A", "2 on hand · 3 gross",
                  "Administrator access", "Add student", "Reset password",
                  "Bulk upload students"))
    if (!grepl(label,students_html,fixed=TRUE)) stop("Students panel is missing: ",label)
  if (grepl("Policy group assignments",students_html,fixed=TRUE))
    stop("Policy group assignments should sit on the Policy project page.")

  # Seven pages; every folded-in section appears once, on the page that uses it.
  if (length(app$SETTINGS_PAGES) != 7) stop("Settings should have seven pages.")
  page_html <- function(id) {
    session$setInputs(config_action=id); session$flushReact()
    paste(as.character(output$config_panel),collapse="")
  }
  expect_on <- function(id, labels) {
    html <- page_html(id)
    for (label in labels) if (!grepl(label,html,fixed=TRUE))
      stop("Settings page ",id," is missing: ",label)
    html
  }
  class_day <- expect_on("round_setup", c("Wage and bidding rules", "Announcement for students",
                                           "save_today_announcement_btn", "Games"))
  jobs_html <- expect_on("jobs", c("Jobs for the selected class date", "Job types", "Templates"))
  if (grepl("Wage and bidding rules",jobs_html,fixed=TRUE))
    stop("Wage and bidding rules should appear only on the Class day page.")
  expect_on("token_admin", c("Award or deduct for a whole class or section", "Random token lottery",
                             "Practice-date pay", "Adjust one account", "Award a chosen group",
                             "What students can spend tokens on", "Spending controls"))
  expect_on("policy_rubrics", "Policy group assignments")
  expect_on("exports", c("save_app_name_btn", "Other apps"))
  # Old section addresses open the page that now holds them.
  for (old in c("spending","game_controls","app_settings","sandbox_demo")) {
    html <- page_html(old)
    if (!nzchar(html)) stop("Old Settings address renders nothing: ",old)
  }
  # No input id is rendered twice on any page.
  for (id in unname(app$SETTINGS_PAGES)) {
    ids <- regmatches(page_html(id), gregexpr('id="[^"]+"', page_html(id)))[[1]]
    dup <- unique(ids[duplicated(ids)])
    if (length(dup)) stop("Settings page ",id," repeats ids: ",paste(dup,collapse=", "))
  }

  panels <- c(spending_overview="Spending options", insurance="Pass Insurance Market",
              extensions="Extension Pricing", flex_questions="Flex Questions",
              reweight="Grade Reweighting")
  for (panel in names(panels)) {
    session$setInputs(config_action="spending", spending_config_action=panel)
    session$flushReact()
    html <- paste(as.character(output$config_panel),collapse="")
    if (!grepl("Spending controls",html,fixed=TRUE) ||
        !grepl(panels[[panel]],html,fixed=TRUE))
      stop("Spending subpanel did not render with its shared navigation: ",panel)
  }

  session$setInputs(config_action="gradebook"); session$flushReact()
  gradebook_html <- paste(as.character(output$config_panel),collapse="")
  if (grepl('id="rw_costs_input"',gradebook_html,fixed=TRUE))
    stop("Grade reweight setup still renders inside Gradebook.")
})

cat("critical Settings organization: PASS\n")
