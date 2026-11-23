suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
  library(shiny)
})

app_file <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}

test_that("instructor grading surfaces show the team's submitted policy work", {
  old_connect <- Sys.getenv("CONNECT_CONTENT_DIR", unset=NA_character_)
  old_demo <- Sys.getenv("DEMO_MODE", unset=NA_character_)
  old_admin_emails <- Sys.getenv("ADMIN_EMAILS", unset=NA_character_)
  fixture_dir <- tempfile("policy-submission-review-")
  dir.create(file.path(fixture_dir, "data"), recursive=TRUE)
  Sys.setenv(CONNECT_CONTENT_DIR=fixture_dir)
  Sys.unsetenv("DEMO_MODE")
  Sys.unsetenv("ADMIN_EMAILS")
  on.exit({
    if (is.na(old_connect)) Sys.unsetenv("CONNECT_CONTENT_DIR") else
      Sys.setenv(CONNECT_CONTENT_DIR=old_connect)
    if (is.na(old_demo)) Sys.unsetenv("DEMO_MODE") else
      Sys.setenv(DEMO_MODE=old_demo)
    if (is.na(old_admin_emails)) Sys.unsetenv("ADMIN_EMAILS") else
      Sys.setenv(ADMIN_EMAILS=old_admin_emails)
    unlink(fixture_dir, recursive=TRUE, force=TRUE)
  }, add=TRUE)

  app <- new.env(parent=globalenv())
  suppressWarnings(source(app_file, local=app))
  on.exit(suppressWarnings(try(
    if (!is.null(app$conn) && DBI::dbIsValid(app$conn)) DBI::dbDisconnect(app$conn),
    silent=TRUE)), add=TRUE)

  for (uid in c("review-alice", "review-bob")) {
    app$db_exec(
      "INSERT INTO users(user_id,display_name,is_admin,course,section,active,is_demo)
       VALUES(?,?,0,'ECON 342','A',1,0);",
      list(uid, tools::toTitleCase(sub("review-", "", uid))))
    app$upsert_policy_group_assignment(
      uid, "Review Team", "2026-12-01", "Family policy",
      policy_topic="Child tax credits",
      policy_question="How should the child tax credit balance poverty reduction and work incentives?")
  }
  app$db_exec(
    "INSERT INTO policy_progress_reports(
       course,policy_team,feedback,why_matters,diagnosis,revision_plan,uncertainty,
       status,submitted_at,updated_by
     ) VALUES('ECON 342','Review Team',?,?,?,?,?,'submitted',CURRENT_TIMESTAMP,'review-alice');",
    list("Clarify the target population.",
         "Eligibility changes the policy incidence.",
         "Our evidence does not separate short- and long-run labor supply.",
         "Add subgroup estimates and compare two phase-out designs.",
         "Take-up under automatic enrollment remains uncertain."))

  progress_responses <- c(
    "How should the child tax credit balance poverty reduction and work incentives?",
    "Clarify the target population.",
    "Eligibility changes the policy incidence.",
    "Our evidence does not separate short- and long-run labor supply.",
    "Add subgroup estimates and compare two phase-out designs.",
    "Take-up under automatic enrollment remains uncertain."
  )

  shiny::testServer(app$server, {
    rv$authed <- TRUE
    rv$is_admin <- TRUE
    rv$impersonating <- FALSE
    rv$user_id <- "review-admin"
    rv$admin_role <- "super_admin"
    rv$active_course <- "ECON 342"
    rv$active_sections <- "A"

    session$setInputs(
      config_action="policy_rubrics",
      policy_rubric_team="Review Team",
      policy_rubric_component="progress"
    )
    session$flushReact()
    grader_html <- paste(as.character(output$config_panel), collapse="")
    expect_true(all(vapply(progress_responses[-1], grepl, logical(1),
                           x=grader_html, fixed=TRUE)))
    expect_true(grepl("Alice, Bob", grader_html, fixed=TRUE))
    expect_false(grepl(progress_responses[1], grader_html, fixed=TRUE))

    session$setInputs(policy_admin_team="Review Team")
    session$flushReact()
    review_html <- paste(as.character(output$policy_brief_tab), collapse="")
    expect_true(all(vapply(progress_responses, grepl, logical(1),
                           x=review_html, fixed=TRUE)))
    expect_true(grepl("Grade submissions and provide feedback under Settings",
                      review_html, fixed=TRUE))
    expect_false(grepl("policy_topic_status", review_html, fixed=TRUE))
    expect_false(grepl("save_policy_topic_review_btn", review_html, fixed=TRUE))

    session$setInputs(policy_rubric_component="question")
    session$flushReact()
    question_html <- paste(as.character(output$config_panel), collapse="")
    expect_true(grepl("Child tax credits", question_html, fixed=TRUE))
    expect_true(grepl(progress_responses[1], question_html, fixed=TRUE))
    expect_true(grepl("policy_grader_topic_note", question_html, fixed=TRUE))
    expect_true(grepl("Alice, Bob", question_html, fixed=TRUE))
    expect_false(grepl(progress_responses[2], question_html, fixed=TRUE))

    session$setInputs(
      policy_grader_topic_status="needs_revision",
      policy_grader_topic_note="Narrow the population and implementation margin.",
      save_policy_grader_question_btn=1
    )
    session$flushReact()
    saved_review <- db_query(
      "SELECT DISTINCT topic_status,topic_note FROM policy_group_assignments
       WHERE policy_team='Review Team';")
    expect_equal(saved_review$topic_status, "needs_revision")
    expect_equal(saved_review$topic_note,
                 "Narrow the population and implementation margin.")

    session$setInputs(policy_rubric_component="presentation")
    session$flushReact()
    presentation_html <- paste(as.character(output$config_panel), collapse="")
    expect_true(grepl(progress_responses[1], presentation_html, fixed=TRUE))
    expect_true(grepl("delivered live", presentation_html, fixed=TRUE))

    session$setInputs(policy_rubric_component="brief")
    session$flushReact()
    brief_html <- paste(as.character(output$config_panel), collapse="")
    expect_true(grepl("No written-brief submission is stored in this app yet.",
                      brief_html, fixed=TRUE))
  })
})

cat("policy submission review regression passed\n")
