# tests/unit/test-prompt-builder.R
# Unit tests for demo-kit-site: parse_sections(), prompt generation,
# file validation, and CLI command construction.
# Run: testthat::test_file("tests/unit/test-prompt-builder.R")

here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "tests/unit")
source(file.path(here, "helpers.R"), local = TRUE)

# ── parse_sections (from demo-kit-site/app.R) ─────────────────────────────────

parse_sections <- function(txt) {
  markers <- c("=== app.R ===", "=== README.md ===", "=== install.R ===")
  keys    <- c("app_r", "readme", "install_r")
  out     <- setNames(as.list(rep("", 3)), keys)
  for (i in seq_along(markers)) {
    s <- regexpr(markers[i], txt, fixed = TRUE)
    if (s == -1) next
    body_start <- s + nchar(markers[i])
    body_end   <- nchar(txt)
    for (j in seq_along(markers)[-i]) {
      ns <- regexpr(markers[j], txt, fixed = TRUE)
      if (ns > body_start && ns < body_end) body_end <- ns - 1
    }
    out[[keys[i]]] <- trimws(substr(txt, body_start, body_end))
  }
  out
}

# ── parse_sections tests ──────────────────────────────────────────────────────

test_that("parse_sections: extracts all three sections", {
  txt <- "=== app.R ===\nlibrary(shiny)\n=== README.md ===\n# My App\n=== install.R ===\ninstall.packages('shiny')"
  res <- parse_sections(txt)
  expect_equal(res$app_r,    "library(shiny)")
  expect_equal(res$readme,   "# My App")
  expect_equal(res$install_r, "install.packages('shiny')")
})

test_that("parse_sections: missing section returns empty string", {
  txt <- "=== app.R ===\nlibrary(shiny)\n=== install.R ===\ninstall.packages('shiny')"
  res <- parse_sections(txt)
  expect_equal(res$app_r,     "library(shiny)")
  expect_equal(res$readme,    "")
  expect_equal(res$install_r, "install.packages('shiny')")
})

test_that("parse_sections: all sections missing → all empty strings", {
  res <- parse_sections("No markers here at all.")
  expect_equal(res$app_r,    "")
  expect_equal(res$readme,   "")
  expect_equal(res$install_r, "")
})

test_that("parse_sections: body does not include the next marker text", {
  txt <- "=== app.R ===\ncode here\n=== README.md ===\nreadme text"
  res <- parse_sections(txt)
  expect_false(grepl("README", res$app_r))
})

test_that("parse_sections: multi-line bodies preserved", {
  app_body <- "library(shiny)\n\nui <- fluidPage()\nserver <- function(i,o,s){}\nshinyApp(ui, server)"
  txt <- paste0("=== app.R ===\n", app_body, "\n=== README.md ===\nREADME")
  res <- parse_sections(txt)
  expect_equal(res$app_r, trimws(app_body))
})

# ── Prompt generation ─────────────────────────────────────────────────────────

build_prompt <- function(course = "[your course]", size = "[class size]",
                         type = "[activity type]", goal = "[learning goal]",
                         student = "[what students do]",
                         instr   = "[setup, round controls, reset, export]",
                         display = "[what goes on the shared screen]",
                         scoring = "[scoring or payoff rules]",
                         stack   = "R Shiny + SQLite",
                         constr  = "Must run with fake data. No external APIs.") {
  sprintf(
"I want you to build the smallest working prototype of a classroom teaching tool.

Course: %s
Class size: %s
Activity type: %s
Learning goal: %s
Student actions: %s
Instructor actions: %s
Public display: %s
Private instructor controls: [setup screen, round controls, data export, raw event log]
Scoring or payoff rules: %s
Data to save: timestamp, session_id, round_number, participant_id, action, outcome
Data not to save: real names, emails, grades, demographic info, accommodations, private notes
Export format: CSV for raw events and round summaries
Preferred tech stack: %s
Known constraints: %s

Requirements:
- Use fake data only during development.
- Do not ask for real student names, emails, grades, or private notes.
- Build a minimal working version first.
- Keep activity rules and scoring logic transparent and inspectable.
- Store raw event data before computing summaries.
- Create student-facing and instructor-facing views when needed.
- Include a reset / test mode.
- Include exportable raw data in CSV or SQLite.
- Write a README explaining how to install, run, test, reset, and export.",
    course, size, type, goal, student, instr, display, scoring, stack, constr)
}

test_that("build_prompt: filled fields appear verbatim in output", {
  p <- build_prompt(course = "ECON 101", size = "30", type = "Auction",
                    goal   = "Understand price discovery")
  expect_match(p, "ECON 101")
  expect_match(p, "Class size: 30")
  expect_match(p, "Activity type: Auction")
  expect_match(p, "Understand price discovery")
})

test_that("build_prompt: unfilled fields use placeholder text", {
  p <- build_prompt()
  expect_match(p, "\\[your course\\]")
  expect_match(p, "\\[class size\\]")
})

test_that("build_prompt: requirements section always present", {
  p <- build_prompt()
  expect_match(p, "Requirements:")
  expect_match(p, "Use fake data only")
  expect_match(p, "reset / test mode")
})

test_that("build_prompt: stack choice is reflected", {
  p1 <- build_prompt(stack = "Python Dash + SQLite")
  expect_match(p1, "Python Dash")
  p2 <- build_prompt(stack = "R Shiny + CSV")
  expect_match(p2, "R Shiny \\+ CSV")
})

# ── CLI command construction ──────────────────────────────────────────────────

build_cli_cmd <- function(tool, full_prompt) {
  safe <- function(s) gsub("'", "'\\''", s)
  switch(tool,
    claude_code = paste0("claude -p '", safe(full_prompt), "'"),
    codex       = paste0("codex -q '", safe(full_prompt), "'"),
    gemini      = paste0("gemini -m gemini-2.0-flash '", safe(full_prompt), "'"),
    full_prompt
  )
}

test_that("build_cli_cmd: claude_code wraps prompt with claude -p", {
  cmd <- build_cli_cmd("claude_code", "build me a shiny app")
  expect_match(cmd, "^claude -p '")
  expect_match(cmd, "build me a shiny app'$")
})

test_that("build_cli_cmd: codex uses codex -q", {
  cmd <- build_cli_cmd("codex", "build me a shiny app")
  expect_match(cmd, "^codex -q '")
})

test_that("build_cli_cmd: gemini uses gemini -m gemini-2.0-flash", {
  cmd <- build_cli_cmd("gemini", "build me a shiny app")
  expect_match(cmd, "^gemini -m gemini-2\\.0-flash '")
})

test_that("build_cli_cmd: chat returns raw prompt", {
  p   <- "build me a shiny app"
  cmd <- build_cli_cmd("chat", p)
  expect_equal(cmd, p)
})

test_that("build_cli_cmd: single quotes in prompt are shell-escaped", {
  cmd <- build_cli_cmd("claude_code", "it's a test")
  expect_match(cmd, "it'\\\\''s a test")
})

# ── R file validation (parse-based) ──────────────────────────────────────────

validate_r_syntax <- function(path) {
  tryCatch({
    parse(path)
    list(ok = TRUE, msg = "")
  }, error = function(e) {
    list(ok = FALSE, msg = conditionMessage(e))
  })
}

test_that("validate_r_syntax: valid R file passes", {
  f <- tempfile(fileext = ".R")
  writeLines(c("library(shiny)", "x <- 1 + 1"), f)
  on.exit(unlink(f))
  result <- validate_r_syntax(f)
  expect_true(result$ok)
})

test_that("validate_r_syntax: file with syntax error fails", {
  f <- tempfile(fileext = ".R")
  writeLines(c("library(shiny)", "x <- (1 + "), f)   # unclosed paren
  on.exit(unlink(f))
  result <- validate_r_syntax(f)
  expect_false(result$ok)
  expect_true(nzchar(result$msg))
})

test_that("validate_r_syntax: empty file is valid R", {
  f <- tempfile(fileext = ".R")
  writeLines(character(0), f)
  on.exit(unlink(f))
  result <- validate_r_syntax(f)
  expect_true(result$ok)
})

test_that("validate_r_syntax: minimal shiny app structure is valid", {
  f <- tempfile(fileext = ".R")
  writeLines(c(
    "library(shiny)",
    "ui <- fluidPage()",
    "server <- function(input, output, session) {}",
    "shinyApp(ui, server)"
  ), f)
  on.exit(unlink(f))
  result <- validate_r_syntax(f)
  expect_true(result$ok)
})
