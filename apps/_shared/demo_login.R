# _shared/demo_login.R
#
# Adds a "Quick test login" panel to any app's login form.
# The panel is hidden by default and only appears when ?demo=1 is in the URL.
#
# Usage:
#   source("../_shared/demo_login.R")   # in any app
#   # then include demo_login_ui inside login_ui:
#   login_ui <- function(msg = NULL) {
#     fluidPage(..., demo_login_ui)
#   }
#
# Test credentials (set up by tests/setup/seed_test_db.R):
#   alice / bob / carol  — section S01, password test123
#   dan / eve            — section S02, password test123
#   instructor           — admin, password admin123

demo_login_ui <- tagList(
  tags$div(
    id    = "demo-login-panel",
    style = paste0(
      "display:none;margin-top:14px;padding:10px 12px;",
      "background:#fff8e1;border:1px solid #ffe082;border-radius:4px;"
    ),
    tags$p(
      tags$strong("Test users"),
      tags$span(" — only shown when ", tags$code("?demo=1"), " is in the URL"),
      style = "margin:0 0 8px;font-size:13px;"
    ),
    tags$button("Alice (S01)",  onclick = "demoLogin('alice','test123')",      class = "btn btn-xs btn-default", style = "margin:2px;"),
    tags$button("Bob (S01)",    onclick = "demoLogin('bob','test123')",        class = "btn btn-xs btn-default", style = "margin:2px;"),
    tags$button("Carol (S01)",  onclick = "demoLogin('carol','test123')",      class = "btn btn-xs btn-default", style = "margin:2px;"),
    tags$button("Dan (S02)",    onclick = "demoLogin('dan','test123')",        class = "btn btn-xs btn-default", style = "margin:2px;"),
    tags$button("Eve (S02)",    onclick = "demoLogin('eve','test123')",        class = "btn btn-xs btn-default", style = "margin:2px;"),
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
        if (new URLSearchParams(window.location.search).get("demo") === "1") {
          var panel = document.getElementById("demo-login-panel");
          if (panel) panel.style.display = "block";
        }
      }
      // Works for both static and renderUI login forms
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", revealIfDemo);
      } else {
        revealIfDemo();
      }
      // Also re-check after Shiny re-renders the login form
      document.addEventListener("shiny:value", revealIfDemo);
    })();
  '))
)
