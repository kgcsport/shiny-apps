library(shiny)

# ── File catalog ──────────────────────────────────────────────────────────────
# Shiny sets wd to the app directory, so "." = ai-teaching-tool-demo-kit/
kit_md <- function(subdir = NULL) {
  d <- if (is.null(subdir)) "." else subdir
  sort(list.files(d, pattern = "\\.md$", full.names = FALSE))
}

make_group <- function(subdir = NULL, prefix = "") {
  files <- kit_md(subdir)
  if (!length(files)) return(character(0))
  keys   <- if (nzchar(prefix)) paste0(prefix, "/", files) else files
  labels <- tools::file_path_sans_ext(files)
  setNames(keys, labels)
}

NAV <- Filter(length, list(
  "Kit files"     = make_group(),
  "Examples"      = make_group("examples",      "examples"),
  "Starter specs" = make_group("starter_specs", "starter_specs")
))
ALL_PATHS <- unlist(NAV, use.names = FALSE)
FIRST     <- if (length(ALL_PATHS)) ALL_PATHS[[1]] else ""

read_kit <- function(rel) {
  tryCatch(paste(readLines(rel, warn = FALSE), collapse = "\n"),
           error = function(e) "*(file not found)*")
}

render_md <- function(rel) {
  txt  <- read_kit(rel)
  html <- tryCatch(commonmark::markdown_html(txt),
                   error = function(e) paste0("<pre>", htmltools::htmlEscape(txt), "</pre>"))
  HTML(html)
}

# ── CSS ────────────────────────────────────────────────────────────────────────
CSS <- "
body { font-family: system-ui, -apple-system, sans-serif; background: #f5f5f5;
       color: #1a1a1a; font-size: 15px; }
.kit-layout { display: flex; min-height: 100vh; }
.kit-sidebar { width: 230px; flex-shrink: 0; background: #fff;
               border-right: 1px solid #e0e0e0; padding: 1.25rem 1rem;
               position: sticky; top: 0; height: 100vh; overflow-y: auto;
               box-sizing: border-box; }
.kit-brand { font-size: 1rem; font-weight: 700; color: #1a1a1a;
             margin-bottom: 1.25rem; line-height: 1.2; }
.kit-brand small { display: block; font-size: .72rem; font-weight: 400;
                   color: #888; margin-top: .15rem; }
.kit-group-label { font-size: .68rem; font-weight: 700; text-transform: uppercase;
                   letter-spacing: .09em; color: #aaa; margin: 1rem 0 .3rem;
                   padding-top: .6rem; border-top: 1px solid #f0f0f0; }
.kit-group-label:first-of-type { margin-top: 0; border-top: none; }
.kit-nav-link { display: block; font-size: .86rem; padding: .28rem .5rem;
                border-radius: 5px; cursor: pointer; color: #444;
                border: none; background: none; width: 100%; text-align: left;
                margin-bottom: .05rem; }
.kit-nav-link:hover { background: #f0f0f0; color: #1a1a1a; }
.kit-nav-link.active { background: #eaf0ff; color: #2d5be3; font-weight: 600; }
.kit-main { flex: 1; padding: 2rem 2.5rem 3rem; max-width: 820px; min-width: 0; }
.kit-toolbar { display: flex; align-items: center; gap: .6rem;
               margin-bottom: 1.5rem; flex-wrap: wrap; }
.kit-toolbar .btn { font-size: .82rem; padding: .3rem .85rem; border-radius: 6px; }
.kit-doc { background: #fff; border-radius: 10px; border: 1px solid #e5e5e5;
           padding: 2rem 2.25rem; }
.kit-doc h1 { font-size: 1.6rem; font-weight: 700; margin: 0 0 1rem; line-height: 1.2; }
.kit-doc h2 { font-size: 1.15rem; font-weight: 700; margin: 1.75rem 0 .5rem;
              border-bottom: 1px solid #eee; padding-bottom: .3rem; }
.kit-doc h3 { font-size: .95rem; font-weight: 700; margin: 1.25rem 0 .35rem; }
.kit-doc p  { margin: 0 0 .85rem; line-height: 1.65; color: #333; }
.kit-doc ul, .kit-doc ol { margin: 0 0 .85rem 1.4rem; color: #333; line-height: 1.65; }
.kit-doc li { margin-bottom: .2rem; }
.kit-doc pre { background: #f4f6f9; border: 1px solid #e0e4ea; border-radius: 6px;
               padding: .9rem 1rem; overflow-x: auto; font-size: .84em; }
.kit-doc code { background: #f0f2f5; border-radius: 3px; padding: .1em .35em;
                font-size: .85em; }
.kit-doc pre code { background: none; padding: 0; }
.kit-doc blockquote { border-left: 3px solid #ddd; margin: .75rem 0 .75rem 0;
                      padding: .25rem 1rem; color: #666; }
.kit-doc table { border-collapse: collapse; width: 100%; margin-bottom: .85rem;
                 font-size: .9em; }
.kit-doc th, .kit-doc td { border: 1px solid #ddd; padding: .45rem .7rem; text-align: left; }
.kit-doc th { background: #f5f5f5; font-weight: 600; }
.kit-doc a { color: #2d5be3; }
@media (max-width: 680px) {
  .kit-layout { flex-direction: column; }
  .kit-sidebar { width: 100%; height: auto; position: static;
                 border-right: none; border-bottom: 1px solid #e0e0e0; }
  .kit-main { padding: 1.25rem 1rem; }
}
"

# ── UI ────────────────────────────────────────────────────────────────────────
sidebar_nav <- tagList(
  div(class = "kit-brand", "AI Teaching Tool Kit",
      tags$small("Instructor-owned classroom tools")),
  lapply(names(NAV), function(grp) {
    keys <- NAV[[grp]]
    tagList(
      div(class = "kit-group-label", grp),
      lapply(seq_along(keys), function(i) {
        k   <- keys[[i]]
        lbl <- names(keys)[i]
        tags$button(
          class    = "kit-nav-link",
          id       = paste0("nav_", gsub("[^a-zA-Z0-9]", "_", k)),
          onclick  = sprintf("Shiny.setInputValue('selected_file','%s',{priority:'event'});", k),
          lbl
        )
      })
    )
  }),
  tags$hr(style = "margin: 1.25rem 0 .75rem; border-color: #eee;"),
  downloadButton("dl_all_zip", "Download all (ZIP)",
                 class = "btn btn-sm btn-outline-secondary",
                 style = "width:100%;font-size:.8rem;")
)

ui <- fluidPage(
  tags$head(
    tags$title("AI Teaching Tool Demo Kit"),
    tags$style(HTML(CSS)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('highlight_nav', function(key) {
        document.querySelectorAll('.kit-nav-link').forEach(function(el) {
          el.classList.remove('active');
        });
        var id = 'nav_' + key.replace(/[^a-zA-Z0-9]/g, '_');
        var el = document.getElementById(id);
        if (el) el.classList.add('active');
      });
    "))
  ),
  div(class = "kit-layout",
    div(class = "kit-sidebar", sidebar_nav),
    div(class = "kit-main",
      div(class = "kit-toolbar",
        downloadButton("dl_current", "Download this file",
                       class = "btn btn-sm btn-primary"),
        tags$span(style = "color:#aaa;font-size:.8rem;", uiOutput("current_path_label", inline = TRUE))
      ),
      uiOutput("doc_panel")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  sel <- reactiveVal(FIRST)

  observeEvent(input$selected_file, {
    if (nzchar(input$selected_file %||% "")) {
      sel(input$selected_file)
      session$sendCustomMessage("highlight_nav", input$selected_file)
    }
  })

  # Highlight first item on load
  observe({
    session$sendCustomMessage("highlight_nav", FIRST)
  })

  output$doc_panel <- renderUI({
    f <- sel()
    if (!nzchar(f %||% ""))
      return(div(class = "kit-doc",
                 tags$p(style = "color:#aaa;", "Select a file from the sidebar.")))
    div(class = "kit-doc", render_md(f))
  })

  output$current_path_label <- renderUI({
    f <- sel()
    if (nzchar(f %||% "")) tags$code(f) else NULL
  })

  output$dl_current <- downloadHandler(
    filename = function() basename(sel()),
    content  = function(file) {
      writeLines(read_kit(sel()), file)
    }
  )

  output$dl_all_zip <- downloadHandler(
    filename = function() paste0("ai-teaching-tool-demo-kit-", Sys.Date(), ".zip"),
    content  = function(file) {
      tmp <- tempfile()
      dir.create(tmp)
      for (rel in ALL_PATHS) {
        dest <- file.path(tmp, basename(rel))
        writeLines(read_kit(rel), dest)
      }
      zip(file, files = list.files(tmp, full.names = TRUE), flags = "-j")
    }
  )
}

shinyApp(ui, server)
