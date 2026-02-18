library(shiny)

# Helper: list app dirs under /srv/shiny-server (excluding some)
list_apps <- function(root = "/srv/shiny-server") {
  dirs <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  dirs <- dirs[dirs != ""]
  # hide internal/system dirs
  hide <- c("appdata", "_logs", "home", "sample-apps")
  dirs[!dirs %in% hide]
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { max-width: 900px; margin: 2rem auto; }
    .card { padding: 1rem 1.25rem; border: 1px solid #ddd; border-radius: 12px; margin-bottom: .75rem; }
    .card a { font-size: 1.1rem; text-decoration: none; }
    .muted { color: #666; }
  "))),
  titlePanel("Class apps"),
  p(class="muted", "Pick an app:"),
  uiOutput("app_list"),
  hr(),
  p(class="muted", "Tip: bookmark specific apps like /class-job-picker/ or /airplanes/.")
)

server <- function(input, output, session) {
  output$app_list <- renderUI({
    apps <- list_apps()
    if (length(apps) == 0) return(div(class="muted", "No apps found."))
    tagList(lapply(sort(apps), function(a) {
      div(class="card",
          tags$a(href = paste0("/", a, "/"), a)
      )
    }))
  })
}

shinyApp(ui, server)

