library(shiny)

# Human-readable display names keyed by directory slug.
# Falls back to title-cased slug for any new apps not listed here.
APP_NAMES <- c(
  "airplanes-game"         = "Airplanes",
  "club-insurance-game"    = "Clubs & Insurance",
  "coordination-games"     = "Coordination Games",
  "excise-tax-game"        = "Excise Tax",
  "oligopoly-game"         = "Oligopoly",
  "restricted-seller-game" = "Restricted Seller",
  "supply-auction-game"    = "Supply Auction",
  "class-job-picker"       = "Class Job Picker",
  "core-arcade"            = "CORE Arcade",
  "final_question_reveal"  = "Final Question Reveal",
  "indifference-to-demand" = "Indifference to Demand",
  "tax-incidence"          = "Tax Incidence",
  "theory-of-firm"         = "Theory of the Firm"
)

app_label <- function(slug) {
  if (slug %in% names(APP_NAMES)) APP_NAMES[[slug]]
  else tools::toTitleCase(gsub("[-_]", " ", slug))
}

is_game <- function(slug) grepl("-games?$", slug)

list_apps <- function(root = "/srv/shiny-server") {
  dirs <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  dirs <- dirs[nzchar(dirs)]
  hide <- c("appdata", "_logs", "_shared", "home", "sample-apps", "default-apps", "bonus-entry")
  dirs[!dirs %in% hide]
}

app_card <- function(slug) {
  div(class = "card",
    tags$a(href = paste0("/", slug, "/"), app_label(slug)),
    tags$span(class = "arrow", "\u2192")
  )
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {
      max-width: 820px;
      margin: 2.5rem auto;
      font-family: system-ui, -apple-system, sans-serif;
      font-size: 18px;
      background: #fafafa;
    }
    .page-title {
      font-size: 2.2rem;
      font-weight: 700;
      color: #951829;
      margin-bottom: 0.2rem;
    }
    .subtitle {
      color: #777;
      margin-bottom: 2rem;
      font-size: 1rem;
    }
    .section-header {
      font-size: 0.85rem;
      font-weight: 700;
      color: #951829;
      text-transform: uppercase;
      letter-spacing: .1em;
      border-bottom: 2px solid #951829;
      padding-bottom: .3rem;
      margin: 1.75rem 0 .6rem;
    }
    .card {
      display: flex;
      align-items: center;
      padding: 1rem 1.25rem;
      background: #fff;
      border: 1px solid #e8e8e8;
      border-radius: 10px;
      margin-bottom: .5rem;
      transition: box-shadow .15s, border-color .15s;
    }
    .card:hover {
      box-shadow: 0 2px 10px rgba(149,24,41,.12);
      border-color: #951829;
    }
    .card a {
      font-size: 1.15rem;
      text-decoration: none;
      color: #1a1a1a;
      flex: 1;
    }
    .card:hover a { color: #951829; }
    .arrow {
      color: #ccc;
      font-size: 1.1rem;
      transition: color .15s, transform .15s;
    }
    .card:hover .arrow {
      color: #951829;
      transform: translateX(3px);
    }
    .footer-note {
      color: #999;
      font-size: .88rem;
      margin-top: 2.5rem;
    }
  "))),
  div(class = "page-title", "Class Apps"),
  div(class = "subtitle", "Pick an app to get started."),
  uiOutput("app_list"),
  div(class = "footer-note",
    "Tip: you can bookmark individual apps directly, e.g. ",
    tags$code("/airplanes-game/"), "."
  )
)

server <- function(input, output, session) {
  output$app_list <- renderUI({
    apps  <- list_apps()
    if (!length(apps)) return(div(class = "subtitle", "No apps found."))

    games <- sort(apps[sapply(apps, is_game)])
    demos <- sort(apps[!sapply(apps, is_game)])

    tagList(
      if (length(games)) tagList(
        div(class = "section-header", "Games"),
        lapply(games, app_card)
      ),
      if (length(demos)) tagList(
        div(class = "section-header", "Tools & Demos"),
        lapply(demos, app_card)
      )
    )
  })
}

shinyApp(ui, server)
