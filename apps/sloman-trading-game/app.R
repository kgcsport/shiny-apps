library(shiny)
library(ggplot2)
library(scales)

`%||%` <- function(a, b) if (!is.null(a)) a else b

SHAPE_IDS <- c("square", "triangle", "rectangle")

calc_price <- function(base, qty, slope, floor_p = 0.25) {
  round(max(floor_p, base - slope * qty), 2)
}

# Input ID for team i, shape s
qty_id <- function(i, s) paste0("qty_t", i, "_", s)

APP_CSS <- "
  body { font-family: 'Helvetica Neue', Arial, sans-serif; background: #f7f7f7; }
  h2   { color: #951829; margin: 0; }
  h4   { margin-top: 0; }

  .price-card {
    border-radius: 14px; padding: 16px 10px 12px; text-align: center;
    box-shadow: 0 4px 18px rgba(0,0,0,.13); color: #fff; margin-bottom: 10px;
  }
  .pc-shape { font-size: 12px; font-weight: 600; opacity: .85; margin-bottom: 2px; }
  .pc-price { font-size: 52px; font-weight: 900; line-height: 1.05; margin: 4px 0; }
  .pc-delta { font-size: 11px; opacity: .78; }

  .timer-num {
    font-size: 86px; font-weight: 900; text-align: center;
    letter-spacing: 2px; font-variant-numeric: tabular-nums; line-height: 1;
  }
  .t-go   { color: #166534; }
  .t-warn { color: #92400e; }
  .t-done { color: #951829; animation: blink 1s step-start infinite; }
  @keyframes blink { 50% { opacity: 0; } }

  .round-badge {
    display: inline-block; background: #951829; color: #fff;
    border-radius: 24px; padding: 4px 18px; font-size: 17px; font-weight: 700;
  }
  .earn-big  { font-size: 20px; font-weight: 800; color: #166534; }
  .divider   { border-top: 2px solid #e2e2e2; margin: 10px 0; }

  .leaderboard { width: 100%; border-collapse: collapse; font-size: 15px; }
  .leaderboard th { background: #951829; color: #fff; padding: 6px 10px;
                    text-align: left; }
  .leaderboard td { padding: 6px 10px; border-bottom: 1px solid #e0e0e0; }
  .leaderboard tr:nth-child(even) td { background: #f5f0f1; }
  .leaderboard .rank1 td { font-weight: 700; font-size: 16px; }

  .team-grid th { font-size: 12px; color: #555; padding: 2px 4px; }
  .team-grid td { padding: 2px 4px; vertical-align: middle; }

  .btn-vassar { background: #951829; border-color: #7a1221; color: #fff; font-weight: 600; }
  .btn-vassar:hover, .btn-vassar:focus { background: #7a1221; color: #fff; }
"

TIMER_JS <- "
(function () {
  var _h = null, _s = 0;
  Shiny.addCustomMessageHandler('timerStart', function (s) {
    _s = parseInt(s); clearInterval(_h); tick(); _h = setInterval(tick, 1000);
  });
  Shiny.addCustomMessageHandler('timerStop',  function () { clearInterval(_h); });
  Shiny.addCustomMessageHandler('timerReset', function (m) {
    clearInterval(_h); setDisp(parseInt(m) * 60);
  });
  function tick() {
    _s = Math.max(0, _s - 1); setDisp(_s);
    if (_s <= 0) {
      clearInterval(_h);
      Shiny.setInputValue('timer_done', Date.now(), { priority: 'event' });
    }
  }
  function setDisp(s) {
    var m = Math.floor(s / 60), sec = s % 60;
    var el = document.getElementById('js_timer');
    if (!el) return;
    el.innerText = pad(m) + ':' + pad(sec);
    el.className = 'timer-num ' +
      (s === 0 ? 't-done' : s <= 30 ? 't-warn' : 't-go');
  }
  function pad(n) { return (n < 10 ? '0' : '') + n; }
})();
"

# ---- UI -------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(APP_CSS)), tags$script(HTML(TIMER_JS))),

  fluidRow(
    column(12,
      div(style = "display:flex; align-items:center; gap:14px; padding:12px 0 4px;",
        h2("Sloman Trading Game"),
        uiOutput("round_badge")
      ),
      p(style = "color:#666; font-size:13px; margin-bottom:6px;",
        "More supply → lower price. Enter each team's output and record the round.")
    )
  ),

  fluidRow(

    # ── Col 1: Timer + settings ──────────────────────────────────────────
    column(3,
      wellPanel(
        div(id = "js_timer", class = "timer-num t-go", "05:00"),
        div(class = "divider"),
        numericInput("timer_min", "Minutes per round", 5, 1, 30, 1),
        fluidRow(
          column(4, actionButton("btn_start",   "▶",  class = "btn-success btn-block btn-sm")),
          column(4, actionButton("btn_stop",    "⏹",  class = "btn-warning btn-block btn-sm")),
          column(4, actionButton("btn_reset_t", "↺",  class = "btn-default btn-block btn-sm"))
        ),
        div(class = "divider"),

        h5("Base prices ($/unit)"),
        fluidRow(
          column(4, numericInput("base_sq",   "□ Sq",  3.00, 0, 50, 0.25)),
          column(4, numericInput("base_tri",  "△ Tri", 2.00, 0, 50, 0.25)),
          column(4, numericInput("base_rect", "▭ Rect",4.00, 0, 50, 0.25))
        ),
        sliderInput("slope", "$ drop per unit (sensitivity)",
                    0.05, 1.00, 0.25, 0.05, ticks = FALSE),
        div(class = "divider"),

        h5("Teams"),
        numericInput("n_teams", "Number of teams", 4, 2, 8, 1),
        uiOutput("team_name_inputs")
      )
    ),

    # ── Col 2: Live price cards ──────────────────────────────────────────
    column(4,
      h4("Current Market Prices"),
      div(class = "price-card", style = "background:#951829",
        div(class = "pc-shape", "Square (2×2 in.)"),
        div(class = "pc-price", textOutput("pr_sq", inline = TRUE)),
        div(class = "pc-delta", textOutput("dx_sq", inline = TRUE))
      ),
      div(class = "price-card", style = "background:#2166ac",
        div(class = "pc-shape", "Right Triangle (2 in.)"),
        div(class = "pc-price", textOutput("pr_tri", inline = TRUE)),
        div(class = "pc-delta", textOutput("dx_tri", inline = TRUE))
      ),
      div(class = "price-card", style = "background:#2d6a4f",
        div(class = "pc-shape", "Rectangle (2×4 in.)"),
        div(class = "pc-price", textOutput("pr_rect", inline = TRUE)),
        div(class = "pc-delta", textOutput("dx_rect", inline = TRUE))
      )
    ),

    # ── Col 3: Team quantity grid + record ───────────────────────────────
    column(5,
      wellPanel(
        h4("Items Received This Round"),
        uiOutput("qty_grid"),
        div(class = "divider"),
        div(class = "earn-big",
          "Round total: ", textOutput("total_earn", inline = TRUE)),
        br(),
        actionButton("btn_record", "✓ Record Round",
                     class = "btn-vassar btn-lg btn-block")
      )
    )
  ),

  div(class = "divider"),

  fluidRow(
    column(5,
      h4("Leaderboard — Cumulative Earnings"),
      tableOutput("leaderboard_tbl")
    ),
    column(4,
      h4("Price Trend by Round"),
      plotOutput("plt_trend", height = "260px")
    ),
    column(3,
      h4("Round History"),
      tableOutput("round_summary_tbl")
    )
  ),

  div(style = "text-align:right; padding:8px 4px; color:#999; font-size:12px;",
    actionButton("btn_reset_game", "Reset All",
                 class = "btn-danger btn-xs", style = "margin-right:8px;"),
    "Sloman Trading Game · Vassar ECON 102"
  )
)

# ---- Server ---------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    round   = 1L,
    history = data.frame(
      round  = integer(0), team = character(0),
      sq_n   = integer(0), tri_n = integer(0), rect_n = integer(0),
      sq_p   = numeric(0), tri_p = numeric(0), rect_p = numeric(0),
      earn   = numeric(0),
      stringsAsFactors = FALSE
    )
  )

  # ── Timer ──────────────────────────────────────────────────────────────
  observeEvent(input$btn_start,   session$sendCustomMessage("timerStart",  as.integer(input$timer_min) * 60L))
  observeEvent(input$btn_stop,    session$sendCustomMessage("timerStop",   ""))
  observeEvent(input$btn_reset_t, session$sendCustomMessage("timerReset",  as.integer(input$timer_min)))
  observeEvent(input$timer_done,  showNotification("⏰ Time is up!", type = "warning", duration = 10))

  # ── Dynamic team name inputs ────────────────────────────────────────────
  output$team_name_inputs <- renderUI({
    n <- max(2L, min(8L, as.integer(input$n_teams %||% 4L)))
    lapply(seq_len(n), function(i) {
      textInput(paste0("team_name_", i),
                label = NULL,
                value = input[[paste0("team_name_", i)]] %||% LETTERS[i],
                placeholder = paste("Team", LETTERS[i]))
    })
  })

  team_names <- reactive({
    n <- max(2L, min(8L, as.integer(input$n_teams %||% 4L)))
    sapply(seq_len(n), function(i) {
      nm <- trimws(input[[paste0("team_name_", i)]] %||% "")
      if (nzchar(nm)) nm else LETTERS[i]
    })
  })

  bases <- reactive({
    c(sq   = as.numeric(input$base_sq   %||% 3),
      tri  = as.numeric(input$base_tri  %||% 2),
      rect = as.numeric(input$base_rect %||% 4))
  })

  # ── Dynamic quantity grid ───────────────────────────────────────────────
  output$qty_grid <- renderUI({
    teams  <- team_names()
    colors <- c(sq = "#951829", tri = "#2166ac", rect = "#2d6a4f")
    labels <- c(sq = "□ Square", tri = "△ Triangle", rect = "▭ Rectangle")

    tagList(
      # Header
      tags$table(class = "team-grid", style = "width:100%;",
        tags$thead(
          tags$tr(
            tags$th("Team", style = "width:18%;"),
            tags$th(style = paste0("color:", colors["sq"],   "; width:18%;"), labels["sq"]),
            tags$th(style = paste0("color:", colors["tri"],  "; width:18%;"), labels["tri"]),
            tags$th(style = paste0("color:", colors["rect"], "; width:18%;"), labels["rect"]),
            tags$th(style = "width:28%; text-align:right;", "Earnings")
          )
        ),
        tags$tbody(
          lapply(seq_along(teams), function(i) {
            tags$tr(
              tags$td(tags$b(teams[i])),
              lapply(SHAPE_IDS, function(s) {
                tags$td(numericInput(qty_id(i, s), NULL, 0, min = 0, step = 1,
                                     width = "70px"))
              }),
              tags$td(style = "text-align:right; font-weight:700; font-size:15px;",
                textOutput(paste0("earn_t", i), inline = TRUE))
            )
          })
        )
      )
    )
  })

  # ── Live prices & per-team earnings ────────────────────────────────────
  lp <- reactive({
    slope  <- input$slope %||% 0.25
    n      <- max(2L, min(8L, as.integer(input$n_teams %||% 4L)))
    b      <- bases()

    # Collect quantities: teams × shapes matrix
    qtys <- lapply(seq_len(n), function(i) {
      sapply(SHAPE_IDS, function(s) max(0L, as.integer(input[[qty_id(i, s)]] %||% 0)))
    })

    totals <- Reduce("+", qtys)  # total supply per shape

    prices <- c(
      sq   = calc_price(b["sq"],   totals["square"],    slope),
      tri  = calc_price(b["tri"],  totals["triangle"],  slope),
      rect = calc_price(b["rect"], totals["rectangle"], slope)
    )

    team_earn <- sapply(qtys, function(q) {
      q["square"] * prices["sq"] + q["triangle"] * prices["tri"] + q["rectangle"] * prices["rect"]
    })

    list(n = n, qtys = qtys, totals = totals, prices = prices, team_earn = team_earn)
  })

  # ── Price card outputs ─────────────────────────────────────────────────
  fmt_delta <- function(cur, base) {
    d <- round(cur - base, 2)
    if      (d < 0) sprintf("▼ $%.2f below base ($%.2f)", abs(d), base)
    else if (d > 0) sprintf("▲ $%.2f above base ($%.2f)", d, base)
    else            sprintf("At base price ($%.2f)", base)
  }

  output$pr_sq   <- renderText(sprintf("$%.2f", lp()$prices["sq"]))
  output$pr_tri  <- renderText(sprintf("$%.2f", lp()$prices["tri"]))
  output$pr_rect <- renderText(sprintf("$%.2f", lp()$prices["rect"]))
  output$dx_sq   <- renderText(fmt_delta(lp()$prices["sq"],   bases()["sq"]))
  output$dx_tri  <- renderText(fmt_delta(lp()$prices["tri"],  bases()["tri"]))
  output$dx_rect <- renderText(fmt_delta(lp()$prices["rect"], bases()["rect"]))

  # ── Per-team earnings preview (one output per possible team slot) ──────
  lapply(seq_len(8), function(i) {
    output[[paste0("earn_t", i)]] <- renderText({
      x <- lp()
      if (i > x$n) return("")
      sprintf("$%.2f", x$team_earn[i])
    })
  })

  output$total_earn <- renderText({
    x <- lp()
    sprintf("$%.2f", sum(x$team_earn))
  })

  # ── Round badge ────────────────────────────────────────────────────────
  output$round_badge <- renderUI(div(class = "round-badge",
                                      sprintf("Round %d of 3", rv$round)))

  # ── Record round ───────────────────────────────────────────────────────
  observeEvent(input$btn_record, {
    x      <- lp()
    teams  <- team_names()
    new_rows <- do.call(rbind, lapply(seq_len(x$n), function(i) {
      data.frame(
        round  = rv$round,
        team   = teams[i],
        sq_n   = x$qtys[[i]]["square"],
        tri_n  = x$qtys[[i]]["triangle"],
        rect_n = x$qtys[[i]]["rectangle"],
        sq_p   = x$prices["sq"],
        tri_p  = x$prices["tri"],
        rect_p = x$prices["rect"],
        earn   = x$team_earn[i],
        row.names = NULL, stringsAsFactors = FALSE
      )
    }))
    rv$history <- rbind(rv$history, new_rows)
    rv$round   <- rv$round + 1L

    for (i in seq_len(x$n))
      for (s in SHAPE_IDS)
        updateNumericInput(session, qty_id(i, s), value = 0)

    session$sendCustomMessage("timerStop",  "")
    session$sendCustomMessage("timerReset", as.integer(input$timer_min))

    showNotification(
      sprintf("Round %d recorded — total earnings $%.2f", rv$round - 1L, sum(x$team_earn)),
      type = "message", duration = 5
    )
  })

  # ── Leaderboard ────────────────────────────────────────────────────────
  output$leaderboard_tbl <- renderUI({
    h <- rv$history
    if (!nrow(h)) return(p(style = "color:#999;", "No rounds recorded yet."))

    agg <- aggregate(earn ~ team, data = h, sum)
    agg <- agg[order(agg$earn, decreasing = TRUE), ]
    agg$rank <- seq_len(nrow(agg))
    agg$earn_fmt <- sprintf("$%.2f", agg$earn)

    rows <- lapply(seq_len(nrow(agg)), function(i) {
      row_class <- if (i == 1) "rank1" else ""
      medal <- c("\U1F947", "\U1F948", "\U1F949")[i]
      medal <- if (!is.na(medal) && i <= 3) medal else as.character(i)
      tags$tr(class = row_class,
        tags$td(medal),
        tags$td(agg$team[i]),
        tags$td(style = "text-align:right; font-weight:700;", agg$earn_fmt[i])
      )
    })

    tags$table(class = "leaderboard",
      tags$thead(tags$tr(
        tags$th("#"),
        tags$th("Team"),
        tags$th(style = "text-align:right;", "Earnings")
      )),
      tags$tbody(rows)
    )
  })

  # ── Round summary table ────────────────────────────────────────────────
  output$round_summary_tbl <- renderTable({
    h <- rv$history
    if (!nrow(h)) return(data.frame(Note = "No rounds yet."))
    agg <- aggregate(earn ~ round, data = h, sum)
    data.frame(
      Round    = agg$round,
      Earnings = sprintf("$%.2f", agg$earn)
    )
  }, striped = TRUE, bordered = TRUE)

  # ── Price trend chart ──────────────────────────────────────────────────
  output$plt_trend <- renderPlot({
    h <- rv$history
    if (!nrow(h)) {
      return(ggplot() +
        annotate("text", x=.5, y=.5, label="Record a round to see trends",
                 size=4.5, color="gray60") + theme_void())
    }

    # One price row per round (prices are the same for all teams in a round)
    price_df <- h[!duplicated(h$round), c("round","sq_p","tri_p","rect_p")]
    df <- rbind(
      data.frame(round = price_df$round, price = price_df$sq_p,   item = "Square"),
      data.frame(round = price_df$round, price = price_df$tri_p,  item = "Triangle"),
      data.frame(round = price_df$round, price = price_df$rect_p, item = "Rectangle")
    )
    df$item <- factor(df$item, c("Square","Triangle","Rectangle"))
    b <- bases()
    base_df <- data.frame(
      item  = factor(c("Square","Triangle","Rectangle"), c("Square","Triangle","Rectangle")),
      base  = c(b["sq"], b["tri"], b["rect"])
    )
    pal <- c(Square="#951829", Triangle="#2166ac", Rectangle="#2d6a4f")

    ggplot(df, aes(x=round, y=price, color=item, group=item)) +
      geom_hline(data=base_df, aes(yintercept=base, color=item),
                 linetype="dashed", alpha=.35, linewidth=.9) +
      geom_line(linewidth=1.4) + geom_point(size=4) +
      scale_color_manual(values=pal, name=NULL) +
      scale_x_continuous(breaks=seq_len(max(h$round)), minor_breaks=NULL) +
      scale_y_continuous(labels=dollar_format(), limits=c(0,NA),
                         expand=expansion(mult=c(0,.12))) +
      labs(x="Round", y="Price per unit", caption="Dashed = base price") +
      theme_minimal(base_size=12) +
      theme(legend.position="bottom", panel.grid.minor=element_blank(),
            plot.background=element_rect(fill="transparent", color=NA))
  }, bg="transparent")

  # ── Reset ──────────────────────────────────────────────────────────────
  observeEvent(input$btn_reset_game, {
    rv$round   <- 1L
    rv$history <- data.frame(
      round=integer(0), team=character(0),
      sq_n=integer(0), tri_n=integer(0), rect_n=integer(0),
      sq_p=numeric(0), tri_p=numeric(0), rect_p=numeric(0), earn=numeric(0),
      stringsAsFactors=FALSE
    )
    n <- max(2L, min(8L, as.integer(input$n_teams %||% 4L)))
    for (i in seq_len(n))
      for (s in SHAPE_IDS)
        updateNumericInput(session, qty_id(i, s), value = 0)
    session$sendCustomMessage("timerStop",  "")
    session$sendCustomMessage("timerReset", as.integer(input$timer_min))
    showNotification("Game reset to Round 1.", type="warning", duration=3)
  })
}

shinyApp(ui=ui, server=server)
