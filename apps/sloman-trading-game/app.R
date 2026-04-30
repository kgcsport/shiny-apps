library(shiny)
library(ggplot2)
library(scales)

`%||%` <- function(a, b) if (!is.null(a)) a else b

SHAPE_IDS <- c("square", "triangle", "rectangle")
MAX_TEAMS <- 8L

calc_price <- function(base, qty, slope, floor_p = 0.25) {
  round(max(floor_p, base - slope * qty), 2)
}

qty_id <- function(i, s) paste0("qty_t", i, "_", s)

# One static row for team i — numericInputs never recreated, so values persist
team_row <- function(i) {
  fluidRow(
    column(2, tags$b(textOutput(paste0("lbl_t", i), inline = TRUE))),
    column(2, numericInput(qty_id(i, "square"),    NULL, 0, 0, step = 1, width = "68px")),
    column(2, numericInput(qty_id(i, "triangle"),  NULL, 0, 0, step = 1, width = "68px")),
    column(2, numericInput(qty_id(i, "rectangle"), NULL, 0, 0, step = 1, width = "68px")),
    column(4, div(style = "text-align:right; font-weight:700; padding-top:6px; font-size:15px;",
                  textOutput(paste0("earn_t", i), inline = TRUE)))
  )
}

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
  .last-earn { font-size: 13px; color: #555; margin-top: 2px; }
  .divider   { border-top: 2px solid #e2e2e2; margin: 10px 0; }
  .grid-hdr  { font-size: 12px; font-weight: 700; padding-bottom: 2px; }

  .leaderboard { width: 100%; border-collapse: collapse; font-size: 15px; }
  .leaderboard th { background: #951829; color: #fff; padding: 6px 10px; text-align: left; }
  .leaderboard td { padding: 6px 10px; border-bottom: 1px solid #e0e0e0; }
  .leaderboard tr:nth-child(even) td { background: #f5f0f1; }
  .leaderboard .rank1 td { font-weight: 700; font-size: 16px; }

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
        "More supply → lower price. Enter each team's output, then record the round.")
    )
  ),

  fluidRow(

    # ── Col 1: Timer + settings ────────────────────────────────────────────
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
          column(4, numericInput("base_sq",   "□ Sq",   3.00, 0, 50, 0.25)),
          column(4, numericInput("base_tri",  "△ Tri",  2.00, 0, 50, 0.25)),
          column(4, numericInput("base_rect", "▭ Rect", 4.00, 0, 50, 0.25))
        ),
        sliderInput("slope", "$ drop per unit (sensitivity)",
                    0.05, 1.00, 0.25, 0.05, ticks = FALSE),
        div(class = "divider"),

        h5("Teams"),
        numericInput("n_teams", "Number of teams", 4, 2, MAX_TEAMS, 1),
        # All 8 name inputs are always present in the DOM — conditionalPanel
        # shows/hides them so they're never destroyed (avoids input ID churn).
        textInput("team_name_1", NULL, "A"),
        textInput("team_name_2", NULL, "B"),
        conditionalPanel("input.n_teams >= 3", textInput("team_name_3", NULL, "C")),
        conditionalPanel("input.n_teams >= 4", textInput("team_name_4", NULL, "D")),
        conditionalPanel("input.n_teams >= 5", textInput("team_name_5", NULL, "E")),
        conditionalPanel("input.n_teams >= 6", textInput("team_name_6", NULL, "F")),
        conditionalPanel("input.n_teams >= 7", textInput("team_name_7", NULL, "G")),
        conditionalPanel("input.n_teams >= 8", textInput("team_name_8", NULL, "H"))
      )
    ),

    # ── Col 2: Price cards ─────────────────────────────────────────────────
    column(4,
      h4("Current Market Prices"),
      p(style = "font-size:12px; color:#666; margin-top:-6px;",
        "Updates after each round is recorded."),
      div(class = "price-card", style = "background:#951829",
        div(class = "pc-shape", "Square (2×2 in.)"),
        div(class = "pc-price", textOutput("pr_sq",  inline = TRUE)),
        div(class = "pc-delta", textOutput("dx_sq",  inline = TRUE))
      ),
      div(class = "price-card", style = "background:#2166ac",
        div(class = "pc-shape", "Right Triangle (2 in.)"),
        div(class = "pc-price", textOutput("pr_tri",  inline = TRUE)),
        div(class = "pc-delta", textOutput("dx_tri",  inline = TRUE))
      ),
      div(class = "price-card", style = "background:#2d6a4f",
        div(class = "pc-shape", "Rectangle (2×4 in.)"),
        div(class = "pc-price", textOutput("pr_rect", inline = TRUE)),
        div(class = "pc-delta", textOutput("dx_rect", inline = TRUE))
      )
    ),

    # ── Col 3: Static team qty grid ───────────────────────────────────────
    column(5,
      wellPanel(
        h4("Items Received This Round"),

        # Header row
        fluidRow(
          column(2, div(class = "grid-hdr", "Team")),
          column(2, div(class = "grid-hdr", style = "color:#951829", "□ Sq")),
          column(2, div(class = "grid-hdr", style = "color:#2166ac", "△ Tri")),
          column(2, div(class = "grid-hdr", style = "color:#2d6a4f", "▭ Rect")),
          column(4, div(class = "grid-hdr", style = "text-align:right", "Earnings"))
        ),

        # Rows 1-2 always visible (minimum n_teams = 2)
        team_row(1),
        team_row(2),
        # Rows 3-8 shown/hidden via CSS only — inputs are never destroyed
        conditionalPanel("input.n_teams >= 3", team_row(3)),
        conditionalPanel("input.n_teams >= 4", team_row(4)),
        conditionalPanel("input.n_teams >= 5", team_row(5)),
        conditionalPanel("input.n_teams >= 6", team_row(6)),
        conditionalPanel("input.n_teams >= 7", team_row(7)),
        conditionalPanel("input.n_teams >= 8", team_row(8)),

        div(class = "divider"),
        div(class = "earn-big",
          "Round total: ", textOutput("total_earn", inline = TRUE)),
        div(class = "last-earn", textOutput("last_earn_msg", inline = TRUE)),
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
      uiOutput("leaderboard_tbl")
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
    round      = 1L,
    last_total = NA_real_,
    history    = data.frame(
      round  = integer(0), team  = character(0),
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

  # ── Helpers ─────────────────────────────────────────────────────────────
  team_names <- reactive({
    n <- max(2L, min(MAX_TEAMS, as.integer(input$n_teams %||% 4L)))
    sapply(seq_len(n), function(i) {
      nm <- trimws(input[[paste0("team_name_", i)]] %||% "")
      if (nzchar(nm)) nm else LETTERS[i]
    })
  })

  bases <- reactive({
    list(
      sq   = as.numeric(input$base_sq   %||% 3),
      tri  = as.numeric(input$base_tri  %||% 2),
      rect = as.numeric(input$base_rect %||% 4)
    )
  })

  # ── Standing prices: only updates when a round is RECORDED (or settings change)
  # Does NOT read qty inputs, so typing quantities never moves the price cards.
  standing_prices <- reactive({
    b     <- bases()
    slope <- as.numeric(input$slope %||% 0.25)
    h     <- rv$history
    list(
      sq   = calc_price(b$sq,   if (nrow(h)) sum(h$sq_n)   else 0L, slope),
      tri  = calc_price(b$tri,  if (nrow(h)) sum(h$tri_n)  else 0L, slope),
      rect = calc_price(b$rect, if (nrow(h)) sum(h$rect_n) else 0L, slope)
    )
  })

  # ── Team name labels in the grid ────────────────────────────────────────
  # textOutput — updates without touching the numericInputs
  for (i in seq_len(MAX_TEAMS)) {
    local({
      li <- i
      output[[paste0("lbl_t", li)]] <- renderText({
        n  <- max(2L, min(MAX_TEAMS, as.integer(input$n_teams %||% 4L)))
        if (li > n) return("")
        nm <- trimws(input[[paste0("team_name_", li)]] %||% "")
        if (nzchar(nm)) nm else LETTERS[li]
      })
    })
  }

  # ── Price card outputs (depend on standing_prices, NOT qty inputs) ───────
  fmt_delta <- function(cur, base) {
    d <- round(cur - base, 2)
    if      (d < 0) sprintf("▼ $%.2f below base ($%.2f)", abs(d), base)
    else if (d > 0) sprintf("▲ $%.2f above base ($%.2f)", d, base)
    else            sprintf("At base price ($%.2f)", base)
  }

  output$pr_sq   <- renderText(sprintf("$%.2f", standing_prices()$sq))
  output$pr_tri  <- renderText(sprintf("$%.2f", standing_prices()$tri))
  output$pr_rect <- renderText(sprintf("$%.2f", standing_prices()$rect))
  output$dx_sq   <- renderText(fmt_delta(standing_prices()$sq,   bases()$sq))
  output$dx_tri  <- renderText(fmt_delta(standing_prices()$tri,  bases()$tri))
  output$dx_rect <- renderText(fmt_delta(standing_prices()$rect, bases()$rect))

  # ── Per-team earnings preview: live qty × standing prices ───────────────
  # Uses standing prices so the per-team number is meaningful (matches what
  # they'd actually get paid), but standing prices don't move as you type.
  get_qty <- function(i, s) max(0L, as.integer(input[[qty_id(i, s)]] %||% 0L))

  for (i in seq_len(MAX_TEAMS)) {
    local({
      li <- i
      output[[paste0("earn_t", li)]] <- renderText({
        n  <- max(2L, min(MAX_TEAMS, as.integer(input$n_teams %||% 4L)))
        if (li > n) return("")
        pr <- standing_prices()
        sq   <- get_qty(li, "square")
        tri  <- get_qty(li, "triangle")
        rect <- get_qty(li, "rectangle")
        sprintf("$%.2f", sq * pr$sq + tri * pr$tri + rect * pr$rect)
      })
    })
  }

  output$total_earn <- renderText({
    n  <- max(2L, min(MAX_TEAMS, as.integer(input$n_teams %||% 4L)))
    pr <- standing_prices()
    total <- sum(sapply(seq_len(n), function(i) {
      get_qty(i, "square") * pr$sq +
      get_qty(i, "triangle") * pr$tri +
      get_qty(i, "rectangle") * pr$rect
    }))
    sprintf("$%.2f", total)
  })

  output$last_earn_msg <- renderText({
    if (is.na(rv$last_total)) return("")
    sprintf("Last round recorded: $%.2f", rv$last_total)
  })

  # ── Round badge ──────────────────────────────────────────────────────────
  output$round_badge <- renderUI(
    div(class = "round-badge", sprintf("Round %d of 3", rv$round))
  )

  # ── Record round ──────────────────────────────────────────────────────────
  observeEvent(input$btn_record, {
    n      <- max(2L, min(MAX_TEAMS, as.integer(isolate(input$n_teams) %||% 4L)))
    pr     <- isolate(standing_prices())   # prices locked in at moment of recording
    teams  <- isolate(team_names())

    rows <- lapply(seq_len(n), function(i) {
      # Read qty inputs with isolate to avoid reactive dependency side-effects
      sq   <- max(0L, as.integer(isolate(input[[qty_id(i, "square")]])    %||% 0L))
      tri  <- max(0L, as.integer(isolate(input[[qty_id(i, "triangle")]])  %||% 0L))
      rect <- max(0L, as.integer(isolate(input[[qty_id(i, "rectangle")]]) %||% 0L))
      earn <- sq * pr$sq + tri * pr$tri + rect * pr$rect

      # unname() prevents named-vector corruption when rbinding data.frames
      data.frame(
        round  = unname(rv$round),
        team   = unname(teams[i]),
        sq_n   = unname(sq),
        tri_n  = unname(tri),
        rect_n = unname(rect),
        sq_p   = unname(pr$sq),
        tri_p  = unname(pr$tri),
        rect_p = unname(pr$rect),
        earn   = unname(earn),
        stringsAsFactors = FALSE
      )
    })

    new_df <- do.call(rbind, rows)
    rv$last_total <- sum(new_df$earn)
    rv$history    <- rbind(rv$history, new_df)
    rv$round      <- rv$round + 1L

    for (i in seq_len(n))
      for (s in SHAPE_IDS)
        updateNumericInput(session, qty_id(i, s), value = 0)

    session$sendCustomMessage("timerStop",  "")
    session$sendCustomMessage("timerReset", as.integer(input$timer_min))

    showNotification(
      sprintf("Round %d recorded — total $%.2f", rv$round - 1L, rv$last_total),
      type = "message", duration = 5
    )
  })

  # ── Leaderboard ─────────────────────────────────────────────────────────
  output$leaderboard_tbl <- renderUI({
    h <- rv$history
    if (!nrow(h)) return(p(style = "color:#999;", "No rounds recorded yet."))

    agg  <- aggregate(earn ~ team, data = h, FUN = sum)
    agg  <- agg[order(agg$earn, decreasing = TRUE), ]
    medals <- c("\U1F947", "\U1F948", "\U1F949")

    rows <- lapply(seq_len(nrow(agg)), function(i) {
      tags$tr(class = if (i == 1) "rank1" else "",
        tags$td(if (i <= 3) medals[i] else as.character(i)),
        tags$td(agg$team[i]),
        tags$td(style = "text-align:right; font-weight:700;",
                sprintf("$%.2f", agg$earn[i]))
      )
    })

    tags$table(class = "leaderboard",
      tags$thead(tags$tr(
        tags$th("#"), tags$th("Team"),
        tags$th(style = "text-align:right;", "Earnings")
      )),
      tags$tbody(rows)
    )
  })

  # ── Round summary ────────────────────────────────────────────────────────
  output$round_summary_tbl <- renderTable({
    h <- rv$history
    if (!nrow(h)) return(data.frame(Note = "No rounds yet."))
    agg <- aggregate(earn ~ round, data = h, FUN = sum)
    data.frame(Round = agg$round, Earnings = sprintf("$%.2f", agg$earn))
  }, striped = TRUE, bordered = TRUE)

  # ── Price trend chart ─────────────────────────────────────────────────────
  output$plt_trend <- renderPlot({
    h <- rv$history
    if (!nrow(h)) {
      return(ggplot() +
        annotate("text", x = .5, y = .5, label = "Record a round to see trends",
                 size = 4.5, color = "gray60") + theme_void())
    }

    price_df <- h[!duplicated(h$round), c("round", "sq_p", "tri_p", "rect_p")]
    df <- rbind(
      data.frame(round = price_df$round, price = price_df$sq_p,   item = "Square"),
      data.frame(round = price_df$round, price = price_df$tri_p,  item = "Triangle"),
      data.frame(round = price_df$round, price = price_df$rect_p, item = "Rectangle")
    )
    df$item <- factor(df$item, c("Square", "Triangle", "Rectangle"))

    b <- bases()
    base_df <- data.frame(
      item = factor(c("Square", "Triangle", "Rectangle"),
                    c("Square", "Triangle", "Rectangle")),
      base = c(b$sq, b$tri, b$rect)
    )
    pal <- c(Square = "#951829", Triangle = "#2166ac", Rectangle = "#2d6a4f")

    ggplot(df, aes(x = round, y = price, color = item, group = item)) +
      geom_hline(data = base_df, aes(yintercept = base, color = item),
                 linetype = "dashed", alpha = .35, linewidth = .9) +
      geom_line(linewidth = 1.4) + geom_point(size = 4) +
      scale_color_manual(values = pal, name = NULL) +
      scale_x_continuous(breaks = seq_len(max(h$round)), minor_breaks = NULL) +
      scale_y_continuous(labels = dollar_format(), limits = c(0, NA),
                         expand = expansion(mult = c(0, .12))) +
      labs(x = "Round", y = "Price per unit", caption = "Dashed = base price") +
      theme_minimal(base_size = 12) +
      theme(legend.position  = "bottom",
            panel.grid.minor = element_blank(),
            plot.background  = element_rect(fill = "transparent", color = NA))
  }, bg = "transparent")

  # ── Reset ────────────────────────────────────────────────────────────────
  observeEvent(input$btn_reset_game, {
    rv$round      <- 1L
    rv$last_total <- NA_real_
    rv$history    <- data.frame(
      round = integer(0), team = character(0),
      sq_n  = integer(0), tri_n = integer(0), rect_n = integer(0),
      sq_p  = numeric(0), tri_p = numeric(0), rect_p = numeric(0),
      earn  = numeric(0), stringsAsFactors = FALSE
    )
    n <- max(2L, min(MAX_TEAMS, as.integer(input$n_teams %||% 4L)))
    for (i in seq_len(n))
      for (s in SHAPE_IDS)
        updateNumericInput(session, qty_id(i, s), value = 0)
    session$sendCustomMessage("timerStop",  "")
    session$sendCustomMessage("timerReset", as.integer(input$timer_min))
    showNotification("Game reset to Round 1.", type = "warning", duration = 3)
  })
}

shinyApp(ui = ui, server = server)
