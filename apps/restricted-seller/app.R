# app.R — Restricted Seller classroom trading game
# Phase-based workflow: setup > r1_trading > r1_results > r2_setup > r2_trading > r2_results > compare
# Session-only state (no database — 15-minute exercise)

library(shiny)
library(ggplot2)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Build a step-function demand curve data.frame from WTP values
build_demand_steps <- function(wtp_values) {
  wtp_sorted <- sort(wtp_values, decreasing = TRUE)
  data.frame(
    q_start = seq(0, length(wtp_sorted) - 1),
    q_end   = seq(1, length(wtp_sorted)),
    wtp     = wtp_sorted
  )
}

#' Render the market diagram with optional surplus shading
#'
#' @param wtp_values  Numeric vector of buyer WTP values
#' @param mc          Marginal cost (flat)
#' @param trades_df   Data.frame with columns `wtp` and `price` (one row per trade)
#' @param title       Plot title
#' @param show_monopoly If TRUE, shade DWL and profit for a monopoly outcome
#' @param mono_price  Monopoly price (used when show_monopoly = TRUE)
#' @param mono_qty    Monopoly quantity (used when show_monopoly = TRUE)
render_market_plot <- function(wtp_values, mc, trades_df = NULL, title = "",
                               show_monopoly = FALSE, mono_price = NULL,
                               mono_qty = NULL) {
  steps <- build_demand_steps(wtp_values)
  n_wtp <- length(wtp_values)

  p <- ggplot() +
    # Demand step function
    geom_segment(
      data = steps,
      aes(x = q_start, xend = q_end, y = wtp, yend = wtp),
      colour = "black", linewidth = 1.2
    ) +
    # Vertical connectors between steps
    geom_segment(
      data = steps[-nrow(steps), ],
      aes(x = q_end, xend = q_end, y = wtp, yend = c(wtp[-1], NA)),
      colour = "black", linewidth = 1.2
    )

  # MC line
  p <- p +
    geom_hline(yintercept = mc, colour = "#008000", linewidth = 1.2) +
    annotate("text", x = n_wtp - 0.5, y = mc + 0.8,
             label = paste0("MC = ", mc),
             colour = "#008000", fontface = "bold", size = 5)

  # Competitive surplus shading (all units where WTP > MC)
  comp_steps <- steps[steps$wtp > mc, ]
  if (nrow(comp_steps) > 0) {
    p <- p + geom_rect(
      data = comp_steps,
      aes(xmin = q_start, xmax = q_end, ymin = mc, ymax = wtp),
      fill = "#4393c3", alpha = 0.15
    )
  }

  # If we have actual trades, shade traded surplus darker

  if (!is.null(trades_df) && nrow(trades_df) > 0) {
    for (i in seq_len(nrow(trades_df))) {
      wtp_i <- trades_df$wtp[i]
      price_i <- trades_df$price[i]
      # CS for this trade
      if (wtp_i > price_i) {
        p <- p + annotate("rect",
          xmin = i - 1, xmax = i,
          ymin = price_i, ymax = wtp_i,
          fill = "#4393c3", alpha = 0.3
        )
      }
      # PS for this trade
      if (price_i > mc) {
        p <- p + annotate("rect",
          xmin = i - 1, xmax = i,
          ymin = mc, ymax = price_i,
          fill = "#90ee90", alpha = 0.4
        )
      }
    }
  }

  # Monopoly overlays

  if (show_monopoly && !is.null(mono_price) && !is.null(mono_qty)) {
    # Monopoly price line
    p <- p +
      geom_hline(yintercept = mono_price, linetype = "dashed",
                 colour = "#d62728", linewidth = 0.8) +
      annotate("text", x = n_wtp - 0.5, y = mono_price + 0.8,
               label = paste0("Monopoly P = ", mono_price),
               colour = "#d62728", fontface = "bold", size = 4.5)

    # Profit rectangle
    if (mono_price > mc && mono_qty > 0) {
      p <- p + annotate("rect",
        xmin = 0, xmax = mono_qty,
        ymin = mc, ymax = mono_price,
        fill = "#90ee90", alpha = 0.35
      ) +
      annotate("label", x = mono_qty / 2, y = (mono_price + mc) / 2,
               label = paste0("Profit = $", (mono_price - mc) * mono_qty),
               fill = "#e6ffe6", size = 5, fontface = "bold")
    }

    # DWL: trades that would happen competitively but not under monopoly
    dwl_steps <- steps[steps$wtp > mc & steps$wtp < mono_price, ]
    if (nrow(dwl_steps) > 0) {
      p <- p + geom_rect(
        data = dwl_steps,
        aes(xmin = q_start, xmax = q_end, ymin = mc, ymax = wtp),
        fill = "#d62728", alpha = 0.35
      ) +
      annotate("label",
        x = mean(c(dwl_steps$q_start[1], dwl_steps$q_end[nrow(dwl_steps)])),
        y = mean(c(mc, max(dwl_steps$wtp))),
        label = "DWL", fill = "#ffcccc", colour = "#d62728",
        size = 5, fontface = "bold"
      )
    }

    # Monopoly Q line
    p <- p + geom_vline(xintercept = mono_qty, linetype = "dotted",
                        colour = "#d62728", linewidth = 0.8)
  }

  # Competitive Q line
  q_comp <- sum(sort(wtp_values, decreasing = TRUE) > mc)
  p <- p + geom_vline(xintercept = q_comp, linetype = "dotted",
                       colour = "#4393c3", linewidth = 0.8)

  p + scale_x_continuous(breaks = 0:(n_wtp + 1)) +
    scale_y_continuous(breaks = seq(0, max(wtp_values) + 2, 2)) +
    coord_cartesian(
      xlim = c(0, n_wtp + 1),
      ylim = c(0, max(wtp_values) + 4),
      expand = FALSE
    ) +
    labs(title = title, x = "Quantity (units traded)", y = "Price / WTP ($)") +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      panel.grid.minor = element_blank()
    )
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-size: 16px; }
    .btn { font-size: 16px; padding: 10px 24px; margin: 4px; }
    .btn-primary { font-size: 18px; padding: 12px 32px; }
    .btn-danger  { font-size: 18px; padding: 12px 32px; }
    .btn-success { font-size: 18px; padding: 12px 32px; }
    h2, h3 { margin-top: 12px; }
    .well { font-size: 16px; }
    .big-stat { font-size: 28px; font-weight: bold; text-align: center;
                padding: 12px; margin: 8px 0; border-radius: 8px; }
    .stat-blue  { background: #d6eaf8; color: #2166ac; }
    .stat-green { background: #d5f5e3; color: #196f3d; }
    .stat-red   { background: #fadbd8; color: #d62728; }
    table.dataframe { font-size: 16px; }
    .shiny-input-container { margin-bottom: 8px; }
  "))),

  titlePanel("The Restricted Seller"),

  # Phase-based UI: show/hide panels via conditionalPanel
  # ------ SETUP ------
  conditionalPanel(
    condition = "output.phase == 'setup'",
    wellPanel(
      h2("Game Setup"),
      fluidRow(
        column(6,
          textInput("wtp_input", "Buyer WTP values (comma-separated)",
                    value = "20,18,16,14,12,10,8,6"),
          numericInput("mc_input", "Seller Marginal Cost", value = 4, min = 0),
          numericInput("n_sets", "Number of WTP sets (for large classes)", value = 1, min = 1, max = 5)
        ),
        column(6,
          h4("Preview: WTP Values"),
          verbatimTextOutput("wtp_preview"),
          h4("Competitive Benchmark"),
          verbatimTextOutput("comp_preview")
        )
      ),
      actionButton("start_r1", "Start Round 1: Competitive Market",
                    class = "btn-primary btn-lg")
    )
  ),

  # ------ ROUND 1 TRADING ------
  conditionalPanel(
    condition = "output.phase == 'r1_trading'",
    wellPanel(
      h2("Round 1: Competitive Market"),
      fluidRow(
        column(4,
          h3("Record a Trade"),
          selectInput("r1_buyer_wtp", "Buyer WTP", choices = NULL),
          numericInput("r1_price", "Agreed Price", value = NULL, min = 0),
          actionButton("r1_record", "Record Trade", class = "btn-success"),
          hr(),
          actionButton("r1_undo", "Undo Last Trade", class = "btn-warning btn-sm")
        ),
        column(8,
          h3("Trades So Far"),
          tableOutput("r1_trade_table"),
          hr(),
          actionButton("end_r1", "End Round 1", class = "btn-danger btn-lg")
        )
      )
    )
  ),

  # ------ ROUND 1 RESULTS ------
  conditionalPanel(
    condition = "output.phase == 'r1_results'",
    wellPanel(
      h2("Round 1 Results: Competitive Market"),
      fluidRow(
        column(4,
          uiOutput("r1_stats"),
          hr(),
          actionButton("start_r2", "Start Round 2: Monopoly", class = "btn-primary btn-lg")
        ),
        column(8,
          plotOutput("r1_plot", height = "500px")
        )
      )
    )
  ),

  # ------ ROUND 2 SETUP ------
  conditionalPanel(
    condition = "output.phase == 'r2_setup'",
    wellPanel(
      h2("Round 2: Monopoly — Monopolist Decides"),
      fluidRow(
        column(6,
          h3("Monopolist: Set Your Terms"),
          numericInput("mono_price", "Your Price ($)", value = 14, min = 0),
          numericInput("mono_qty", "Units You Will Sell", value = 4, min = 0),
          p(tags$em("Commit before buyers decide.")),
          actionButton("lock_in", "Lock In Price & Quantity", class = "btn-danger btn-lg")
        ),
        column(6,
          h4("Demand Schedule (for reference)"),
          tableOutput("demand_reference")
        )
      )
    )
  ),

  # ------ ROUND 2 TRADING ------
  conditionalPanel(
    condition = "output.phase == 'r2_trading'",
    wellPanel(
      h2("Round 2: Monopoly Trading"),
      fluidRow(
        column(6,
          div(class = "big-stat stat-green",
            textOutput("mono_committed_txt")
          ),
          hr(),
          numericInput("r2_accepted", "How many buyers accepted?", value = 0, min = 0),
          p(tags$em("Count the buyers who agreed to pay the monopolist's price.")),
          actionButton("end_r2", "End Round 2", class = "btn-danger btn-lg")
        ),
        column(6,
          h4("Eligible Buyers (WTP >= committed price)"),
          tableOutput("eligible_buyers")
        )
      )
    )
  ),

  # ------ ROUND 2 RESULTS ------
  conditionalPanel(
    condition = "output.phase == 'r2_results'",
    wellPanel(
      h2("Round 2 Results: Monopoly"),
      fluidRow(
        column(4,
          uiOutput("r2_stats"),
          hr(),
          actionButton("show_compare", "Compare Both Rounds", class = "btn-primary btn-lg")
        ),
        column(8,
          plotOutput("r2_plot", height = "500px")
        )
      )
    )
  ),

  # ------ COMPARE ------
  conditionalPanel(
    condition = "output.phase == 'compare'",
    wellPanel(
      h2("Comparison: Competition vs. Monopoly"),
      fluidRow(
        column(6, plotOutput("compare_r1_plot", height = "420px")),
        column(6, plotOutput("compare_r2_plot", height = "420px"))
      ),
      hr(),
      fluidRow(
        column(6, uiOutput("compare_table")),
        column(6,
          div(class = "big-stat stat-red",
            textOutput("dwl_message")
          ),
          hr(),
          actionButton("reset_game", "Reset Game", class = "btn-warning btn-lg")
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Reactive state ---
  rv <- reactiveValues(
    phase       = "setup",
    wtp_values  = c(20, 18, 16, 14, 12, 10, 8, 6),
    mc          = 4,
    r1_trades   = data.frame(trade = integer(), wtp = numeric(), price = numeric(),
                             stringsAsFactors = FALSE),
    available_wtp = numeric(),
    mono_price  = NA_real_,
    mono_qty    = NA_integer_,
    r2_accepted = 0L
  )

  # Expose phase for conditionalPanel

  output$phase <- reactive(rv$phase)
  outputOptions(output, "phase", suspendWhenHidden = FALSE)

  # --- SETUP phase ---
  parse_wtp <- reactive({
    vals <- suppressWarnings(
      as.numeric(trimws(unlist(strsplit(input$wtp_input, ","))))
    )
    vals <- vals[!is.na(vals) & vals > 0]
    if (length(vals) == 0) vals <- c(20, 18, 16, 14, 12, 10, 8, 6)
    n_sets <- max(1, input$n_sets %||% 1)
    rep(sort(vals, decreasing = TRUE), times = n_sets)
  })

  output$wtp_preview <- renderText({
    paste(parse_wtp(), collapse = ", ")
  })

  output$comp_preview <- renderText({
    wtp <- parse_wtp()
    mc  <- input$mc_input
    q_comp <- sum(wtp > mc)
    max_surplus <- sum(pmax(wtp - mc, 0))
    paste0("Competitive Q = ", q_comp, " trades\n",
           "Maximum total surplus = $", max_surplus)
  })

  # --- Start Round 1 ---
  observeEvent(input$start_r1, {
    rv$wtp_values <- parse_wtp()
    rv$mc <- input$mc_input
    rv$r1_trades <- data.frame(trade = integer(), wtp = numeric(), price = numeric(),
                               stringsAsFactors = FALSE)
    rv$available_wtp <- rv$wtp_values
    updateSelectInput(session, "r1_buyer_wtp",
                      choices = sort(rv$available_wtp, decreasing = TRUE))
    updateNumericInput(session, "r1_price", value = rv$mc)
    rv$phase <- "r1_trading"
  })

  # --- Record R1 trade ---
  observeEvent(input$r1_record, {
    req(input$r1_buyer_wtp, input$r1_price)
    wtp_val <- as.numeric(input$r1_buyer_wtp)
    price_val <- input$r1_price

    new_row <- data.frame(
      trade = nrow(rv$r1_trades) + 1L,
      wtp   = wtp_val,
      price = price_val,
      stringsAsFactors = FALSE
    )
    rv$r1_trades <- rbind(rv$r1_trades, new_row)

    # Remove used WTP from available pool
    idx <- match(wtp_val, rv$available_wtp)
    if (!is.na(idx)) {
      rv$available_wtp <- rv$available_wtp[-idx]
    }
    updateSelectInput(session, "r1_buyer_wtp",
                      choices = sort(rv$available_wtp, decreasing = TRUE))
    updateNumericInput(session, "r1_price", value = rv$mc)
  })

  # --- Undo last R1 trade ---
  observeEvent(input$r1_undo, {
    if (nrow(rv$r1_trades) > 0) {
      last <- rv$r1_trades[nrow(rv$r1_trades), ]
      rv$r1_trades <- rv$r1_trades[-nrow(rv$r1_trades), ]
      rv$available_wtp <- c(rv$available_wtp, last$wtp)
      updateSelectInput(session, "r1_buyer_wtp",
                        choices = sort(rv$available_wtp, decreasing = TRUE))
    }
  })

  output$r1_trade_table <- renderTable({
    if (nrow(rv$r1_trades) == 0) return(data.frame(Message = "No trades yet"))
    rv$r1_trades
  }, digits = 0)

  # --- End Round 1 ---
  observeEvent(input$end_r1, {
    rv$phase <- "r1_results"
  })

  # --- R1 Results ---
  r1_summary <- reactive({
    trades <- rv$r1_trades
    if (nrow(trades) == 0) {
      return(list(n = 0, cs = 0, ps = 0, ts = 0, avg_price = NA))
    }
    cs <- sum(pmax(trades$wtp - trades$price, 0))
    ps <- sum(pmax(trades$price - rv$mc, 0))
    list(
      n = nrow(trades),
      cs = cs,
      ps = ps,
      ts = cs + ps,
      avg_price = round(mean(trades$price), 1)
    )
  })

  output$r1_stats <- renderUI({
    s <- r1_summary()
    max_surplus <- sum(pmax(rv$wtp_values - rv$mc, 0))
    tagList(
      div(class = "big-stat stat-blue", paste0("Trades: ", s$n)),
      div(class = "big-stat stat-blue", paste0("Consumer Surplus: $", s$cs)),
      div(class = "big-stat stat-green", paste0("Producer Surplus: $", s$ps)),
      div(class = "big-stat",
          style = "background:#f0f0f0;",
          paste0("Total Surplus: $", s$ts, " / $", max_surplus)),
      if (!is.na(s$avg_price))
        div(class = "big-stat", style = "background:#fff3cd;",
            paste0("Avg Price: $", s$avg_price))
    )
  })

  output$r1_plot <- renderPlot({
    render_market_plot(
      rv$wtp_values, rv$mc,
      trades_df = rv$r1_trades,
      title = "Round 1: Competitive Market"
    )
  })

  # --- Start Round 2 ---
  observeEvent(input$start_r2, {
    rv$phase <- "r2_setup"
  })

  # --- Demand reference table ---
  output$demand_reference <- renderTable({
    wtp <- sort(rv$wtp_values, decreasing = TRUE)
    qty <- seq_along(wtp)
    total_profit <- (wtp - rv$mc) * qty
    data.frame(
      `Units sold` = qty,
      `Price (= lowest WTP served)` = paste0("$", wtp),
      `Total profit at this price` = paste0("$", total_profit),
      check.names = FALSE
    )
  })

  # --- Lock in monopoly terms ---
  observeEvent(input$lock_in, {
    if (is.null(input$mono_price) || input$mono_price <= 0) {
      showNotification("Price must be positive.", type = "error")
      return()
    }
    if (is.null(input$mono_qty) || input$mono_qty < 1) {
      showNotification("Quantity must be at least 1.", type = "error")
      return()
    }
    rv$mono_price <- input$mono_price
    rv$mono_qty <- input$mono_qty
    # Cap quantity at eligible buyers
    eligible <- sum(rv$wtp_values >= rv$mono_price)
    updateNumericInput(session, "r2_accepted",
                       value = min(rv$mono_qty, eligible),
                       max = eligible)
    rv$phase <- "r2_trading"
  })

  output$mono_committed_txt <- renderText({
    paste0("Monopolist committed: P = $", rv$mono_price,
           " | Q = ", rv$mono_qty, " units")
  })

  output$eligible_buyers <- renderTable({
    wtp <- sort(rv$wtp_values, decreasing = TRUE)
    eligible <- wtp[wtp >= rv$mono_price]
    if (length(eligible) == 0) {
      return(data.frame(Message = "No buyers willing to pay this price"))
    }
    data.frame(
      Buyer = seq_along(eligible),
      WTP = paste0("$", eligible),
      `Surplus if they buy` = paste0("$", eligible - rv$mono_price),
      check.names = FALSE
    )
  })

  # --- End Round 2 ---
  observeEvent(input$end_r2, {
    rv$r2_accepted <- min(input$r2_accepted, rv$mono_qty)
    rv$phase <- "r2_results"
  })

  # --- R2 Results ---
  r2_summary <- reactive({
    p <- rv$mono_price
    q <- rv$r2_accepted
    mc <- rv$mc

    # Buyers who traded: top q WTP values >= p
    wtp_sorted <- sort(rv$wtp_values, decreasing = TRUE)
    buyers <- wtp_sorted[wtp_sorted >= p]
    traded_wtp <- buyers[seq_len(min(q, length(buyers)))]

    cs <- sum(pmax(traded_wtp - p, 0))
    ps <- (p - mc) * length(traded_wtp)
    ts <- cs + ps

    # DWL: surplus from units that would trade competitively but didn't
    # Use count-based logic (not setdiff) to handle duplicate WTP values
    comp_wtp <- wtp_sorted[wtp_sorted > mc]
    n_comp <- length(comp_wtp)
    n_traded <- length(traded_wtp)
    n_missed <- max(0, n_comp - n_traded)
    if (n_missed > 0) {
      missed_wtp <- comp_wtp[(n_traded + 1):n_comp]
      dwl <- sum(pmax(missed_wtp - mc, 0))
    } else {
      dwl <- 0
    }

    list(n = length(traded_wtp), cs = cs, ps = ps, ts = ts, dwl = dwl)
  })

  output$r2_stats <- renderUI({
    s <- r2_summary()
    tagList(
      div(class = "big-stat stat-blue", paste0("Trades: ", s$n)),
      div(class = "big-stat stat-blue", paste0("Consumer Surplus: $", s$cs)),
      div(class = "big-stat stat-green", paste0("Producer Surplus: $", s$ps)),
      div(class = "big-stat", style = "background:#f0f0f0;",
          paste0("Total Surplus: $", s$ts)),
      div(class = "big-stat stat-red", paste0("Deadweight Loss: $", s$dwl))
    )
  })

  output$r2_plot <- renderPlot({
    render_market_plot(
      rv$wtp_values, rv$mc,
      title = "Round 2: Monopoly Outcome",
      show_monopoly = TRUE,
      mono_price = rv$mono_price,
      mono_qty = rv$r2_accepted
    )
  })

  # --- Compare ---
  observeEvent(input$show_compare, {
    rv$phase <- "compare"
  })

  output$compare_r1_plot <- renderPlot({
    render_market_plot(
      rv$wtp_values, rv$mc,
      trades_df = rv$r1_trades,
      title = "Round 1: Competition"
    )
  })

  output$compare_r2_plot <- renderPlot({
    render_market_plot(
      rv$wtp_values, rv$mc,
      title = "Round 2: Monopoly",
      show_monopoly = TRUE,
      mono_price = rv$mono_price,
      mono_qty = rv$r2_accepted
    )
  })

  output$compare_table <- renderUI({
    s1 <- r1_summary()
    s2 <- r2_summary()
    tags$table(
      class = "table table-bordered",
      style = "font-size: 18px;",
      tags$thead(tags$tr(
        tags$th(""), tags$th("Competition"), tags$th("Monopoly"), tags$th("Difference")
      )),
      tags$tbody(
        tags$tr(tags$td("Trades"), tags$td(s1$n), tags$td(s2$n),
                tags$td(s2$n - s1$n)),
        tags$tr(tags$td("Consumer Surplus"), tags$td(paste0("$", s1$cs)),
                tags$td(paste0("$", s2$cs)),
                tags$td(paste0("$", s2$cs - s1$cs))),
        tags$tr(tags$td("Producer Surplus"), tags$td(paste0("$", s1$ps)),
                tags$td(paste0("$", s2$ps)),
                tags$td(paste0("$", s2$ps - s1$ps))),
        tags$tr(tags$td(tags$strong("Total Surplus")),
                tags$td(tags$strong(paste0("$", s1$ts))),
                tags$td(tags$strong(paste0("$", s2$ts))),
                tags$td(tags$strong(paste0("$", s2$ts - s1$ts)))),
        tags$tr(style = "background:#fadbd8;",
                tags$td(tags$strong("Deadweight Loss")),
                tags$td("$0"), tags$td(paste0("$", s2$dwl)),
                tags$td(paste0("$", s2$dwl)))
      )
    )
  })

  output$dwl_message <- renderText({
    s2 <- r2_summary()
    paste0("$", s2$dwl, " in surplus disappeared. Nobody got it. That's deadweight loss.")
  })

  # --- Reset ---
  observeEvent(input$reset_game, {
    rv$phase <- "setup"
    rv$r1_trades <- data.frame(trade = integer(), wtp = numeric(), price = numeric(),
                               stringsAsFactors = FALSE)
    rv$available_wtp <- numeric()
    rv$mono_price <- NA_real_
    rv$mono_qty <- NA_integer_
    rv$r2_accepted <- 0L
  })
}

shinyApp(ui, server)
