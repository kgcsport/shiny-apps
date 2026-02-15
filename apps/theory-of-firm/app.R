# app.R
# Shiny app: Demand slope (flat -> steep), Nike-swoosh MC, solve MR=MC,
# show (i) curves and (ii) revenue/cost overlap boxes (profit/loss leftover)

library(shiny)
library(ggplot2)
library(scales)

# ---- helper functions ----

# Inverse demand: P(Q) = A - B Q
P_demand <- function(Q, A, B) A + B * Q

# Marginal revenue under linear inverse demand
MR <- function(Q, A, B) A + 2 * B * Q

# Nike-swoosh style MC: cubic polynomial MC(Q) = a + b Q + c Q^2 + d Q^3
MC <- function(Q, a, b, c, d) pmax(0, a + b*Q + c*Q^2 + d*Q^3)

# Total cost from integrating MC + fixed cost F:
# TC(Q) = F + a Q + b Q^2/2 + c Q^3/3 + d Q^4/4
TC <- function(Q, F, a, b, c, d) {
  F + a*Q + b*Q^2/2 + c*Q^3/3 + d*Q^4/4
}

VC <- function(Q, F, a, b, c, d) {
    TC(Q, F, a, b, c, d)-F
}

ATC <- function(Q, F, a, b, c, d) {
  # vectorized: for Q == 0, return NA_real_ (or Inf if you prefer)
  out <- TC(Q, F, a, b, c, d) / Q
  out[Q == 0] <- NA_real_
  out
}

AVC <- function(Q, F, a, b, c, d) {
    VC(Q, F, a, b, c, d)/Q
}
AFC <- function(Q, F, a, b, c, d) {
    ATC(Q, F, a, b, c, d) - AVC(Q, F, a, b, c, d)
}

# Solve MR=MC on [0, Qmax]. Returns NA if no sign change / no solution.
solve_q_star <- function(A, B, F, a, b, c, d, Qmax) {
  f <- function(Q) MR(Q, A, B) - MC(Q, a, b, c, d)

  # Evaluate grid to find a bracket automatically
  grid <- seq(0, Qmax, length.out = 400)
  vals <- f(grid)

  # If everything is NA or no finite values
  if (all(!is.finite(vals))) return(NA_real_)

  # Find sign changes
  s <- sign(vals)
  idx <- which(diff(s) != 0 & is.finite(vals[-1]) & is.finite(vals[-length(vals)]))

  if (length(idx) == 0) {
    # If MR-MC never crosses 0, pick boundary that maximizes profit as fallback
    # (still "best effort" if MR=MC doesn't exist on interval)
    Qcand <- grid
    Pcand <- P_demand(Qcand, A, B)
    TR    <- Pcand * Qcand
    TCv   <- TC(Qcand, F, a, b, c, d)
    pi    <- TR - TCv
    return(Qcand[which.max(pi)])
  }

  lo <- grid[idx[1]]
  hi <- grid[idx[1] + 1]

  out <- tryCatch(
    uniroot(f, lower = lo, upper = hi)$root,
    error = function(e) NA_real_
  )
  out
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Firm profits with adjustable demand and costs"),
  sidebarLayout(
    sidebarPanel(
      h4("Demand (inverse): P(Q) = A - B Q"),
      sliderInput("A", "A (intercept)", min = 1, max = 200, value = 80, step = 1),

      # Log slider for slope B: near 0 (flat) to very steep
      sliderInput("B", "Demand slope: flat to steep",
                  min = -10, max = -.01, value = -2, step = 0.1),

      hr(), 
      h4("Costs: MC(Q) = a + b*Q + c*Q^2 + d*Q^3"),
      numericInput("F", "Fixed cost F", value = 0, step = 1),
      numericInput("mc_a", "a (level)",  value = 40, step = 1),
      numericInput("mc_b", "b (linear; negative helps 'dip')", value = -2, step = 0.1),
      numericInput("mc_c", "c (quadratic; positive makes rise)", value = .02, step = 0.01),
      numericInput("mc_d", "d (cubic; shapes tail)", value = .0005, step = 0.005),

      hr(),
      numericInput("Qplot", "Plot Q range", value = 100, step = 5),
      numericInput("miny", "Min Y range", value = -100, step = 1),
      numericInput("maxy", "Max Y range", value = 100, step = 1),
      hr(),
      checkboxInput("show_ATC", "Show ATC on curve plot (optional)", value = TRUE)
    ),

    mainPanel(
      fluidRow(
        column(12, plotOutput("plot_curves", height = "420px"))
      ),
      fluidRow(
        column(12, plotOutput("plot_boxes", height = "360px"))
      ),
      fluidRow(
        column(12, verbatimTextOutput("eq_text"))
      ),
      fluidRow(
        column(
          12,
          tags$details(
            tags$summary("Notes / how to use in class"),
            tags$ul(
              tags$li("Move demand slope toward 'flat' (log10 B very negative) to approximate perfect competition."),
              tags$li("Make demand steeper to approximate more inelastic demand; monopoly MR drops faster."),
              tags$li("Profit box uses ATC(Q*) as the cost height (standard micro diagram convention).")
            )
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  params <- reactive({
    list(
      A = input$A,
      B = input$B,
      F = input$F,
      a = input$mc_a,
      b = input$mc_b,
      c = input$mc_c,
      d = input$mc_d,
      Qplot = input$Qplot,
      miny = input$miny,
      maxy = input$maxy
    )
  })

  eq <- reactive({
    p <- params()
    Qstar <- solve_q_star(p$A, p$B, p$F, p$a, p$b, p$c, p$d, p$Qplot)
    maxQ <- -p$A/p$B
    if (Qstar < 0) Qstar <- 0
    else if (Qstar > maxQ) Qstar <- maxQ

    Pstar <- P_demand(Qstar, p$A, p$B)
    TR    <- Pstar * Qstar
    TCv   <- TC(Qstar, p$F, p$a, p$b, p$c, p$d)
    pi    <- TR - TCv

    ATCstar <- if (Qstar > 0) TCv / Qstar else NA_real_

    list(Q = Qstar, P = Pstar, TR = TR, TC = TCv, profit = pi, ATC = ATCstar)
  })

  output$eq_text <- renderPrint({
    p <- params()
    e <- eq()

    cat("Parameters\n")
    cat(sprintf("  Demand: P(Q) = A - BQ  with  A = %.2f,  B = %.6f (log10 B = %.2f)\n",
                p$A, p$B, input$logB))
    cat(sprintf("  Costs:  Fixed F = %.2f;  MC(Q)= a + bQ + cQ^2 + dQ^3 with a=%.2f, b=%.2f, c=%.3f, d=%.3f\n\n",
                p$F, p$a, p$b, p$c, p$d))

    cat("Solution (profit-maximizing via MR=MC when feasible)\n")
    cat(sprintf("  Q* = %.3f\n", e$Q))
    cat(sprintf("  P* = %.3f\n", e$P))
    cat(sprintf("  TR = %.2f\n", e$TR))
    cat(sprintf("  TC = %.2f\n", e$TC))
    cat(sprintf("  Profit = TR - TC = %.2f\n", e$profit))
    if (is.finite(e$ATC)) cat(sprintf("  ATC(Q*) = %.3f\n", e$ATC))
  })

  output$plot_curves <- renderPlot({
    p <- params()
    e <- eq()

    Q <- seq(0, p$Qplot, length.out = 400)
    df <- data.frame(
      Q = Q,
      Demand = P_demand(Q, p$A, p$B),
      MR = MR(Q, p$A, p$B),
      MC = MC(Q, p$a, p$b, p$c, p$d),
      ATC = ATC(Q, p$F, p$a, p$b, p$c, p$d),
      AVC = AVC(Q, p$F, p$a, p$b, p$c, p$d)
    )
    df$AFC <- df$ATC - df$AVC

    # Reshape to long format (base R)
    if (isTRUE(input$show_ATC)) {
      curve_cols <- c("Demand", "MR", "MC", "ATC", "AVC", "AFC")
    } else {
      curve_cols <- c("Demand", "MR", "MC")
    }
    lines <- reshape(df[, c("Q", curve_cols)],
                     direction = "long",
                     varying = curve_cols,
                     v.names = "y",
                     timevar = "curve",
                     times = curve_cols,
                     idvar = "Q_id")
    lines$curve <- factor(lines$curve, levels = curve_cols)
    if (p$F == 0) lines <- lines[lines$curve != "AFC", ]

    # allow for flexibility in y-axis range
    if (is.na(p$miny)) p$miny <- min(lines$y, na.rm = TRUE)
    if (is.na(p$maxy)) p$maxy <- max(lines$y, na.rm = TRUE)

    g <- ggplot(lines, aes(Q, y, linetype = curve, color = curve)) +
      geom_line(linewidth = 1) +
      labs(
        x = "Quantity Q",
        y = "Price / Cost",
        linetype = NULL,
        title = "Demand, MR",
        color = NULL
      ) +
      geom_hline(yintercept = 0, linewidth = 0.6) +
      coord_cartesian(ylim = c(p$miny, p$maxy)) +
      theme_minimal(base_size = 13)

    # Mark optimum
    g <- g +
      geom_vline(xintercept = e$Q, linewidth = 0.6, linetype = "dotted") +
      geom_hline(yintercept = e$P, linewidth = 0.6, linetype = "dotted") +
      annotate("point", x = e$Q, y = MC(e$Q, p$a, p$b, p$c, p$d), size = 3) +
      annotate("text", x = e$Q, y = e$P, label = sprintf(" (Q*, P*) = (%.2f, %.2f)", e$Q, e$P),
               hjust = -0.05, vjust = -0.5)

    g
  })

  output$plot_boxes <- renderPlot({
    p <- params()
    e <- eq()

    Qs <- e$Q
    Ps <- e$P

    # Use ATC(Q*) as the height for the total cost rectangle (standard diagram convention)
    ATCstar <- if (Qs > 0) e$TC / Qs else 0
    profit_per_unit <- Ps - ATCstar

    # Build a simple rectangle diagram in "quantity x dollars-per-unit" space
    # Revenue box: height Ps, width Qs
    # Cost box:    height ATC*, width Qs
    # Profit/loss: leftover (positive or negative)
    df_rect <- data.frame(
      xmin = c(0, 0),
      xmax = c(Qs, Qs),
      ymin = c(0, 0),
      ymax = c(Ps, ATCstar),
      box  = factor(c("Total Revenue (PxQ*)", "Total Cost (ATCxQ*)"),
                    levels = c("Total Revenue (PxQ*)", "Total Cost (ATCxQ*)"))
    )

    # Profit or loss annotation
    label <- if (profit_per_unit >= 0) {
      sprintf("Profit = (P* - ATC*)Q* = %.2f", e$profit)
    } else {
      sprintf("Loss = (ATC* - P*)Q* = %.2f", abs(e$profit))
    }

    ggplot() +
      geom_rect(data = df_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = box),
                alpha = 0.35, color = "black") +
      geom_segment(aes(x = Qs, xend = Qs, y = 0, yend = max(Ps, ATCstar)),
                   linewidth = 0.6) +
      annotate("text", x = Qs/2, y = Ps/2, label = "TR", size = 6) +
      annotate("text", x = Qs/2, y = ATCstar/2, label = "TC", size = 6) +
      annotate("text", x = Qs/2, y = max(Ps, ATCstar) * 1.03, label = label, vjust = 0, size = 4.3) +
      scale_fill_discrete(name = NULL) +
      labs(
        x = "Quantity Q*",
        y = "Dollars per unit (height)",
        title = "Overlapping rectangles: Revenue vs Cost (profit/loss is what's left)"
      ) +
      theme_minimal(base_size = 13) +
      coord_cartesian(xlim = c(0, max(1, Qs) * 1.15),
                      ylim = c(0, max(1, Ps, ATCstar) * 1.15))
  })
}

shinyApp(ui, server)
