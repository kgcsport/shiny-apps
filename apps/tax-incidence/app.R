library(shiny)
library(ggplot2)

# ---- UI ---------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Tax Incidence Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Market Parameters"),
      sliderInput("b", "Demand slope (steeper = more inelastic)",
                  min = 0.1, max = 5, value = 1, step = 0.1),
      sliderInput("d", "Supply slope (steeper = more inelastic)",
                  min = 0.1, max = 5, value = 1, step = 0.1),
      sliderInput("tax", "Tax per unit ($)",
                  min = 0, max = 10, value = 4, step = 0.5),
      hr(),
      helpText("Equilibrium is fixed at Q*=6, P*=8 as you move the sliders."),
      hr(),
      h4("Results"),
      uiOutput("results_ui")
    ),
    mainPanel(
      width = 9,
      plotOutput("incidence_plot", height = "500px")
    )
  )
)

# ---- Server -----------------------------------------------------------
server <- function(input, output, session) {

  calc <- reactive({
    b   <- input$b
    d   <- input$d
    tax <- input$tax

    # Equilibrium fixed at Q*=6, P*=8
    # Demand: P = a - b*Q  where a = 8 + 6b
    # Supply: P = c + d*Q  where c = 8 - 6d
    a <- 8 + 6 * b
    c <- 8 - 6 * d

    q_star <- 6
    p_star <- 8

    # With tax t on buyers:
    # Q_t = (a - c - tax) / (b + d)
    q_t <- max(0, (a - c - tax) / (b + d))
    p_s <- c + d * q_t
    p_b <- p_s + tax

    buyer_burden  <- tax * b / (b + d)
    seller_burden <- tax * d / (b + d)
    buyer_pct     <- round(100 * b / (b + d))
    seller_pct    <- 100 - buyer_pct

    revenue <- tax * q_t
    dwl     <- 0.5 * tax * (q_star - q_t)

    list(
      a = a, b = b, c = c, d = d, tax = tax,
      q_star = q_star, p_star = p_star,
      q_t = q_t, p_s = p_s, p_b = p_b,
      buyer_burden = buyer_burden, seller_burden = seller_burden,
      buyer_pct = buyer_pct, seller_pct = seller_pct,
      revenue = revenue, dwl = dwl
    )
  })

  output$results_ui <- renderUI({
    v <- calc()
    tagList(
      tags$b("Equilibrium (no tax):"),
      tags$p(sprintf("Q* = %.2f,  P* = $%.2f", v$q_star, v$p_star)),
      tags$b("After tax:"),
      tags$p(sprintf("Q_tax = %.2f", v$q_t)),
      tags$p(sprintf("P_buyer  = $%.2f  (\u2191$%.2f)", v$p_b, v$buyer_burden)),
      tags$p(sprintf("P_seller = $%.2f  (\u2193$%.2f)", v$p_s, v$seller_burden)),
      tags$hr(),
      tags$b("Burden shares:"),
      tags$p(style = "color:#2166ac; font-weight:bold;",
             sprintf("Buyers  bear: %d%%", v$buyer_pct)),
      tags$p(style = "color:#1a9641; font-weight:bold;",
             sprintf("Sellers bear: %d%%", v$seller_pct)),
      tags$hr(),
      tags$b("Revenue:"),  tags$p(sprintf("$%.2f", v$revenue)),
      tags$b("DWL:"),      tags$p(sprintf("$%.2f", v$dwl))
    )
  })

  output$incidence_plot <- renderPlot({
    v <- calc()

    q_rng <- seq(0, 12, length.out = 300)

    # Demand curve (clip where P < 0)
    q_d_max  <- min(12, v$a / v$b)
    q_d      <- seq(0, q_d_max, length.out = 300)

    # Supply curve (clip where P < 0 when c < 0)
    q_s_min  <- max(0, -v$c / v$d)
    q_s      <- seq(q_s_min, 12, length.out = 300)

    # Welfare polygons
    cs_p  <- data.frame(x = c(0, 0, v$q_t),             y = c(v$a,  v$p_b, v$p_b))
    ps_p  <- data.frame(x = c(0, 0, v$q_t),             y = c(v$c,  v$p_s, v$p_s))
    rev_p <- data.frame(x = c(0, 0, v$q_t, v$q_t),      y = c(v$p_s, v$p_b, v$p_b, v$p_s))
    dwl_p <- data.frame(x = c(v$q_t, v$q_t, v$q_star),  y = c(v$p_s, v$p_b, v$p_star))

    ylim_top <- max(18, v$a * 0.7)

    plt <- ggplot() +
      geom_polygon(data = cs_p,  aes(x = x, y = y), fill = "#2166ac", alpha = 0.30) +
      geom_polygon(data = ps_p,  aes(x = x, y = y), fill = "#1a9641", alpha = 0.30) +
      geom_polygon(data = rev_p, aes(x = x, y = y), fill = "#f0c050", alpha = 0.60) +
      geom_polygon(data = dwl_p, aes(x = x, y = y), fill = "#d73027", alpha = 0.70) +
      geom_line(data = data.frame(q = q_d, p = v$a - v$b * q_d),
                aes(x = q, y = p), color = "#2166ac", linewidth = 1.3) +
      geom_line(data = data.frame(q = q_s, p = v$c + v$d * q_s),
                aes(x = q, y = p), color = "#1a9641", linewidth = 1.3) +
      geom_hline(yintercept = v$p_star, linetype = "dashed", color = "grey55") +
      geom_hline(yintercept = v$p_b,    linetype = "dashed", color = "grey35") +
      geom_hline(yintercept = v$p_s,    linetype = "dashed", color = "grey35") +
      geom_vline(xintercept = v$q_star, linetype = "dashed", color = "grey55") +
      geom_vline(xintercept = v$q_t,    linetype = "dashed", color = "grey35") +
      # Tax wedge arrow
      annotate("segment",
               x = v$q_t + 0.2, xend = v$q_t + 0.2, y = v$p_s, yend = v$p_b,
               arrow = arrow(ends = "both", length = unit(0.2, "cm")),
               color = "black", linewidth = 0.9) +
      annotate("text", x = v$q_t + 0.45, y = (v$p_s + v$p_b) / 2,
               label = paste0("$", v$tax), hjust = 0, size = 4) +
      # Curve labels
      annotate("text", x = 11.2, y = pmin(v$a - v$b * 11.2 + 0.8, ylim_top * 0.95),
               label = "Demand", color = "#2166ac", fontface = "bold", size = 4.5) +
      annotate("text", x = 11.2, y = pmax(v$c + v$d * 11.2 - 0.8, 0.8),
               label = "Supply", color = "#1a9641", fontface = "bold", size = 4.5) +
      # Price labels
      annotate("text", x = 0.2, y = v$p_b + 0.5,
               label = sprintf("P_buyer = $%.2f  (\u2191%.0f%%)", v$p_b, v$buyer_pct),
               hjust = 0, size = 3.8, color = "#2166ac", fontface = "bold") +
      annotate("text", x = 0.2, y = v$p_s - 0.7,
               label = sprintf("P_seller = $%.2f  (\u2193%.0f%%)", v$p_s, v$seller_pct),
               hjust = 0, size = 3.8, color = "#1a9641", fontface = "bold") +
      annotate("text", x = v$q_star + 0.15, y = 0.6,
               label = sprintf("Q* = %.1f", v$q_star), hjust = 0, size = 3.5, color = "grey50") +
      annotate("text", x = v$q_t + 0.15, y = 0.6,
               label = sprintf("Q_t = %.2f", v$q_t), hjust = 0, size = 3.5, color = "grey30") +
      # Welfare area labels (only when large enough)
      { if (v$q_t > 0.5)
          annotate("label", x = v$q_t / 2, y = (v$p_s + v$p_b) / 2,
                   label = sprintf("Revenue\n$%.1f", v$revenue),
                   fill = "#f0c050", color = "black", size = 3.5, fontface = "bold")
        else list() } +
      { if (v$q_t < v$q_star - 0.3)
          annotate("label",
                   x = (2 * v$q_t + v$q_star) / 3,
                   y = (v$p_s + v$p_b + v$p_star) / 3,
                   label = sprintf("DWL\n$%.1f", v$dwl),
                   fill = "#d73027", color = "white", size = 3.5, fontface = "bold")
        else list() } +
      coord_cartesian(xlim = c(0, 12), ylim = c(0, ylim_top), expand = FALSE) +
      labs(x = "Quantity", y = "Price ($)",
           title = sprintf(
             "Tax Incidence: demand slope=%.1f, supply slope=%.1f, tax=$%.1f",
             v$b, v$d, v$tax
           )) +
      theme_bw(base_size = 14) +
      theme(plot.title = element_text(size = 13))

    plt
  })
}

shinyApp(ui, server)
