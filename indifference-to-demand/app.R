# app.R
# Shiny app: Cobb-Douglas, perfect substitutes, complements, quasilinear (linear demand) consumer choice
# - Left: indifference/budget/optimum for current utility type
# - Right: Marshallian demand for the selected good (can be linear!)
# - Table of (price, quantity) equilibria optional

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# ---- Core math: multi utility types ----

# Cobb–Douglas
cobb_douglas_optimum <- function(income, px, py, alpha) {
  x <- alpha * income / px
  y <- (1 - alpha) * income / py
  utility <- x^alpha * y^(1 - alpha)
  list(x = x, y = y, utility = utility)
}

# Perfect substitutes: U = alpha*x + beta*y
substitutes_optimum <- function(income, px, py, alpha_subs) {
  # alpha_subs > 0, (1-alpha_subs) for y
  a <- alpha_subs
  b <- 1 - alpha_subs
  # Compare per-dollar utility
  uxpx <- a / px
  uypy <- b / py
  if (abs(uxpx - uypy) < 1e-8) {
    # Equal: optimal at any point on the budget line
    x <- income / px * 0.5
    y <- income / py * 0.5
  } else if (uxpx > uypy) {
    x <- income / px
    y <- 0
  } else {
    x <- 0
    y <- income / py
  }
  utility <- a * x + b * y
  list(x = x, y = y, utility = utility)
}

# Perfect complements: U = min(x/a, y/b)
complements_optimum <- function(income, px, py, a_compl=1, b_compl=1) {
  # Ratio: consume in fixed proportion a:b
  x_star <- (income) / (px * a_compl + py * b_compl) * a_compl
  y_star <- (income) / (px * a_compl + py * b_compl) * b_compl
  utility <- min(x_star/a_compl, y_star/b_compl)
  list(x = x_star, y = y_star, utility = utility)
}

# Quasilinear: U = x + alpha*log(y)
quasilinear_optimum <- function(income, px, py, alpha_ql) {
  # First, get y = alpha/py, as long as affordable; rest on x
  y0 <- alpha_ql / py
  spend_y <- py * y0
  if (income >= spend_y && y0 > 0) {
    x <- (income - spend_y) / px
    y <- y0
  } else {
    # Can't afford y0, corner
    y <- income / py
    x <- 0
  }
  utility <- x + alpha_ql * log(y + 1e-12)
  list(x = x, y = y, utility = utility)
}

# Wrapper for all
get_optimum <- function(type, income, px, py, alpha, ...) {
  switch(type,
         "Cobb-Douglas" = cobb_douglas_optimum(income, px, py, alpha),
         "Perfect Substitutes" = substitutes_optimum(income, px, py, alpha),
         "Perfect Complements" = complements_optimum(income, px, py, 1, 1),
         "Quasilinear" = quasilinear_optimum(income, px, py, alpha),
         stop("Unknown utility type")
  )
}

# Plot indifference curves + budget
draw_indiff_and_budget_plot <- function(
  type = "Cobb-Douglas",
  income = 100,
  px = 2,
  py = 4,
  alpha = 0.5,
  good_x = "Good X",
  good_y = "Good Y",
  num_utility_levels = 3,
  utility_level_increment = 10,
  xlim = NULL, ylim = NULL
) {
  opt <- get_optimum(type, income, px, py, alpha)
  # Axis limits guess by model, account for special utility
  if (is.null(xlim)) xlim <- c(0, max(income / px, 1.5 * opt$x) * 1.05)
  if (is.null(ylim)) ylim <- c(0, max(income / py, 1.5 * opt$y) * 1.05)
  xgrid <- seq(0.01, xlim[2], length.out = 600)

  # Indifference curves
  if (type == "Cobb-Douglas") {
    # Usual way as before
    k_low  <- ceiling((num_utility_levels - 1) / 2)
    k_high <- floor((num_utility_levels - 1) / 2)
    levels_low  <- if (k_low  > 0) seq(opt$utility - utility_level_increment * k_low,
                                      opt$utility - utility_level_increment, by = utility_level_increment) else numeric(0)
    levels_high <- if (k_high > 0) seq(opt$utility + utility_level_increment,
                                      opt$utility + utility_level_increment * k_high, by = utility_level_increment) else numeric(0)
    utility_levels <- c(levels_low, opt$utility, levels_high)
    indiff <- bind_rows(lapply(utility_levels, function(U) {
      yvals <- (U / (xgrid^alpha))^(1 / (1 - alpha))
      yvals[!is.finite(yvals)] <- NA_real_
      data.frame(x = xgrid, y = yvals, U = factor(sprintf("%.2f", U)))
    }))
    # label each indifference curve near its max valid x
    labels_df <- indiff %>%
      filter(!is.na(y)) %>%
      group_by(U) %>%
      slice_max(order_by = x, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(label = paste0("U=", U))
  } else if (type == "Perfect Substitutes") {
    # Indifference curves: straight lines x = (U - b*y)/a ↔ a*x + b*y = U
    a <- alpha
    b <- 1 - alpha
    k_low  <- ceiling((num_utility_levels - 1) / 2)
    k_high <- floor((num_utility_levels - 1) / 2)
    levels_low  <- if (k_low  > 0) seq(opt$utility - utility_level_increment * k_low,
                                        opt$utility - utility_level_increment, by = utility_level_increment) else numeric(0)
    levels_high <- if (k_high > 0) seq(opt$utility + utility_level_increment,
                                        opt$utility + utility_level_increment * k_high, by = utility_level_increment) else numeric(0)
    utility_levels <- c(levels_low, opt$utility, levels_high)
    yvals <- lapply(utility_levels, function(U) (U - a*xgrid)/b)
    indiff <- bind_rows(lapply(seq_along(utility_levels), function(i) {
      y <- yvals[[i]]
      y[!is.finite(y)] <- NA_real_
      data.frame(x = xgrid, y = y, U = factor(sprintf("%.2f", utility_levels[i])))
    }))
    labels_df <- indiff %>%
      filter(!is.na(y)) %>%
      group_by(U) %>%
      slice_max(order_by = x, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(label = paste0("U=", U))
  } else if (type == "Perfect Complements") {
    # L-shaped curves: x = y
    k_low  <- ceiling((num_utility_levels - 1) / 2)
    k_high <- floor((num_utility_levels - 1) / 2)
    levels_low  <- if (k_low  > 0) seq(opt$utility - utility_level_increment * k_low,
                                        opt$utility - utility_level_increment, by = utility_level_increment) else numeric(0)
    levels_high <- if (k_high > 0) seq(opt$utility + utility_level_increment,
                                        opt$utility + utility_level_increment * k_high, by = utility_level_increment) else numeric(0)
    utility_levels <- c(levels_low, opt$utility, levels_high)
    indiff <- bind_rows(lapply(utility_levels, function(U) {
      xvals <- pmin(xgrid, U)
      yvals <- U
      # Just vertical then horizontal at the kink
      rbind(
        data.frame(x = xvals, y = yvals, U = factor(sprintf("%.2f", U))),
        data.frame(x = U, y = xgrid[xgrid <= U], U = factor(sprintf("%.2f", U)))
      )
    }))
    labels_df <- data.frame(x = utility_levels, y = utility_levels, U = factor(sprintf("%.2f", utility_levels)), label = paste0("U=", sprintf("%.2f", utility_levels)))
  } else if (type == "Quasilinear") {
    # U = x + alpha*log(y)
    alpha_ql <- alpha
    k_low  <- ceiling((num_utility_levels - 1) / 2)
    k_high <- floor((num_utility_levels - 1) / 2)
    levels_low  <- if (k_low  > 0) seq(opt$utility - utility_level_increment * k_low,
                                        opt$utility - utility_level_increment, by = utility_level_increment) else numeric(0)
    levels_high <- if (k_high > 0) seq(opt$utility + utility_level_increment,
                                        opt$utility + utility_level_increment * k_high, by = utility_level_increment) else numeric(0)
    utility_levels <- c(levels_low, opt$utility, levels_high)
    indiff <- bind_rows(lapply(utility_levels, function(U) {
      # x = U - alpha*log(y), for y > 0
      yvals <- seq(max(0.01, ylim[1]), ylim[2], length.out = 300)
      xvals <- U - alpha_ql * log(yvals)
      xvals[!is.finite(xvals)] <- NA_real_
      data.frame(x = xvals, y = yvals, U = factor(sprintf("%.2f", U)))
    }))
    # label top of each curve
    labels_df <- indiff %>%
      filter(!is.na(x)) %>%
      group_by(U) %>%
      slice_min(order_by = y, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(label = paste0("U=", U))
  } else {
    indiff <- data.frame()
    labels_df <- data.frame()
  }

  # budget constraint (same)
  bc_y <- (income - px * xgrid) / py
  bc_y[bc_y < 0] <- NA_real_
  bc <- data.frame(x = xgrid, y = bc_y)

  p <- ggplot() +
    geom_line(data = indiff, aes(x = x, y = y, group = U), linewidth = 0.8) +
    geom_text(data = labels_df, aes(x = x, y = y, label = label), hjust = -0.05, size = 3) +
    geom_line(data = bc, aes(x = x, y = y), linewidth = 1.1) +
    geom_point(data = data.frame(x = opt$x, y = opt$y), aes(x = x, y = y), size = 3) +
    geom_segment(aes(x = 0, y = opt$y, xend = opt$x, yend = opt$y), linetype = "dashed") +
    geom_segment(aes(x = opt$x, y = 0, xend = opt$x, yend = opt$y), linetype = "dashed") +
    annotate("text",
             x = xlim[2] * 0.60,
             y = (income - px * xlim[2] * 0.60) / py + (ylim[2] * 0.03),
             label = "Budget",
             vjust = 0,
             size = 4) +
    labs(
      x = good_x,
      y = good_y,
      title = paste0(type, ": Utility Maximization"),
      subtitle = "Indifference curves + budget constraint + optimum"
    ) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )

  list(plot = p, opt = opt)
}

# Build demand schedule for selected good by varying its price
build_demand_schedule <- function(
  type = "Cobb-Douglas",
  demand_for = c("x", "y"),
  income,
  px, py,
  alpha,
  price_min,
  price_max,
  n_points
) {
  demand_for <- match.arg(demand_for)
  a <- alpha
  b <- 1 - alpha
  prices <- seq(price_min, price_max, length.out = n_points)
  if (type == "Cobb-Douglas") {
    if (demand_for == "x") {
      qty <- a * income / prices
      data.frame(price = prices, quantity = qty)
    } else {
      qty <- b * income / prices
      data.frame(price = prices, quantity = qty)
    }
  } else if (type == "Perfect Substitutes") {
    # For x: if a/px > b/py, all money to x, linear demand: quantity = income/p
    if (demand_for == "x") {
      u_py <- b / py
      # If x per $ is best, demand is linear (corner)
      # But need to check at every price
      qty <- sapply(prices, function(p) {
        uxpx <- a / p
        if (uxpx > u_py) income / p else 0
      })
      data.frame(price = prices, quantity = qty)
    } else {
      u_px <- a / px
      # Demand for y
      qty <- sapply(prices, function(p) {
        uypy <- b / p
        if (uypy > u_px) income / p else 0
      })
      data.frame(price = prices, quantity = qty)
    }
  } else if (type == "Perfect Complements") {
    # Demand for x: x = income/(px+py) (for a=b=1); y: income/(px+py)
    tot <- prices + if (demand_for == "x") py else px
    qty <- income / tot
    data.frame(price = prices, quantity = qty)
  } else if (type == "Quasilinear") {
    alpha_ql <- alpha
    if (demand_for == "x") {
      # x = (income - spend on y0)/px, where y0 = alpha/py (if affordable), so price here is px
      spend_y <- alpha_ql / py * py # = alpha
      qty <- ifelse(prices * 0 + income >= spend_y & spend_y > 0, (income - spend_y) / prices, 0)
      data.frame(price = prices, quantity = qty)
    } else {
      # y = alpha / price (for py)
      qty <- alpha_ql / prices
      data.frame(price = prices, quantity = qty)
    }
  } else {
    data.frame(price = prices, quantity = NA)
  }
}

# ---- Shiny app ----
ui <- fluidPage(
  titlePanel("Consumer Choice (Cobb-Douglas, Perfect Substitutes, Complements, Quasilinear/Linear Demand)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "utility_type",
        "Utility Type",
        choices = c("Cobb-Douglas", "Perfect Substitutes", "Perfect Complements", "Quasilinear"),
        selected = "Cobb-Douglas"
      ),
      textInput("good_x", "Good X name", value = "Pizza (X)"),
      textInput("good_y", "Good Y name", value = "Movies (Y)"),
      tags$hr(),

      numericInput("income", "Income (m)", value = 100, min = 1, step = 1),
      numericInput("px", "Price of X (pX)", value = 2, min = 0.01, step = 0.1),
      numericInput("py", "Price of Y (pY)", value = 4, min = 0.01, step = 0.1),
      conditionalPanel(
        condition = "['Cobb-Douglas', 'Perfect Substitutes', 'Quasilinear'].includes(input.utility_type)",
        sliderInput("alpha", "Preference parameter (alpha; share/weight on X)", min = 0.01, max = 0.99, value = 0.5, step = 0.01)
      ),
      tags$hr(),

      selectInput(
        "demand_for",
        "Demand curve for which good?",
        choices = c("Good X" = "x", "Good Y" = "y"),
        selected = "x"
      ),
      numericInput("price_min", "Price range: min", value = 0.5, min = 0.01, step = 0.1),
      numericInput("price_max", "Price range: max", value = 10, min = 0.01, step = 0.1),
      sliderInput("n_points", "Number of equilibria to trace", min = 10, max = 200, value = 60, step = 5),

      tags$hr(),
      sliderInput("num_utility_levels", "Indifference curves (count)", min = 1, max = 9, value = 3, step = 1),
      numericInput("utility_increment", "Utility increment", value = 10, min = 0.1, step = 1),

      tags$hr(),
      checkboxInput("show_table", "Show equilibrium schedule table", value = FALSE)
    ),

    mainPanel(
      fluidRow(
        column(6, plotOutput("choice_plot", height = 420)),
        column(6, plotOutput("demand_plot", height = 420))
      ),
      tags$hr(),
      uiOutput("optimum_summary"),
      tags$hr(),
      conditionalPanel(
        condition = "input.show_table === true",
        tableOutput("demand_table")
      ),

      # note
      tags$p("Note: This app supports Cobb-Douglas, perfect substitutes, perfect complements, and quasilinear (linear demand curve) preferences. The demand curve is recalculated for the selected good and utility type. For perfect substitutes and quasilinear, demand can become linear in price.")
    )
  )
)

server <- function(input, output, session) {
  # Defensive checks
  observeEvent(c(input$price_min, input$price_max), {
    if (input$price_max <= input$price_min) {
      showNotification("Price range: max must be > min.", type = "error")
    }
  })

  current_optimum <- reactive({
    utility_type <- input$utility_type
    income <- input$income
    px <- input$px
    py <- input$py
    # For CD/PS/QL: alpha slider; for PC, alpha is ignored
    alpha <- if (!is.null(input$alpha)) input$alpha else 0.5
    # Check params
    validate(
      need(income > 0, "Income must be > 0."),
      need(px > 0, "pX must be > 0."),
      need(py > 0, "pY must be > 0."),
      if (utility_type %in% c("Cobb-Douglas", "Perfect Substitutes", "Quasilinear")) need(alpha > 0 && alpha < 1, "alpha must be in (0,1).") else NULL
    )
    get_optimum(utility_type, income, px, py, alpha)
  })

  output$choice_plot <- renderPlot({
    utility_type <- input$utility_type
    alpha <- if (!is.null(input$alpha)) input$alpha else 0.5
    res <- draw_indiff_and_budget_plot(
      type = utility_type,
      income = input$income,
      px = input$px,
      py = input$py,
      alpha = alpha,
      good_x = input$good_x,
      good_y = input$good_y,
      num_utility_levels = input$num_utility_levels,
      utility_level_increment = input$utility_increment
    )
    res$plot
  })

  demand_schedule <- reactive({
    utility_type <- input$utility_type
    alpha <- if (!is.null(input$alpha)) input$alpha else 0.5
    validate(
      need(input$price_max > input$price_min, "Price range must have max > min."),
      need(input$n_points >= 2, "Need at least 2 points.")
    )
    build_demand_schedule(
      type = utility_type,
      demand_for = input$demand_for,
      income = input$income,
      px = input$px,
      py = input$py,
      alpha = alpha,
      price_min = input$price_min,
      price_max = input$price_max,
      n_points = input$n_points
    )
  })

  output$demand_plot <- renderPlot({
    ds <- demand_schedule()
    opt <- current_optimum()
    good_name <- if (input$demand_for == "x") input$good_x else input$good_y
    current_price <- if (input$demand_for == "x") input$px else input$py
    current_qty   <- if (input$demand_for == "x") opt$x else opt$y

    ggplot(ds, aes(x = price, y = quantity)) +
      geom_line(linewidth = 1.1) +
      geom_point(aes(x = current_price, y = current_qty), size = 3) +
      labs(
        x = paste0("Price of ", good_name),
        y = paste0("Quantity demanded of ", good_name),
        title = paste("Demand curve for", good_name, "(", input$utility_type, ")"),
        subtitle = "Each point = optimum quantity at that price (income, other price, preferences fixed)"
      ) +
      theme_minimal(base_size = 13) +
      scale_x_continuous(labels = label_number(accuracy = 0.01)) +
      scale_y_continuous(labels = label_number(accuracy = 0.01))
  })

  output$optimum_summary <- renderUI({
    opt <- current_optimum()
    HTML(paste0(
      "<b>Current optimum</b><br/>",
      input$good_x, ": <b>", signif(opt$x, 4), "</b><br/>",
      input$good_y, ": <b>", signif(opt$y, 4), "</b><br/>",
      "Utility: <b>", signif(opt$utility, 4), "</b>"
    ))
  })

  output$demand_table <- renderTable({
    ds <- demand_schedule()
    ds %>%
      mutate(
        price = round(price, 4),
        quantity = round(quantity, 4)
      )
  })
}

shinyApp(ui, server)
