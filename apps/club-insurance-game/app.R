library(shiny)
library(dplyr)
library(tibble)

# Set the admin password here (simple for demo; in production, secure this better)
ADMIN_PASSWORD <- Sys.getenv("SHINY_PASSWORD")

ui <- fluidPage(
  titlePanel("Religion as Insurance -- Class Simulation (Pooled)"),

  # -- How to play (collapsible) -----------------------------------------------
  tags$details(
    tags$summary(tags$b("How to play (click to expand)")),
    tags$div(
      style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",

      tags$h5("Setup"),
      tags$ol(
        tags$li("The instructor runs this app locally or on a server (e.g. shiny.kylecoombs.com)."),
        tags$li("Students open the same URL in their browser. Each browser tab gets its own session."),
        tags$li("There are no separate tabs -- everyone sees both panels. ",
                "Students use the left panel; only the instructor should touch the right panel.")
      ),

      tags$h5("Instructor workflow (Admin Panel, right side)"),
      tags$ol(
        tags$li("Set the round parameters (shock probability, costs, transfers, etc.)."),
        tags$li("Click ", tags$b("Start / Open Round"), " to let students submit choices."),
        tags$li("Wait for students to submit, then click ", tags$b("Resolve Round"),
                " to draw shocks and update balances."),
        tags$li("Repeat. Toggle ", tags$b("Government insurance"), " or ",
                tags$b("Strict mode"), " across rounds to demonstrate different regimes."),
        tags$li("Use ", tags$b("Download full data (CSV)"), " to get all choices and outcomes for discussion.")
      ),

      tags$h5("Student workflow (Student Panel, left side)"),
      tags$ol(
        tags$li("Enter a name or nickname and wait for the instructor to open a round."),
        tags$li("Check or uncheck ", tags$b("Join the church group this round"), "."),
        tags$li("Click ", tags$b("Submit choice"), ". Your status box below will update after the round resolves."),
        tags$li("Watch your balance change across rounds. Think about when joining is worth the cost.")
      ),

      tags$h5("Running the app"),
      tags$pre(
        style = "background: #e9ecef; padding: 8px;",
        "# From the R console (not inside app.R!):\n",
        "shiny::runApp('shiny-apps/apps/religious-insurance-game', host = '0.0.0.0', port = 3838)"
      )
    )
  ),

  fluidRow(
    column(
      width = 5,
      wellPanel(
        h4("Student Panel"),
        textInput("display_name", "Your name (or nickname)", value = ""),
        uiOutput("round_status"),
        checkboxInput("join_church", "Join the church group this round", value = FALSE),
        actionButton("submit_choice", "Submit choice", class = "btn-primary"),
        tags$hr(),
        h4("Your status"),
        verbatimTextOutput("my_status")
      )
    ),
    column(
      width = 7,
      wellPanel(
        h4("Admin Panel (Instructor)"),
        uiOutput("admin_panel_ui")
      )
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(
    round_open = FALSE,
    round = 0L,
    roster = tibble(token = character(), name = character()),
    balances = tibble(token = character(), name = character(), balance = numeric()),
    choices = tibble(round = integer(), token = character(), name = character(), join = logical(), submitted_at = character()),
    outcomes = tibble(round = integer(), token = character(), name = character(), shocked = logical(),
                      church_cost = numeric(), church_tax = numeric(), church_transfer = numeric(),
                      gov_transfer = numeric(), net_change = numeric(), end_balance = numeric(),
                      resolved_at = character()),
    admin_authed = FALSE
  )

  ensure_student <- function(token, name) {
    if (is.null(name) || !nzchar(trimws(name))) return(FALSE)
    name <- trimws(name)

    if (!(token %in% rv$roster$token)) {
      final_name <- name
      if (final_name %in% rv$roster$name) {
        final_name <- paste0(final_name, "-", substr(token, 1, 4))
      }
      rv$roster <- bind_rows(rv$roster, tibble(token = token, name = final_name))
      rv$balances <- bind_rows(rv$balances, tibble(token = token, name = final_name, balance = input$initial_income))
    }
    TRUE
  }

  output$round_status <- renderUI({
    if (rv$round_open) {
      tags$div(tags$b(paste0("Round ", rv$round, " is OPEN.")),
               tags$p("Submit your choice for this round."))
    } else {
      tags$div(tags$b("No round open right now."),
               tags$p("Wait for the instructor to open the next round."))
    }
  })

  observeEvent(input$submit_choice, {
    token <- session$token
    ok <- ensure_student(token, input$display_name)
    if (!ok) return()

    if (!rv$round_open) {
      showNotification("Round is not open yet. Wait for the instructor.", type = "error")
      return()
    }

    rv$choices <- rv$choices %>%
      filter(!(round == rv$round & token == token)) %>%
      bind_rows(tibble(
        round = rv$round,
        token = token,
        name = rv$balances$name[match(token, rv$balances$token)],
        join = isTRUE(input$join_church),
        submitted_at = as.character(Sys.time())
      ))

    showNotification("Choice submitted.", type = "message")
  })

  output$my_status <- renderText({
    token <- session$token
    bal_row <- rv$balances %>% filter(token == !!token)
    if (nrow(bal_row) == 0) {
      return("Enter a name above to join the game.\nThen submit your choice when the round is open.")
    }

    my_name <- bal_row$name[[1]]
    my_bal  <- bal_row$balance[[1]]

    last_outcome <- rv$outcomes %>% filter(token == !!token) %>% arrange(desc(round)) %>% head(1)

    txt <- paste0("Name: ", my_name, "\nBalance: ", my_bal)

    if (nrow(last_outcome) == 1) {
      txt <- paste0(
        txt,
        "\n\nLast resolved round: ", last_outcome$round,
        "\nShocked: ", last_outcome$shocked,
        "\nNet change: ", round(last_outcome$net_change, 2),
        "\nEnd balance: ", round(last_outcome$end_balance, 2)
      )
    }
    txt
  })

  observeEvent(input$start_round, {
    if (!rv$round_open) {
      rv$round <- rv$round + 1L
      rv$round_open <- TRUE
      rv$choices <- rv$choices %>% filter(round != rv$round)
      showNotification(paste0("Round ", rv$round, " opened."), type = "message")
    } else {
      showNotification("Round already open.", type = "warning")
    }
  })

  observeEvent(input$resolve_round, {
    if (!rv$round_open) {
      showNotification("No round is open.", type = "error")
      return()
    }

    if (nrow(rv$balances) == 0) {
      showNotification("No students registered yet.", type = "error")
      return()
    }

    base <- rv$balances %>% select(token, name, balance)
    ch <- rv$choices %>% filter(round == rv$round) %>% select(token, join)
    base <- base %>% left_join(ch, by = "token") %>% mutate(join = ifelse(is.na(join), FALSE, join))

    shocked <- runif(nrow(base)) < input$shock_prob
    base$shocked <- shocked

    church_cost <- input$church_cost + ifelse(isTRUE(input$strict_mode), input$strict_extra_cost, 0)
    church_transfer <- input$church_transfer
    gov_transfer <- ifelse(isTRUE(input$gov_on), input$gov_transfer, 0)

    members <- base %>% filter(join)
    n_members <- nrow(members)
    n_shocked_members <- sum(members$shocked)

    total_church_transfers <- n_shocked_members * church_transfer
    church_tax <- if (n_members > 0) total_church_transfers / n_members else 0

    base <- base %>%
      mutate(
        loss = ifelse(shocked, -input$shock_loss, 0),
        pay_church_cost = ifelse(join, -church_cost, 0),
        pay_church_tax  = ifelse(join, -church_tax, 0),
        recv_church     = ifelse(join & shocked, church_transfer, 0),
        recv_gov        = ifelse(shocked, gov_transfer, 0),
        net_change      = loss + pay_church_cost + pay_church_tax + recv_church + recv_gov,
        end_balance     = balance + net_change
      )

    rv$balances <- rv$balances %>%
      select(token, name) %>%
      left_join(base %>% select(token, end_balance), by = "token") %>%
      mutate(balance = end_balance) %>%
      select(token, name, balance)

    rv$outcomes <- bind_rows(
      rv$outcomes,
      base %>% transmute(
        round = rv$round,
        token, name,
        shocked = shocked,
        church_cost = ifelse(join, church_cost, 0),
        church_tax = ifelse(join, church_tax, 0),
        church_transfer = ifelse(join & shocked, church_transfer, 0),
        gov_transfer = ifelse(shocked, gov_transfer, 0),
        net_change,
        end_balance,
        resolved_at = as.character(Sys.time())
      )
    )

    rv$round_open <- FALSE
    showNotification(paste0("Round ", rv$round, " resolved."), type = "message")
  })

  observeEvent(input$reset_game, {
    rv$round_open <- FALSE
    rv$round <- 0L
    rv$roster <- rv$roster[0,]
    rv$balances <- rv$balances[0,]
    rv$choices <- rv$choices[0,]
    rv$outcomes <- rv$outcomes[0,]
    showNotification("Game reset.", type = "message")
  })

  output$summary_table <- renderTable({
    if (nrow(rv$outcomes) == 0) {
      return(tibble(
        round = integer(),
        participants = integer(),
        members = integer(),
        shocked = integer(),
        avg_end_balance = numeric()
      ))
    }
    rv$outcomes %>%
      group_by(round) %>%
      summarise(
        participants = n(),
        members = sum(church_cost > 0),
        shocked = sum(shocked),
        avg_end_balance = round(mean(end_balance), 2),
        .groups = "drop"
      )
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("religion_insurance_sim_", Sys.Date(), ".csv")
    },
    content = function(file) {
      choices_clean <- rv$choices %>%
        arrange(round, name) %>%
        select(round, name, join, submitted_at)

      outcomes_clean <- rv$outcomes %>%
        arrange(round, name) %>%
        select(round, name, shocked, church_cost, church_tax, church_transfer, gov_transfer, net_change, end_balance, resolved_at)

      full <- full_join(choices_clean, outcomes_clean, by = c("round", "name")) %>%
        arrange(round, name)

      write.csv(full, file, row.names = FALSE)
    }
  )

  # ------------------ Admin Panel UI with Password Gate ----------------------
  output$admin_panel_ui <- renderUI({
    if (!isTRUE(rv$admin_authed)) {
      tagList(
        tags$p("Instructor login required:"),
        passwordInput("admin_pass", "Admin password"),
        actionButton("admin_login", "Login")
      )
    } else {
      tagList(
        fluidRow(
          column(4, numericInput("initial_income", "Initial tokens (starting balance)", 10, min = 0)),
          column(4, numericInput("shock_prob", "Shock probability", 0.30, min = 0, max = 1, step = 0.05)),
          column(4, numericInput("shock_loss", "Loss if shocked", 6, min = 0))
        ),
        fluidRow(
          column(4, numericInput("church_cost", "Church cost to join", 2, min = 0)),
          column(4, numericInput("church_transfer", "Church transfer if shocked", 4, min = 0)),
          column(4, checkboxInput("gov_on", "Government insurance on?", value = FALSE))
        ),
        fluidRow(
          column(4, numericInput("gov_transfer", "Gov transfer if shocked", 4, min = 0)),
          column(4, checkboxInput("strict_mode", "Strict mode? (higher cost)", value = FALSE)),
          column(4, numericInput("strict_extra_cost", "Extra cost in strict mode", 1, min = 0))
        ),
        fluidRow(
          column(4, actionButton("start_round", "Start / Open Round", class = "btn-success")),
          column(4, actionButton("resolve_round", "Resolve Round (draw shocks)", class = "btn-warning")),
          column(4, actionButton("reset_game", "Reset Game", class = "btn-danger"))
        ),
        tags$hr(),
        h4("Live class summary"),
        tableOutput("summary_table"),
        tags$hr(),
        downloadButton("download_data", "Download full data (CSV)"),
        tags$hr(),
        actionButton("admin_logout", "Logout", class = "btn-secondary")
      )
    }
  })

  observeEvent(input$admin_login, {
    req(input$admin_pass)
    if (identical(input$admin_pass, ADMIN_PASSWORD)) {
      rv$admin_authed <- TRUE
      showNotification("Admin access granted.", type = "message")
    } else {
      showNotification("Incorrect password.", type = "error")
    }
  })

  observeEvent(input$admin_logout, {
    rv$admin_authed <- FALSE
    # Optionally clear password input. This only works for new UI re-render.
    updateTextInput(session, "admin_pass", value = "")
    showNotification("Admin access revoked.", type = "message")
  })

}

shinyApp(ui, server)
