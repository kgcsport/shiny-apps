if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, ggplot2, tidyverse)

# ReactiveValues for app-level storage of responses
responses <- shiny::reactiveValues(data = data.frame(
  timestamp = as.POSIXct(character()),
  name = character(),
  answer = numeric(),
  stringsAsFactors = FALSE
))

ui <- fluidPage(
  h2("Points Pot"),
  p("Contribute points to the class pot. You can contribute up to 5 points. Each of you will receive 1.5 times the average number of points you all contribute."),
  textInput("name", "Your name"),
  sliderInput("answer", "Points to contribute (0–5)", 0, 5, 0, 1),
  actionButton("submit", "Submit"),
  hr(),
  actionButton("show_plot", "Show plot", class = "btn btn-primary"),
  conditionalPanel(
    condition = "output.plot_available",
    plotOutput("plot")
  ),
  hr(),
  # Simple admin authentication section
  passwordInput("admin_pass", "Admin password:"),
  actionButton("admin_auth", "Log in as admin"),
  uiOutput("admin_download_ui")
)

server <- function(input, output, session) {
  observeEvent(input$submit, {
    req(trimws(input$name) != "")
    new_entry <- data.frame(
      timestamp = Sys.time(),
      name = trimws(input$name),
      answer = input$answer,
      stringsAsFactors = FALSE
    )
    responses$data <- rbind(responses$data, new_entry)
    showNotification("Thanks!", type = "message")
  })
  
  output$plot <- renderPlot({
    req(input$show_plot)
    req(nrow(responses$data) > 0)
    ggplot(responses$data, aes(x = answer)) +
      geom_bar(fill = "#0073C2FF") +
      labs(
        title = "Distribution of Points Contributed",
        x = "Points",
        y = "Count"
      ) +
      scale_x_continuous(breaks = 0:5, limits = c(0, 5))
  })
  
  output$plot_available <- reactive({
    input$show_plot && nrow(responses$data) > 0
  })
  outputOptions(output, "plot_available", suspendWhenHidden = FALSE)
  
  # Simple admin login status
  admin_logged_in <- reactiveVal(FALSE)
  
  observeEvent(input$admin_auth, {
    passphrase <- Sys.getenv("SHINY_PASSWORD")
    if (!is.null(input$admin_pass) && identical(input$admin_pass, passphrase)) {
      admin_logged_in(TRUE)
      showNotification("Admin logged in. Download enabled.", type = "message")
    } else {
      admin_logged_in(FALSE)
      showNotification("Incorrect password.", type = "error")
    }
  })
  
  output$admin_download_ui <- renderUI({
    if (admin_logged_in()) {
      downloadButton("download", "Download responses as CSV", class = "btn btn-primary")
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("bonus_entry_responses_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(responses$data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
