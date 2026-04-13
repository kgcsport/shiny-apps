try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
# app.R
library(shiny)
library(DT)
library(ggplot2)

init <- data.frame(
  firm    = c("A","B","C"),
  workers = c(1, 1, 1),
  output  = c(NA, NA, NA),
  stringsAsFactors = FALSE
)

VASSAR_CSS <- "
  body { font-size: 16px; }
  .btn-primary       { background-color: #951829; border-color: #7a1221; }
  .btn-primary:hover { background-color: #7a1221; border-color: #5e0d19; }
  .btn-success       { background-color: #2d6a4f; border-color: #245c43; }
  .btn-success:hover { background-color: #245c43; border-color: #1b4d38; }
  a { color: #951829; } a:hover { color: #7a1221; }
  .shiny-notification-panel { top:16px; bottom:auto; right:16px; min-width:340px; width:auto; }
  .shiny-notification { font-size:1.1rem; padding:1rem 1.25rem; border-radius:8px; border:none;
    box-shadow:0 4px 20px rgba(0,0,0,.25); color:#fff; margin-bottom:8px; }
  .shiny-notification-message { background:#2d6a4f; }
  .shiny-notification-warning { background:#92400e; }
  .shiny-notification-error   { background:#951829; }
  .shiny-notification-close   { color:rgba(255,255,255,.8); font-size:1.2rem; }
"
ui <- fluidPage(
  tags$head(tags$style(HTML(VASSAR_CSS))),
  tags$h2("Airplane experiment: live entry"),
  tags$p("Edit the table during class. The plot updates automatically."),

  fluidRow(
    column(
      7,
      DTOutput("tbl"),
      br(),
      actionButton("add", "Add row"),
      actionButton("reset", "Reset")
    ),
    column(
      5,
      plotOutput("plot", height = 420)
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveVal(init)

  output$tbl <- renderDT({
    datatable(
      rv(),
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = NULL)),
      options = list(dom = "tip", pageLength = 25)
    )
  })

  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit
    df <- rv()
    i <- info$row
    j <- info$col + 1  # DT is 0-indexed; +1 because rownames=FALSE
    v <- info$value

    # coerce numeric columns
    if (names(df)[j] %in% c("workers","output")) v <- suppressWarnings(as.numeric(v))
    df[i, j] <- v
    rv(df)
  })

  observeEvent(input$add, {
    df <- rv()
    df <- rbind(df, data.frame(firm = "", workers = NA, output = NA))
    rv(df)
  })

  observeEvent(input$reset, {
    rv(init)
  })

  output$plot <- renderPlot({
    df <- rv()
    df <- df[!is.na(df$workers) & !is.na(df$output) & df$firm != "", ]
    ggplot(df, aes(x = workers, y = output, color = firm, label = firm)) +
      geom_point(size = 3) +
      labs(x = "Workers", y = "Planes Produced") +
      theme_minimal(base_size = 16)
  })
}

shinyApp(ui, server)
