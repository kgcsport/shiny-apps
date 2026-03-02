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

ui <- fluidPage(
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
