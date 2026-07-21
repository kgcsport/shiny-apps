try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
library(shiny)

ui <- fluidPage(
  tags$head(
    tags$title("Classroom Economy"),
    tags$meta(`http-equiv` = "refresh", content = "0; url=/arcade/"),
    tags$style(HTML("
      body { font-family: system-ui, -apple-system, sans-serif;
             background: #f4f5f7; display: flex; align-items: center;
             justify-content: center; min-height: 100vh; margin: 0; }
      .redir-box { text-align: center; color: #555; }
      .redir-box a { color: #951829; font-weight: 600; }
    "))
  ),
  div(class = "redir-box",
    tags$p("Redirecting to the Classroom Economy app…"),
    tags$p(tags$a(href = "/arcade/", "Click here if you are not redirected automatically."))
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
