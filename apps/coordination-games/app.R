# Legacy route only. Bonus Pot now has its own stable application path.
shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::tags$script(shiny::HTML("window.location.replace('/bonus-pot/');")),
    shiny::tags$p("Bonus Pot has moved. ", shiny::tags$a(href = "/bonus-pot/", "Continue to Bonus Pot."))
  ),
  server = function(input, output, session) {}
)
