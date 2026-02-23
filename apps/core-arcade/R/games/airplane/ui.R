# apps/core-arcade/R/games/airplane/ui.R
# Shiny module UI for the Paper Aeroplane Production game.

airplane_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Paper Aeroplane Production"),
    shiny::p(shiny::em("Submit your approved-planes count for this round.")),
    shiny::numericInput(ns("approved"), "Approved planes this round:", value = 0, min = 0, step = 1),
    shiny::actionButton(ns("submit"), "Submit", class = "btn-primary"),
    shiny::verbatimTextOutput(ns("confirmation"))
  )
}
