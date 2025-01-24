library(shiny)


# modules/module1.R
module1UI <- function(id) {
  ns <- NS(id)  # Namespace to avoid input/output conflicts
  tagList(
    textInput(ns("text"), "Enter text:"),
    actionButton(ns("btn"), "Submit"),
    textOutput(ns("result"))
  )
}

module1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$result <- renderText({
      paste("You entered:", input$text)
    })
  })
}
