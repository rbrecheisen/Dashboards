library(shiny)


ui <- fluidPage(
  textInput("name", "name"),
  textOutput("greeting")
)


server <- function(input, output, session) {
  msg <- reactive(paste0("Hello ", input$name, "!"))
  output$greeting <- renderText(msg())
  observeEvent(input$name, {
    message("Greeting performed")
  })
}


shinyApp(ui, server)