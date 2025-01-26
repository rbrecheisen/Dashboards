source("dependencies.R")

library(shiny)
library(reticulate)

source_python("script.py")


ui <- fluidPage(
  titlePanel("Modular Shiny App"),
  sidebarLayout(
    sidebarPanel(
      actionButton("run_button", "Run")
    ),
    mainPanel(
      verbatimTextOutput("result")
    )
  )
)


server <- function(input, output, session) {
  observeEvent(input$run_button, {
    output$result <- renderText({
      py_execute()
    })
  })
}


shinyApp(ui, server)