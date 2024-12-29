library(shiny)

ui = fluidPage(
  titlePanel("Central Limit Theorem"),
  sidebarLayout(
    sidebarPanel(
      numericInput("m", "Nr. of observations:", 2, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

server = function(input, output, session) {
  output$histogram = renderPlot({
    means = replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  }, res = 96)
}

shinyApp(ui, server)