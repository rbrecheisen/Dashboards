library(shiny)
library(ggplot2)


freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(x = c(x1, x2), g = c(rep("x1", length(x1)), rep("x2", length(x2))))
  ggplot(df, aes(x, colour = g)) + 
    geom_freqpoly(binwidth = binwidth, linewidth = 1) +
    coord_cartesian(xlim = xlim)
}


ui <- fluidPage(
  fluidRow(
    column(
      3,
      numericInput("lambda1", "lambda1", value = 3),
      numericInput("lambda2", "lambda2", value = 5),
      numericInput("n", "n", value = 1e4, min = 0),
      actionButton("simulate", "simulate")
    ),
    column(9, plotOutput("histogram"))
  )
)


# server <- function(input, output, session) {
#   timer <- reactiveTimer(500)
#   x1 <- reactive({
#     timer()
#     rpois(input$n, input$lambda1)
#   })
#   x2 <- reactive({
#     timer()
#     rpois(input$n, input$lambda2)
#   })
#   output$histogram <- renderPlot({
#     freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
#   }, res = 96)
# }


server <- function(input, output, session) {
  x1 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda1)
  })
  x2 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda2)
  })
  output$histogram <- renderPlot({
    freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}


shinyApp(ui, server)