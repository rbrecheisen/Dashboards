library(shiny)
library(ggplot2)


freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(x = c(x1, x2), g = c(rep("x1", length(x1)), rep("x2", length(x2))))
  ggplot(df, aes(x, colour = g)) + 
    geom_freqpoly(binwidth = binwidth, linewidth = 1) +
    coord_cartesian(xlim = xlim)
}


test <- function(x1, x2) {
  t <- t.test(x1, x2)
  sprintf(
    "p-value: %0.3f\n[%0.2f, %0.2f]",
    t$p.value, t$conf.int[1], t$conf.int[2]
  )
}


ui <- fluidPage(
  fluidRow(
    column(
      4,
      "Distribution 1",
      numericInput("n1", label = "n", value = 1000, min = 1),
      numericInput("mean1", label = "mean", value = 0, step = 0.1),
      numericInput("stdev1", label = "stdev", value = 0.5, min = 0.1, step = 0.1),
    ),
    column(
      4,
      "Distribution 2",
      numericInput("n2", label = "n", value = 1000, min = 1),
      numericInput("mean2", label = "mean", value = 0, step = 0.1),
      numericInput("stdev2", label = "stdev", value = 0.5, min = 0.1, step = 0.1),
    ),
    column(
      4,
      "Frequency Polygon",
      numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
      sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
    )
  ),
  fluidRow(
    column(9, plotOutput("histogram")),
    column(3, verbatimTextOutput("test"))
  )
)


server <- function(input, output, session) {
  x1 <- reactive(rnorm(input$n1, input$mean1, input$stdev1))
  x2 <- reactive(rnorm(input$n2, input$mean2, input$stdev2))
  output$histogram <- renderPlot({
    freqpoly(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  output$test <- renderText({
    test(x1(), x2())
  })
}


shinyApp(ui, server)