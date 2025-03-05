# install.packages("remote")
# devtools::install_github("ColinFay/brochure")

library(shiny)
library(brochure)

brochureApp(
  page(
    href = "/",
    ui = fluidPage(
      h1("This is my first page"),
      plotOutput("plot")
    ),
    server = function(input, output, session) {
      output$plot <- renderPlot({
        plot(iris)
      })
    }
  ),
  page(
    href = "/about",
    ui = fluidPage(
      h1("This is the about page"),
      tags$p("There is no server function for this page")
    )
  )
)
