source("dependencies.R")

library(shiny)

source("modules/module1.R")
source("modules/module2.R")


ui <- fluidPage(
  titlePanel("Modular Shiny App"),
  sidebarLayout(
    sidebarPanel(
      module1UI("mod1"),
      module2UI("mod2")
    ),
    mainPanel(h3("Main Panel"))
  )
)

server <- function(input, output, session) {
  module1Server("mod1")
  module2Server("mod2")
}

shinyApp(ui, server)