# Load necessary libraries
library(shiny)
library(ggplot2) # Example dependency

# Define the UI
ui <- fluidPage(
  titlePanel("Simple Shiny App"),
  sidebarLayout(
    sidebarPanel(
      h3("Welcome to the App"),
      p("This is a basic Shiny app with a simple layout.")
    ),
    mainPanel(
      h4("Main Content Area"),
      plotOutput("examplePlot") # Placeholder for a plot
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Render a simple example plot
  output$examplePlot <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_point() +
      labs(title = "Weight vs. MPG", x = "Weight", y = "Miles Per Gallon")
  })
}

# Run the app
shinyApp(ui, server)
