source("dependencies.R")

library(shiny)
library(DT)
library(arrow)
library(ggplot2)

# Define UI
ui <- fluidPage(
  
  titlePanel("TLC Taxi Trip Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Parquet File", accept = ".parquet"),
      selectInput("column", "Select Column for Visualization", choices = NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Overview", DTOutput("table")),
        tabPanel("Column Info", DTOutput("col_info")),
        tabPanel("Summary Stats", DTOutput("summary_stats")),
        tabPanel("Visualization", plotOutput("plot"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 500 * 1024^2)
  
  data <- reactive({
    req(input$file)
    df <- read_parquet(input$file$datapath)
    updateSelectInput(session, "column", choices = names(df))
    df
  })
  
  output$table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10))
  })
  
  output$col_info <- renderDT({
    req(data())
    df <- data()
    col_info <- data.frame(
      Column = names(df),
      DataType = sapply(df, class)
    )
    datatable(col_info)
  })
  
  output$summary_stats <- renderDT({
    req(data())
    df <- data()
    summary_stats <- data.frame(
      Column = names(df),
      Mean = sapply(df, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
      Variance = sapply(df, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA),
      Missing_Values = sapply(df, function(x) sum(is.na(x)))
    )
    datatable(summary_stats)
  })
  
  output$plot <- renderPlot({
    req(data(), input$column)
    df <- data()
    
    if (is.numeric(df[[input$column]])) {
      ggplot(df, aes_string(x = input$column)) +
        geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
        labs(title = paste("Distribution of", input$column))
    } else {
      ggplot(df, aes_string(x = input$column)) +
        geom_bar(fill = "blue", alpha = 0.7) +
        labs(title = paste("Count of", input$column))
    }
  })
}

# Run app
shinyApp(ui, server)
