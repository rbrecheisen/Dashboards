library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)

df <- read_csv("random_data.csv")
df <- df %>%
  mutate(Date = ymd(Date)) %>%
  mutate(MonthYear = paste0(month(Date, label = TRUE), year(Date))) %>%
  group_by(MonthYear) %>%
  summarise(across(starts_with("Column"), sum)) %>%
  arrange(match(MonthYear, paste0(month.abb, unique(year(df$Date)))))
df

ui <- fluidPage(
  titlePanel("Complications"),
  DTOutput("table"),
  selectInput("column", "Select complication:", choices = colnames(df)[-1], selected = colnames(df)[2]),
  plotOutput("plot")
)

server <- function(input, output, session) {
  
  output$table <- renderDT({
    datatable(df, selection = "none", options = list(scrollX = TRUE))
  })
  
  observe({
    updateSelectInput(session, "column", choices = colnames(df)[-1])
  })
  
  output$plot <- renderPlot({
    req(input$column)
    ggplot(df, aes(x = MonthYear, y = get(input$column))) +
      geom_col(fill = "steelblue") +
      labs(title = paste("Bar Chart of", input$column), x = "MonthYear", y = input$column) +
      theme_minimal()
  })
}

shinyApp(ui, server)
