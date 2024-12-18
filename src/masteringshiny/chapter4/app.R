library(shiny)
library(vroom)
library(tidyverse)
library(rstudioapi)


# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(file.path("D:", "TemporaryData"))
dir.create("neiss")


download <- function(name) {
  target_file <- file.path("neiss", name)
  download.file(paste0("https://raw.github.com/hadley/mastering-shiny/main/neiss/", name), target_file, quiet = TRUE)
}


download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")


injuries <- vroom::vroom(file.path("neiss", "injuries.tsv.gz"))
injuries

products <- vroom::vroom(file.path("neiss", "products.tsv"))
products

population <- vroom::vroom(file.path("neiss", "population.tsv"))
population

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}


product_codes <- setNames(products$prod_code, products$title)


ui <- fluidPage(
  fluidRow(
    column(6, selectInput("code", "product", choices = product_codes))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)


server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))

  output$diag <- renderTable({
    # selected() %>% count(diag, wt = weight, sort = TRUE)
    count_top(selected(), diag)
  }, width = "100%")

  output$body_part <- renderTable({
    # selected() %>% count(body_part, wt = weight, sort = TRUE)
    count_top(selected(), body_part)
  }, width = "100%")
  
  output$location <- renderTable({
    # selected() %>% count(location, wt = weight, sort = TRUE)
    count_top(selected(), location)
  }, width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
}


shinyApp(ui, server)
