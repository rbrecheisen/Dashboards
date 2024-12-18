library(shiny)

animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")

ui <- fluidPage(
  
  textInput("name", "What's your name?"),
  
  passwordInput("password", "What's your password?"),
  
  textAreaInput("story", "Tell me about yourself", rows = 3),
  
  numericInput("num1", "Number One", value = 0, min = 0, max = 100),
  
  sliderInput("num2", "Number Two", value = 50, min = 0, max = 100),
  
  sliderInput("range", "Range", value = c(10, 20), min = 0, max = 100),
  
  dateInput("dob", "When were you born?"),
  
  dateRangeInput("holiday", "When is your holiday?"),
  
  selectInput("state", "What is your favorite state?", state.name),
  
  radioButtons("animal", "What is your favorite animal?", animals),
  
  radioButtons("rb", "Choose one:", 
    choiceNames = list(
     icon("angry"),
     icon("smile"),
     icon("sad-tear")
    ),
    choiceValues = list("angry", "happy", "sad")
  ),
  
  selectInput("stateDropDown", "What is your favorite state?", state.name, multiple = TRUE),
  
  checkboxGroupInput("animalsMultiple", "What is your favorite animal?", animals),

  fileInput("upload", NULL),
  
  actionButton("click", "Click me!", class = "btn-danger"),
  
  actionButton("drink", "Drink me!", icon = icon("cocktail"), class = "btn-success"),
  
  sliderInput("dateSlider", "Date Slider", value = Sys.Date(), min = as.Date("2024-01-01"), max = as.Date("2024-12-31")),
  
  selectInput("sections", "Sections", choices = list(
    "Fruits" = list("Apple" = "apple", "Banana" = "banana"),
    "Vegetables" = list("Carrot" = "carrot", "Spinach" = "spinach"),
    "Beverages" = list("Water" = "water", "Coffee" = "coffee")
  ))
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)