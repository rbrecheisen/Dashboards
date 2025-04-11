library(shiny)
library(shinyjs)


ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
    .btn-connected { background-color: lightgreen !important; color: white; }
  "))
  ),
  titlePanel("HPB performance dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      textInput(
        inputId = "client_id",
        label = "Client ID",
        value = ""
      ),
      textInput(
        inputId = "client_secret",
        label = "Client secret",
        value = ""
      ),
      textInput(
        inputId = "study_name",
        label = "Castor study name",
        value = ""
      ),
      fluidRow(
        column(
          12,
          actionButton(
            inputId = "connect",
            label = "Connect",
            width = "100%"
          )
        )
      ),
      br(),
      fluidRow(
        column(
          12,
          actionButton(
            inputId = "save_study_data",
            label = "Save study data",
            width = "100%",
            disabled = TRUE
          )
        )
      ),
      br(),
      br(),
      selectInput(
        inputId = "chart",
        label = "Select a chart:",
        choices = c(
          "LIVER: Number of procedures",
          "LIVER: Number of open/closed procedures",
          "LIVER: Number of complications",
          "LIVER: Number of days MDT to surgery",
          "LIVER: Number of days surgery to discharge",
          "PANCREAS: Number of procedures",
          "PANCREAS: Number of open/closed procedures",
          "PANCREAS: Number of complications",
          "PANCREAS: Number of days MDT to surgery",
          "PANCREAS: Number of days surgery to discharge"
        ),
      )
    ),
    mainPanel(
      width = 7,
      uiOutput("dynamic_panel")
    )
  )
)