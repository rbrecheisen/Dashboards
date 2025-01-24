library(shiny)
library(shinyjs)


build_ui = function() {
  ui = fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
      .btn-connected { background-color: lightgreen !important; color: white; }
    "))
    ),
    titlePanel("HPB performance dashboard"),
    sidebarLayout(
      sidebarPanel(
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
            6,
            actionButton(
              inputId = "save_info",
              label = "Save",
              width = "100%"
            )
          ),
          column(
            6,
            actionButton(
              inputId = "connect",
              label = "Connect",
              width = "100%"
            )
          )
        ),
        br(),
        br(),
        selectInput(
          inputId = "chart",
          label = "Select a chart:",
          choices = c(
            "Liver procedures",
            "Liver procedures open/closed",
            "Liver complications",
            "Liver time MDT to surgery",
            "Pancreas procedures",
            "Pancreas procedures open/closed",
            "Pancreas complications"
          ),
          selected = "Liver procedures"
        )
      ),
      mainPanel(
        uiOutput("dynamic_panel")
      )
    )
  )
  return(ui)
}
