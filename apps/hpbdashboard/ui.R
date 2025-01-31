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
            "LIVER: Number of procedures",
            "LIVER: Number of open/closed procedures",
            "LIVER: Number of omplications",
            "LIVER: Number of days MDT to surgery",
            "LIVER: Number of days surgery to discharge",
            "PANCREAS: Number of procedures",
            "PANCREAS: Number of open/closed procedures",
            "PANCREAS: Number of complications",
            "PANCREAS: Number of days MDT to surgery",
            "PANCREAS: Number of days surgery to discharge",
            "PANCREAS: Number of days OPEN surgery to discharge",
            "PANCREAS: Number of days ROBOT surgery to discharge",
            "PANCREAS: Number of days LAPAROSCOPIC surgery to discharge"
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
