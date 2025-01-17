library(shiny)
library(shinyjs)
library(tidyverse)

source("castorclient.R")
source("charts.R")


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
          "Pancreas procedures",
          "Pancreas procedures open/closed",
          "Pancreas complications"
        ),
        selected = "Liver complications per month"
      )
    ),
    mainPanel(
      uiOutput("dynamic_panel")
    )
  )
)


server = function(input, output, session) {
  app_dir = file.path(path.expand("~"), ".castordashboard")
  connected = reactiveVal(FALSE)
  df = reactiveVal(NULL)
  if(!dir.exists(app_dir)) {
    dir.create(app_dir, showWarnings = TRUE)
  }
  study_info_file = file.path(app_dir, "study_info.rds")
  if(file.exists(study_info_file)) {
    study_info = readRDS(study_info_file)
    if(!is.null(study_info) && length(study_info) >= 2) {
      updateTextInput(session, "client_id", value = study_info$client_id)
      updateTextInput(session, "client_secret", value = study_info$client_secret)
      updateTextInput(session, "study_name", value = study_info$study_name)
    }
  }
  observeEvent(input$save_info, {
    saveRDS(list(client_id = input$client_id, client_secret = input$client_secret, study_name = input$study_name), study_info_file)
    showNotification("Study info saved", type = "message")
  })
  observeEvent(input$connect, {
    api_client = CastorAPI$new(client_id = input$client_id, client_secret = input$client_secret)
    df(api_client$get_study_data_as_dataframe(input$study_name))
    runjs("$('#connect').addClass('btn-connected')")
    showNotification("Connected!", type = "message")
    connected(TRUE)
  })
  output$dynamic_panel = renderUI({
    plotOutput("selected_chart")
  })
  output$selected_chart = renderPlot({
    req(df())
    if(input$chart == "Liver procedures") {
      chart = LiverProceduresPerMonthChart$new(df())
      chart$show()
    } else if(input$chart == "Liver procedures open/closed") {
      chart = LiverProceduresPerMonthOpenClosedChart$new(df())
      chart$show()
    } else if(input$chart == "Liver complications") {
      chart = LiverComplicationsPerMonthChart$new(df())
      chart$show()
    } else if(input$chart == "Pancreas procedures") {
      chart = PancreasProceduresPerMonthChart$new(df())
      chart$show()
    } else if(input$chart == "Pancreas procedures open/closed") {
        chart = PancreasProceduresOpenClosedPerMonthChart$new(df())
        chart$show()
    } else if(input$chart == "Pancreas complications") {

    }
  })
}

shinyApp(ui = ui, server = server)