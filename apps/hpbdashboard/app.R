source("dependencies.R")

library(shiny)

source("castorclient.R")
source("charts/liverprocedureschart.R")
source("charts/liverproceduresopenclosedchart.R")
source("charts/livercomplicationschart.R")
source("charts/livertimemdttosurgerychart.R")
source("charts/pancreasprocedureschart.R")
source("charts/pancreasproceduresopenclosedchart.R")
source("charts/pancreascomplicationschart.R")
source("ui.R")


ui = build_ui()

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
      chart = LiverProceduresChart$new(df())
      chart$show()
    } else if(input$chart == "Liver procedures open/closed") {
      chart = LiverProceduresOpenClosedChart$new(df())
      chart$show()
    } else if(input$chart == "Liver complications") {
      chart = LiverComplicationsChart$new(df())
      chart$show()
    } else if(input$chart == "Liver time MDT to surgery") {
      chart = LiverTimeMdtToSurgeryChart$new(df())
      chart$show()
    } else if(input$chart == "Pancreas procedures") {
      chart = PancreasProceduresChart$new(df())
      chart$show()
    } else if(input$chart == "Pancreas procedures open/closed") {
      chart = PancreasProceduresOpenClosedChart$new(df())
      chart$show()
    } else if(input$chart == "Pancreas complications") {

    }
  })
}

shinyApp(ui = ui, server = server)