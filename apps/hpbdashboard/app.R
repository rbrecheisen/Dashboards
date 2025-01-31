source("dependencies.R")

library(shiny)

source("castorapiclient.R")
source("charts/liverprocedureschart.R")
source("charts/liverproceduresopenclosedchart.R")
source("charts/livercomplicationschart.R")
source("charts/livertimemdttosurgerychart.R")
source("charts/livertimesurgerytodischargechart.R")
source("charts/pancreasprocedureschart.R")
source("charts/pancreasproceduresopenclosedchart.R")
source("charts/pancreascomplicationschart.R")
source("charts/pancreastimemdttosurgerychart.R")
source("charts/pancreastimesurgerytodischargechart.R")
source("charts/pancreastimeopensurgerytodischargechart.R")
source("charts/pancreastimerobotsurgerytodischargechart.R")
source("charts/pancreastimelaparoscopicsurgerytodischargechart.R")
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
    api_client = CastorApiClient$new(client_id = input$client_id, client_secret = input$client_secret)
    df(api_client$get_study_data_as_dataframe(input$study_name))
    api_client$save_data()
    api_client$save_field_definitions()
    runjs("$('#connect').addClass('btn-connected')")
    showNotification("Connected!", type = "message")
    connected(TRUE)
  })
  
  output$dynamic_panel = renderUI({
    plotOutput("selected_chart")
  })
  
  output$selected_chart = renderPlot({
    req(df())
    if(input$chart == "LIVER: Number of procedures") {
      chart = LiverProceduresChart$new(df())
      chart$show()
    } 
    else if(input$chart == "LIVER: Number of open/closed procedures") {
      chart = LiverProceduresOpenClosedChart$new(df())
      chart$show()
    }
    else if(input$chart == "LIVER: Number of complications") {
      chart = LiverComplicationsChart$new(df())
      chart$show()
    } 
    else if(input$chart == "LIVER: Number of days MDT to surgery") {
      chart = LiverTimeMdtToSurgeryChart$new(df())
      chart$show()
    } 
    else if(input$chart == "LIVER: Number of days surgery to discharge") {
      chart = LiverTimeSurgeryToDischargeChart$new(df())
      chart$show()
    } 
    else if(input$chart == "PANCREAS: Number of procedures") {
      chart = PancreasProceduresChart$new(df())
      chart$show()
    } 
    else if(input$chart == "PANCREAS: Number of open/closed procedures") {
      chart = PancreasProceduresOpenClosedChart$new(df())
      chart$show()
    } 
    else if(input$chart == "PANCREAS: Number of omplications") {
      chart = PancreasComplicationsChart$new(df())
      chart$show()
    }
    else if(input$chart == "PANCREAS: Number of days MDT to surgery") {
      chart = PancreasTimeMdtToSurgeryChart$new(df())
      chart$show()
    }
    else if(input$chart == "PANCREAS: Number of days surgery to discharge") {
      chart = PancreasTimeSurgeryToDischargeChart$new(df())
      chart$show()
    }
    else if(input$chart == "PANCREAS: Number of days OPEN surgery to discharge") {
      chart = PancreasTimeOpenSurgeryToDischargeChart$new(df())
      chart$show()
    }
    else if(input$chart == "PANCREAS: Number of days ROBOT surgery to discharge") {
      chart = PancreasTimeRobotSurgeryToDischargeChart$new(df())
      chart$show()
    }
    else if(input$chart == "PANCREAS: Number of days LAPAROSCOPIC surgery to discharge") {
      chart = PancreasTimeLaparoscopicSurgeryToDischargeChart$new(df())
      chart$show()
    }
  })
}

shinyApp(ui = ui, server = server)