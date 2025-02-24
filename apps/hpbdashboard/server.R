library(shiny)
library(DT)

source("castorapiclient.R")
source("credentials.R")
source("charts.R")
source("ui.R")


load_credentials <- function(session) {
  credentials <- CastorApiCredentials$new()
  client_id <- credentials$load_client_id()
  client_secret <- credentials$load_client_secret()
  study_name <- credentials$load_study_name()

  if(!is.null(client_id)) {
    updateTextInput(session, "client_id", value = client_id)
  }
  else {
    showNotification("Client ID not found. Specify it and click 'Save'.", type = "error")
  }
  
  if(!is.null(client_secret)) {
    updateTextInput(session, "client_secret", value = client_secret)
  }
  else {
    showNotification("Client secret not found. Specify it and click 'Save'.", type = "error")
  }
  
  if(!is.null(study_name)) {
    updateTextInput(session, "study_name", value = study_name)
  }
  else {
    showNotification("Study name not found. Specify it and click 'Save'.", type = "error")
  }
  
  if(!is.null(client_id) && !is.null(client_secret) && !is.null(study_name)) {
    showNotification("Credentials successfully loaded", type = "message")
  }

  return(credentials)
}


save_credentials <- function(credentials, input) {
  if(!is.null(input$client_id) && !is.null(input$client_secret)) {
    credentials$save_client_id(input$client_id)
    credentials$save_client_secret(input$client_secret)
    credentials$save_study_name(input$study_name)
    showNotification("Credentials saved", type = "message")
  }
  else {
    showNotification("Credentials are empty", type = "error")
  }
}


handle_connect_and_get_client <- function(input) {
  if(!is.null(input$client_id) && !is.null(input$client_secret)) {
    client <- CastorApiClient$new(input$client_id, input$client_secret)
    if(client$is_connected()) {
      runjs("$('#connect').addClass('btn-connected')")
      showNotification("Connected!", type = "message")
      return(client)
    }
    else {
      showNotification("Connection failed!", type = "error")
    }
  }
  else {
    showNotification("Missing credentials!", type = "error")
  }
  return(NULL)
}


server <- function(input, output, session) {
  connected <- reactiveVal(FALSE)
  study_data <- reactiveVal(NULL)
  client <- reactiveVal(NULL)
  
  #Load credentials and study name
  credentials <- load_credentials(session)

  # User clicked 'Connect' so try and connect to Castor after saving credentials
  observeEvent(input$connect, {
    save_credentials(credentials, input)
    client(handle_connect_and_get_client(input))
    if(!is.null(client)) {
      study_data(client()$get_study_data(input$study_name))
      showNotification("Retrieved study data from Castor successfully", type = "message")
      updateActionButton(session, "save_study_data", disabled = FALSE)
      connected(TRUE)
    }
    else {
      updateActionButton(session, "save_study_data", disabled = TRUE)
      connected(FALSE)
    }
  })
  
  # User clicked "Save study data" so save the records, field definitions and option groups
  observeEvent(input$save_study_data, {
    req(client())
    req(study_data())
    client()$save_records()
    client()$save_field_defs()
    client()$save_na_counts()
  })
  
  # Create dynamic output panel that can display the selected chart later
  output$dynamic_panel = renderUI({
    plotOutput("selected_chart")
  })

  # Render the selected chart in the dynamic output panel called "selected_chart"
  output$selected_chart = renderPlot({
    req(study_data())
    if(input$chart == "LIVER: Number of procedures") {
      show_liver_nr_procedures_chart(study_data())
    }
    else if(input$chart == "LIVER: Number of open/closed procedures") {
      show_liver_nr_procedures_open_closed_chart(study_data())
    }
    else if(input$chart == "LIVER: Number of complications") {
      show_liver_nr_complications_chart(study_data())
    }
    else if(input$chart == "LIVER: Number of days MDT to surgery") {
      show_liver_nr_days_mdt_to_surgery_chart(study_data())
    }
    else if(input$chart == "LIVER: Number of days surgery to discharge") {
      show_liver_nr_days_surgery_to_discharge_chart(study_data())
    }
    else if(input$chart == "PANCREAS: Number of procedures") {
      show_pancreas_nr_procedures_chart(study_data())
    }
    else if(input$chart == "PANCREAS: Number of open/closed procedures") {
      show_pancreas_nr_open_closed_procedures_chart(study_data())
    }
    else if(input$chart == "PANCREAS: Number of complications") {
      show_pancreas_nr_complications_chart(study_data())
    }
    else if(input$chart == "PANCREAS: Number of days MDT to surgery") {
      show_pancreas_nr_days_mdt_to_surgery_chart(study_data())
    }
    else if(input$chart == "PANCREAS: Number of days surgery to discharge") {
      show_pancreas_nr_days_surgery_to_discharge_chart(study_data())
    }
    else {
      showNotification(paste0("Unknown chart: ", input$chart), type = "error")
    }
  })
}