library(shiny)
library(shinyjs)


CastorAPI = R6Class(
  
  "CastorAPI",
  
  public = list(
    
    api_token_url = "https://data.castoredc.com/oauth/token",
    api_base_url = "https://data.castoredc.com/api",
    token = NULL,
    studies = NULL,
    nr_retries = 1,
    retry_waiting_time = 1,
    
    initialize = function(client_id, client_secret, nr_retries = 5, retry_waiting_time = 5) {
      self$token = self$connect(client_id, client_secret, self$api_token_url)
      self$nr_retries = nr_retries
      self$retry_waiting_time = retry_waiting_time
    },
    
    connect = function(client_id, client_secret, token_url) {
      response = POST(
        url = token_url,
        encode = "form",
        body = list(
          grant_type = "client_credentials",
          client_id = client_id,
          client_secret = client_secret
        )
      )
      if(http_status(response)$category != "Success") {
        stop("Failed to authenticate with Castor API: ", content(response, "text", encoding = "UTF-8"))
      }
      token = content(response, "parsed")$access_token
      if(is.null(token)) {
        stop("No access token received")
      }
      return(token)
    },
    
    get_studies = function() {
      if(!is.null(self$studies)) {
        return(self$studies)
      }
      response = GET(
        url = paste0(self$api_base_url, "/study"),
        add_headers(Authorization = paste("Bearer", self$token))
      )
      if(http_status(response)$category != "Success") {
        stop("API request failed", content(response, "text"))
      }
      response_content = content(response, "text", encoding = "UTF-8")
      json_content = fromJSON(response_content)
      self$studies = data.frame(
        study_id = json_content$`_embedded`$study$study_id,
        study_name = json_content$`_embedded`$study$name
      )
      return(self$studies)
    },
    
    get_study_name_by_id = function(study_id) {
      studies = self$get_studies()
      for(i in 1:nrow(studies)) {
        if(studies$study_id[i] == study_id) {
          return(studies$study_name[i])
        }
      }
      return(NULL)
    },
    
    get_study_id_by_name = function(study_name) {
      studies = self$get_studies()
      for(i in 1:nrow(studies)) {
        if(studies$study_name[i] == study_name) {
          return(studies$study_id[i])
        }
      }
      return(NULL)
    },
    
    get_study_data_as_csv = function(study_id, data_type, tmp_dir = NULL) {
      stopifnot(data_type %in% c('data', 'optiongroups', 'structure'))
      url = paste0(self$api_base_url, "/study/", study_id, "/export/", data_type)
      count = 0; status_code = 0; response = NULL
      while(status_code != 200 && count <= self$nr_retries) {
        response = GET(url, add_headers(Authorization = paste("Bearer", self$token)))
        status_code = response$status_code
        if(status_code == 200) {
          break
        } else if(count == self$nr_retries) {
          message(sprintf("Could not retrieve %s for study ID %s (status: %d)", data_type, study_id, status_code))
          return(NULL)
        }
        count = count + 1
        Sys.sleep(self$retry_waiting_time)
      }
      content = content(response, as = "text", encoding = "UTF-8")
      if(!is.null(tmp_dir)) {
        study_name = self$get_study_name_by_id(study_id)
        dir.create(file.path(tmp_dir, study_name), recursive = TRUE, showWarnings = FALSE)
        file_path = file.path(tmp_dir, study_name, paste0(data_type, ".csv"))
        writeLines(content, file_path, useBytes = TRUE)
        message(sprintf("Data written to %s", file_path))
      }
      return(content)
    },
    
    load_csv_data = function(csv_data) {
      dict_data = read_delim(csv_data, delim = ";", col_names = TRUE, show_col_types = FALSE, name_repair = "minimal")
      return(dict_data)
    },
    
    one_hot_encode_multi_value_columns = function(df, structure, optiongroups) {
      return(df)
    },
    
    get_study_data_as_dataframe = function(study_name, tmp_dir = NULL) {
      study_id = self$get_study_id_by_name(study_name)
      field_defs = self$load_csv_data(self$get_study_data_as_csv(study_id, "structure", tmp_dir))
      optiongroups = self$load_csv_data(self$get_study_data_as_csv(study_id, "optiongroups", tmp_dir))
      data = self$load_csv_data(self$get_study_data_as_csv(study_id, "data", tmp_dir))
      # Merge datasets and build initial dataframe
      data = data %>%
        left_join(
          field_defs, by = "Field ID") %>%
        select(
          `Record ID`, `Field Variable Name`, `Value`)
      df = data %>%
        pivot_wider(
          id_cols = `Record ID`, names_from = `Field Variable Name`, values_from = `Value`)
      # One-hot encode multi-value columns
      multi_value_columns = field_defs$`Field Variable Name`[field_defs$`Field Type` == "checkbox"]
      for(column in multi_value_columns) {
        optiongroup_id = field_defs$`Field Option Group`[field_defs$`Field Variable Name` == column]
        option_values = optiongroups$`Option Value`[optiongroups$`Option Group Id` == optiongroup_id]
        option_names = optiongroups$`Option Name`[optiongroups$`Option Group Id` == optiongroup_id]
        for(i in 1:length(option_values)) {
          df[[paste0(column, "_", option_names[i])]] = sapply(df[[column]], function(x) {
            option_values[i] %in% unlist(strsplit(x, ";"))
          }) * 1
        }
      }
      df = df %>% select(-all_of(multi_value_columns))
      df = df %>% select(-"NA")
      df = df %>% clean_names()
      if(!is.null(tmp_dir)) {
        write.csv2(df, file = sprintf("%s/%s/df.csv", tmp_dir, study_name), row.names = FALSE)
      }
      return(df)
    }
  )
)


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
      fluidRow(
        column(
          6, 
          actionButton(
            inputId = "save_credentials", 
            label = "Save to file", 
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
        inputId = "panel_selector",
        label = "Select a chart:",
        choices = c(
          "Panel 1", 
          "Panel 2", 
          "Panel 3"
        ),
        selected = "Panel 1"
      )
  ),
  mainPanel(
    uiOutput("dynamic_panel")
  )
)
)

server = function(input, output, session) {
  app_dir = file.path(path.expand("~"), ".castordashboard")
  if(!dir.exists(app_dir)) {
    dir.create(app_dir, showWarnings = FALSE)
  }
  credentials_file = file.path(app_dir, "credentials.rds")
  if(file.exists(credentials_file)) {
    credentials = readRDS(credentials_file)
    if(!is.null(credentials) && length(credentials) >= 2) {
      updateTextInput(session, "client_id", value = credentials$client_id)
      updateTextInput(session, "client_secret", value = credentials$client_secret)
    }
  }
  observeEvent(input$save_credentials, {
    saveRDS(list(client_id = input$client_id, client_secret = input$client_secret), credentials_file)
    showNotification("Credentials saved successfully!", type = "message")
  })
  observeEvent(input$connect, {
    api_client = CastorAPI$new(client_id = input$client_id, client_secret = input$client_secret)
    runjs("$('#connect').addClass('btn-connected')")
    showNotification("Connected!", type = "message")
  })
  output$dynamic_panel <- renderUI({
    switch(
      input$panel_selector,
      "Panel 1" = div(h3("Welcome to Panel 1"), p("This is the content of Panel 1.")),
      "Panel 2" = div(h3("Welcome to Panel 2"), p("This is the content of Panel 2.")),
      "Panel 3" = div(h3("Welcome to Panel 3"), p("This is the content of Panel 3."))
    )
  })
}

shinyApp(ui = ui, server = server)
