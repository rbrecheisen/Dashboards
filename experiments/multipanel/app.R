library(shiny)
library(shinyjs)
library(httr)
library(R6)
library(jsonlite)
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)


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


Chart = R6Class(

  "Chart",

  public = list(

    df = NULL,
    tmp_dir = NULL,

    initialize = function(df, tmp_dir = NULL) {
      self$tmp_dir = tmp_dir
      self$df = df
    },

    show = function() {
      message("Not implemented")
    }
  )
)


LiverProceduresPerMonthChart = R6Class(

  "LiverProceduresPerMonthChart",

  inherit = Chart,

  public = list(

    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 0,
          operatie_lever_operatie_niet_doorgegaan != 1,
          resectie != 6
        )
      self$df = self$df %>%
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month) %>%
        summarise(num_procedures = n())
    },

    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures)) +
        geom_bar(stat = "identity") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(
          title = "Number of liver procedures per month",
          x = "Month",
          y = "Number of procedures"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)


LiverProceduresPerMonthOpenClosedChart = R6Class(

  "LiverProceduresPerMonthOpenClosedChart",

  inherit = Chart,

  public = list(

    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 0,
          operatie_lever_operatie_niet_doorgegaan != 1,
          resectie == 6
        )
      self$df = self$df %>%
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month) %>%
        summarise(num_procedures = n())
    },

    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures)) +
        geom_bar(stat = "identity") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(
          title = "Number of open/closed liver procedures per month",
          x = "Month",
          y = "Number of procedures"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)


LiverComplicationsPerMonthChart = R6Class(

  "LiverComplicationsPerMonthChart",

  inherit = Chart,

  public = list(

    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        mutate(orgaanfalen = factor(
          replace_na(orgaanfalen, "-1"),
          levels = c("-1", "0", "1", "2"),
          labels = c("Missing", "No", "Single-organ", "Multi-organ")
        )) %>%
        filter(
          lever_pancreas == 0,
          operatie_lever_operatie_niet_doorgegaan != 1,
          resectie != 6,
          complicatie_ok == 1,
          complicaties_waarvoor_reinterventie == 1,
          orgaanfalen %in% c("Single-organ", "Multi-organ")
        )
      self$df = self$df %>%
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month, orgaanfalen) %>%
        summarize(num_procedures = n(), .groups = "drop")
    },

    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures, fill = orgaanfalen)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Number of liver complications per month",
          x = "Month",
          y = "Number of complications",
          fill = "Orgaanfalen"
        ) +
        theme_minimal() +
        scale_fill_discrete(labels = c(
          "-1" = "Missing",
          "0" = "No",
          "1" = "Single-orgaan",
          "2" = "Multi-orgaan"
        )) +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)


PancreasProceduresPerMonthChart = R6Class(

  "PancreasProceduresPerMonthChart",

  inherit = Chart,

  public = list(

    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 1,
          operatie_pancreas != 11,
          operatie_pancreas_techniek != 7
        )
      self$df = self$df %>%
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month, operatie_pancreas_techniek) %>%
        summarize(num_procedures = n(), .groups = "drop")
    },

    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures, fill = operatie_pancreas_techniek)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Number of pancreas procedures per month",
          x = "Month",
          y = "Number of procedures",
          fill = "Surgical technique"
        ) +
        theme_minimal() +
        scale_fill_discrete(labels = c(
          "0" = "Open procedure",
          "1" = "Fully laparoscopic",
          "2" = "Laparoscopic with conversion",
          "3" = "Fully robotic",
          "4" = "Robotic with conversion to laparoscopic",
          "5" = "Robot with conversion to open",
          "6" = "Laparoscopic resection with robotic reconstruction",
          "7" = "Exploration without resection",
          "8" = "Other",
          "9" = "Not applicable"
        )) +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)


PancreasProceduresOpenClosedPerMonthChart = R6Class(

  "PancreasProceduresOpenClosedPerMonthChart",

  inherit = Chart,

  public = list(

    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 1,
          operatie_pancreas != 11,
          operatie_pancreas_techniek == 7
        )
      self$df = self$df %>%
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month) %>%
        summarize(num_procedures = n(), .groups = "drop")
    },

    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Number of open/closed pancreas procedures per month",
          x = "Month",
          y = "Number of procedures"
        ) +
        theme_minimal() +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
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

# study_info_file = file.path(file.path(path.expand("~"), ".castordashboard"), "study_info.rds")
# study_info = readRDS(study_info_file)
# input = list(client_id = study_info$client_id, client_secret = study_info$client_secret, study_name = "ESPRESSO_v3.0")
# api_client = CastorAPI$new(client_id = input$client_id, client_secret = input$client_secret)
# df = api_client$get_study_data_as_dataframe(input$study_name)
# chart = PancreasProceduresPerMonthChart$new(df)
# # chart = PancreasProceduresOpenClosedPerMonthChart$new(df)
# chart$show()
