library(httr)
library(R6)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)


CastorAPI <- R6Class(
  
  "CastorAPI",
  
  public = list(
    
    api_base_url = NULL,
    token = NULL,
    studies = NULL,
    nr_retries = 1,
    retry_waiting_time = 1,
    
    # ==========================================================================
    # Retrieves JSON structure containing all Castor studies associated with this
    # account
    # - api_endpoint_url: API endpoint URL for studies
    # - token: API access token
    # return: studies
    initialize = function(client_id, client_secret, api_base_url, token_url, nr_retries = 5, retry_waiting_time = 5) {
      self$api_base_url = api_base_url
      self$token <- self$connect(client_id, client_secret, token_url)
      self$nr_retries = nr_retries
      self$retry_waiting_time = retry_waiting_time
    },
    
    # ==========================================================================
    # Connects to the Castor API
    # - client_id: Client ID for this account
    # - client_secret: Client secret for this account
    # - token_url: Endpoint for retrieving access token for this connection
    # return: access token
    connect = function(client_id, client_secret, token_url) {
      response <- POST(
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
      token <- content(response, "parsed")$access_token
      if(is.null(token)) {
        stop("No access token received")
      }
      message("Access token received successfully")
      return(token)
    },

    # ==========================================================================
    # Retrieves study IDs and names for all studies
    # return: studies
    get_studies = function() {
      if(!is.null(self$studies)) {
        return(self$studies)
      }
      response <- GET(
        url = paste0(self$api_base_url, "/study"),
        add_headers(Authorization = paste("Bearer", self$token))
      )
      if(http_status(response)$category != "Success") {
        stop("API request failed", content(response, "text"))
      }
      response_content <- content(response, "text", encoding = "UTF-8")
      json_content <- fromJSON(response_content)
      self$studies <- data.frame(
        study_id = json_content$`_embedded`$study$study_id,
        study_name = json_content$`_embedded`$study$name
      )
      return(self$studies)
    },
    
    # ==========================================================================
    # Retrieves study name for given study ID
    # - study_id: Study ID
    # return: study name
    get_study_name_by_id = function(study_id) {
      studies <- self$get_studies()
      for(i in 1:nrow(studies)) {
        if(studies$study_id[i] == study_id) {
          return(studies$study_name[i])
        }
      }
      return(NULL)
    },
    
    # ==========================================================================
    # Retrieves study ID for given study name
    # - study_name: Study name
    # return: study ID
    get_study_id_by_name = function(study_name) {
      studies <- self$get_studies()
      for(i in 1:nrow(studies)) {
        if(studies$study_name[i] == study_name) {
          return(studies$study_id[i])
        }
      }
      return(NULL)
    },

    # ==========================================================================
    # Retrieves study data in CSV format. There are 3 data types: data, optiongroups
    # and structure
    # - study_id: The study ID to retrieve data from
    # - data_type: Data type. Must be one of ["data", "optiongroup", "structure"]
    # - tpm_dir: Temporary directory for storing output files
    # return: Study data in CSV format
    get_study_data_as_csv = function(study_id, data_type, tmp_dir = NULL) {
      stopifnot(data_type %in% c('data', 'optiongroups', 'structure'))
      url <- paste0(self$api_base_url, "/study/", study_id, "/export/", data_type)
      count <- 0; status_code <- 0; response <- NULL
      while(status_code != 200 && count <= self$nr_retries) {
        response <- GET(url, add_headers(Authorization = paste("Bearer", self$token)))
        status_code <- response$status_code
        if(status_code == 200) {
          break
        } else if(count == self$nr_retries) {
          message(sprintf("Could not retrieve %s for study ID %s (status: %d)", data_type, study_id, status_code))
          return(NULL)
        }
        count <- count + 1
        Sys.sleep(self$retry_waiting_time)
      }
      content <- content(response, as = "text", encoding = "UTF-8")
      if(!is.null(tmp_dir)) {
        study_name <- self$get_study_name_by_id(study_id)
        dir.create(file.path(tmp_dir, study_name), recursive = TRUE, showWarnings = FALSE)
        file_path <- file.path(tmp_dir, study_name, paste0(data_type, ".csv"))
        writeLines(content, file_path, useBytes = TRUE)
        message(sprintf("Data written to %s", file_path))
      }
      return(content)
    },
    
    get_field_type = function() {
      
    },
    
    get_optiongroup_id = function() {
      
    },
    
    get_option_names_and_values = function() {
      
    },
    
    get_record_value = function(field_id, record_id, data) {
      for(record in data) {
        if(!is.na(record[["Field ID"]]) && record[["Field ID"]] == field_id && record[["Record ID"]] == record_id) {
          return(record[["Value"]])
        }
      }
      return("")
    },
    
    # ==========================================================================
    # Loads CSV data as a list of dictionaries (each row one dictionary)
    # - csv_data: CSV data as text
    # return: List of dictionaries
    load_csv_data = function(csv_data) {
      dict_data <- read_delim(csv_data, delim = ";", col_names = TRUE, show_col_types = FALSE)
      dict_data_list <- split(dict_data, seq(nrow(dict_data)))
      return(dict_data_list)
    },

    # ==========================================================================
    # One-hot encodes multi-value columns (checkboxes)
    # - df: Dataframe to update
    # - structure: Castor structure data
    # - optiongroups: Castor option group data
    # return: Dataframe with one-hot encoded multi-value columns
    one_hot_encode_multi_value_columns = function(df, structure, optiongroups) {
      return(df)
    },
    
    # ==========================================================================
    # Returns study data as a dataframe
    # - study_id: ID of the study to retrieve
    # return: Dataframe with study data
    get_study_data_as_dataframe = function(study_id, tmp_dir = NULL) {
      
      structure <- self$load_csv_data(self$get_study_data_as_csv(study_id, "structure", tmp_dir))
      optiongroups <- self$load_csv_data(self$get_study_data_as_csv(study_id, "optiongroups", tmp_dir))
      data <- self$load_csv_data(self$get_study_data_as_csv(study_id, "data", tmp_dir))
      
      records <- list()
      for(record in data) {
        if(is.na(record[["Field ID"]])) {
          new_record <- list("Record ID" = record[["Record ID"]])
          for(field_def in structure) {
            new_record[[field_def[["Field Variable Name"]]]] <- ""
          }
          records <- append(records, list(new_record))
        }
      }
      
      values = list()
      for(record in records) {
        for(field_def in structure) {
          field_id = field_def[["Field ID"]]
          field_variable_name = field_def[["Field Variable Name"]]
          value = self$get_record_value(field_id, record[["Record ID"]], data)
          values = append(values, value)
        }
      }
      
      return(records)
    }
  )
)