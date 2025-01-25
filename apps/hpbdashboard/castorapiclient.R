library(httr)
library(R6)
library(jsonlite)
library(tidyverse)
library(janitor)
library(lubridate)


CastorApiClient = R6Class("CastorApiClient",
  
  public = list(
    api_token_url = "https://data.castoredc.com/oauth/token",
    api_base_url = "https://data.castoredc.com/api",
    token = NULL,
    studies = NULL,
    nr_retries = -1,
    retry_waiting_time = -1,
    
    #' Title: Initializes Castor API client
    #' 
    #' @description 
    #' Initializes the Castor API client with a client ID and secret.
    #' 
    #' @param client_id API client ID
    #' @param client_secret API client secret
    #' @param nr_retries Number of time to retry API endpoint calls
    #' @param retry_waiting_time Time in seconds before trying to call API endpoint again
    #' 
    #' @examples
    #' # Example usage:
    #' client <- CastorApiClient$new("1234", "ABCD", 5, 5)
    #' 
    #' @export
    initialize = function(client_id, client_secret, nr_retries = 5, retry_waiting_time = 1) {
      self$token <- self$connect(client_id, client_secret, self$api_token_url)
      self$nr_retries <- nr_retries
      self$retry_waiting_time <- retry_waiting_time
    },
    
    #' Title: Connects to API
    #' 
    #' @description 
    #' Makes connection with API using provided client ID and secret. 
    #' 
    #' @param client_id API client ID
    #' @param client_secret API client secret
    #' @param token_url URL endpoint for retrieving API access token
    #' 
    #' @export
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
      
      return(token)
    },
    
    #' Title: Retrieves all studies (permitted for user)
    #' 
    #' @description 
    #' Retrieves all Castor studies for which the user, associated with the client ID 
    #' and secret, has access.
    #' 
    #' @export
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
    
    #' Title: Gets study name for given study ID
    #' 
    #' @description 
    #' Gets study name for given study ID
    #' 
    #' @param study_id Study ID
    #'
    #' @export
    get_study_name_by_id = function(study_id) {
      studies <- self$get_studies()
      
      for(i in 1:nrow(studies)) {
        if(studies$study_id[i] == study_id) {
          return(studies$study_name[i])
        }
      }
      
      return(NULL)
    },
    
    #' Title: Get study ID for given study name
    #' 
    #' @description]
    #' Gets study ID for given study name
    #' 
    #' @param study_name Study name
    #'
    #' @export
    get_study_id_by_name = function(study_name) {
      studies <- self$get_studies()
      
      for(i in 1:nrow(studies)) {
        if(studies$study_name[i] == study_name) {
          return(studies$study_id[i])
        }
      }
      
      return(NULL)
    },
    
    #' Title: Retrieves study data of given type
    #' 
    #' @description 
    #' Retrieves study data depending on the type of data requested. Possibilities
    #' are field definitions, option groups or records.
    #' 
    #' @param study_id Study ID
    #' @param data_type Type of study data requested. Must be "structure", "optiongroups"
    #' or "data", where data are the records
    #' @param tmp_dir Optional temporary directory where to save the retrieved data (in
    #' CSV format)
    #' 
    #' @export
    get_study_data_as_csv = function(study_id, data_type, tmp_dir = NULL) {
      stopifnot(data_type %in% c('data', 'optiongroups', 'structure'))
      
      url <- paste0(self$api_base_url, "/study/", study_id, "/export/", data_type)
      
      count <- 0; 
      status_code <- 0; 
      response <- NULL
      
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
      
      csv_data <- content(response, as = "text", encoding = "UTF-8")
      
      if(!is.null(tmp_dir)) {
        study_name <- self$get_study_name_by_id(study_id)
        dir.create(file.path(tmp_dir, study_name), recursive = TRUE, showWarnings = FALSE)
        file_path <- file.path(tmp_dir, study_name, paste0(data_type, ".csv"))
        writeLines(content, file_path, useBytes = TRUE)
        message(sprintf("Data written to %s", file_path))
      }
      
      return(csv_data)
    },
    
    #' Title: Loads string-encoded CSV data as a data frame
    #' 
    #' @description 
    #' Loads string-encoded CSV data (retrieved with get_study_data_as_csv()) and returns
    #' the data as a data frame
    #' 
    #' @param csv_data String-encoded CSV data
    #' 
    #' @export
    load_csv_data = function(csv_data) {
      df <- read_delim(csv_data, delim = ";", col_names = TRUE, show_col_types = FALSE, name_repair = "minimal")
      return(df)
    },
    
    #' Title: Gets field type for a given field variable name
    #' 
    #' @description 
    #' Gets field type for given field variable name
    #' 
    #' @param field_name Field variable name
    #' @param field_defs Field definitions (as retrieved by get_study_data_as_csv())
    #' 
    #' @export
    get_field_type = function(field_name, field_defs) {
      return(
        field_defs %>%
          filter(`Field Variable Name` == field_name) %>%
          pull(`Field Type`) %>%
          first()
      )
    },
    
    #' Title: Update column data types
    #' 
    #' @description 
    #' Updates column data types in given data frame using the given field definitions
    #' 
    #' @param df Data frame to be updated
    #' @param field_defs Field definitions (as retrieved by get_study_data_as_csv())
    #' 
    #' @export
    set_dataframe_data_types = function(df, field_defs) {
      for(field_name in names(df)) {
        field_type <- self$get_field_type(field_name, field_defs)
        
        if(!is.na(field_type)) {
          if (field_type == "number") {
            df[[field_name]] <- suppressWarnings(as.numeric(df[[field_name]]))
          } else if (field_type %in% c("radio", "dropdown")) {
            df[[field_name]] <- suppressWarnings(as.integer(df[[field_name]]))
          } else if (field_type %in% c("date", "datetime")) {
            df[[field_name]] <- suppressWarnings(as.Date(df[[field_name]], format = "%d-%m-%Y"))
          } else if (field_type == "year") {
            df[[field_name]] <- suppressWarnings(as.Date(paste0(df[[field_name]], "-01-01"), format = "%Y-%m-%d"))
          }
        }
      }
      
      return(df)
    },
    
    #' Title: Get study data as data frame
    #' 
    #' @description 
    #' Gets study data as data frame by retrieving field definitions, option groups and record data
    #' as string-encoded CSV data and merging these together to form a data frame.
    #' 
    #' @param study_name Study name
    #' @param tmp_dir Optional temporary directory where to save the retrieved data (in
    #' CSV format)
    #' 
    #' @examples 
    #' # Example usage:
    #' client <- CastorApiClient$new("1234", "ABCD")
    #' client$get_study_data_as_dataframe("My_Study")
    #' 
    #' @export
    get_study_data_as_dataframe = function(study_name, tmp_dir = NULL) {
      study_id <- self$get_study_id_by_name(study_name)
      field_defs <- self$load_csv_data(self$get_study_data_as_csv(study_id, "structure", tmp_dir))
      optiongroups <- self$load_csv_data(self$get_study_data_as_csv(study_id, "optiongroups", tmp_dir))
      data <- self$load_csv_data(self$get_study_data_as_csv(study_id, "data", tmp_dir))

      data <- data %>%
        left_join(
          field_defs, by = "Field ID") %>%
        select(
          `Record ID`, `Field Variable Name`, `Value`)

      df <- data %>%
        pivot_wider(
          id_cols = `Record ID`, names_from = `Field Variable Name`, values_from = `Value`)

      multi_value_columns <- field_defs$`Field Variable Name`[field_defs$`Field Type` == "checkbox"]
      
      for(column in multi_value_columns) {
        optiongroup_id <- field_defs$`Field Option Group`[field_defs$`Field Variable Name` == column]
        option_values <- optiongroups$`Option Value`[optiongroups$`Option Group Id` == optiongroup_id]
        option_names <- optiongroups$`Option Name`[optiongroups$`Option Group Id` == optiongroup_id]
        
        for(i in 1:length(option_values)) {
          df[[paste0(column, "_", option_names[i])]] <- sapply(df[[column]], function(x) {
            option_values[i] %in% unlist(strsplit(x, ";"))
          }) * 1
        }
      }
      
      df <- df %>% select(-all_of(multi_value_columns))
      df <- df %>% select(-"NA")
      df <- df %>% clean_names()
      
      df <- self$set_dataframe_data_types(df, field_defs)
      
      if(!is.null(tmp_dir)) {
        write.csv2(df, file = sprintf("%s/%s/df.csv", tmp_dir, study_name), row.names = FALSE)
      }

      return(df)
    }
  )
)

# source("credentials.R")
# credentials <- CastorApiCredentials$new()
# client <- CastorApiClient$new(credentials$load_client_id(), credentials$load_client_secret())
# study_name <- "ESPRESSO_v3.0"
# study_id <- client$get_study_id_by_name(study_name)
# field_defs <- client$load_csv_data(client$get_study_data_as_csv(study_id, "structure"))
# study_data <- client$get_study_data_as_dataframe(study_name)