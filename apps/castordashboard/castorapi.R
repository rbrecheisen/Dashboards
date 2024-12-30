CastorAPI = R6Class(
  
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
      self$token = self$connect(client_id, client_secret, token_url)
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
    
    # ==========================================================================
    # Retrieves study name for given study ID
    # - study_id: Study ID
    # return: study name
    get_study_name_by_id = function(study_id) {
      studies = self$get_studies()
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
      studies = self$get_studies()
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
    
    # ==========================================================================
    # Loads CSV data as a list of dictionaries (each row one dictionary)
    # - csv_data: CSV data as text
    # return: List of dictionaries
    load_csv_data = function(csv_data) {
      dict_data = read_delim(csv_data, delim = ";", col_names = TRUE, show_col_types = FALSE, name_repair = "minimal")
      return(dict_data)
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
    # - study_name: Name of the study to retrieve
    # return: Dataframe with study data
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