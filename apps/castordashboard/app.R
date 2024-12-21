library(rprojroot)
library(yaml)

# Define directories
app_dir <- rprojroot::find_root(rprojroot::is_rstudio_project)
modules_dir <- file.path(app_dir, "modules")
setwd(app_dir)

# Load configuration
config <- yaml.load_file("config.yaml")

# Load required packages
source("utils.R")
utils_load_packages(config$packages)

# Import external R scripts
source(file.path(modules_dir, "api.R"))

# Get Castor client ID and secret
api_client_id <- utils_load_api_client_id(config$api_client_id_filepath)
api_client_secret <- utils_load_api_client_secret(config$api_client_secret_filepath)

# Initialize Castor API client
client <- CastorAPI$new(
  client_id = api_client_id, 
  client_secret = api_client_secret, 
  api_base_url = config$api_base_url, 
  token_url = config$api_token_url
)

# Retrieve study IDs and names
studies <- client$get_studies()
studies

study_name <- client$get_study_name_by_id("0FA8DA88-B6E8-4D9B-AA1B-E8353869C980")
study_name

study_id <- client$get_study_id_by_name("ESPRESSO_v2.0_DHBA")
study_id

study_data <- client$get_study_data_as_csv("0FA8DA88-B6E8-4D9B-AA1B-E8353869C980", "data")
study_data