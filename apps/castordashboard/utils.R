# ===============================================================================
# Loads packages from a list of package names, if they have not bee loaded yet
# - packages: List of package names
# return: NULL
utils_load_packages <- function(packages) {
  for(package in packages) {
    if(!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
  }
}


# ===============================================================================
# Loads value from a text file
# - filepath: File path of the text file
# return: value
utils_load_value_from_file <- function(filepath) {
  value <- readLines(filepath, n = 1)
  return(value)
}


# ===============================================================================
# Loads API client ID from file
# - client_id_filepath: File path of text file containing client ID
# return: client ID
utils_load_api_client_id <- function(client_id_filepath) {
  value <- utils_load_value_from_file(client_id_filepath)
  return(value)
}


# ===============================================================================
# Loads API client secret from file
# - client_secret_filepath: File path of text file containing client secret
# return: client secret
utils_load_api_client_secret <- function(client_secret_filepath) {
  value <- utils_load_value_from_file(client_secret_filepath)
  return(value)
}


# ===============================================================================
# Builds URL from a base URL and an endpoint
# - base_url: The base URL to use
# - endpoint: The URL endpoint
# return: constructed URL
utils_build_url <- function(base_url, endpoint) {
  return(paste0(base_url, endpoint))
}