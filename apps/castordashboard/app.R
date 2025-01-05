# Sys.setenv(CURL_CA_BUNDLE = "/opt/homebrew/etc/ca-certificates/cert.pem")
# Sys.setenv(LD_LIBRARY_PATH = "/opt/homebrew/lib")
# 
# install.packages("curl", type = "source",
#                  configure.args = "--with-curl-config=/opt/homebrew/opt/curl/bin/curl-config")

if(!requireNamespace("rprojroot", quietly = TRUE)) {
  install.packages("rprojroot")
}

library(rprojroot)

app_dir = rprojroot::find_root(rprojroot::is_rstudio_project)
setwd(app_dir)
tmp_dir = file.path(tempdir(), "castordashboard")

required_packages = c(
  "curl",
  "yaml",
  "httr",
  "R6",
  "jsonlite",
  "tidyverse",
  "janitor",
  "lubridate",
  "shiny",
  "ggplot2"
)

missing_packages = required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

config = yaml.load_file("config.yaml")

source("utils.R")
source("castorapi.R")

client = CastorAPI$new(
  client_id = utils_load_api_client_id(config$api_client_id_filepath), 
  client_secret = utils_load_api_client_secret(config$api_client_secret_filepath), 
  api_base_url = config$api_base_url, 
  token_url = config$api_token_url
)

df = client$get_study_data_as_dataframe("ESPRESSO_v3.0")

source("charts/liverprocedures.R")
chart = LiverProceduresPerMonthChart$new(df)
chart$show()

source("charts/liverproceduresopenclosed.R")
chart = LiverProceduresPerMonthOpenClosedChart$new(df)
chart$show()

source("charts/livercomplications.R")
chart = LiverComplicationsPerMonthChart$new(df)
chart$show()

source("charts/pancreasprocedures.R")
chart = PancreasProceduresPerMonthChart$new(df)
chart$show()
