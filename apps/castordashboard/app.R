library(rprojroot)
library(yaml)

app_dir = rprojroot::find_root(rprojroot::is_rstudio_project)
tmp_dir = file.path(tempdir(), "castordashboard")
setwd(app_dir)

config = yaml.load_file("config.yaml")

source("utils.R")
utils_load_packages(config$packages)

source("castorapi.R")

api_client_id = utils_load_api_client_id(config$api_client_id_filepath)
api_client_secret = utils_load_api_client_secret(config$api_client_secret_filepath)

client = CastorAPI$new(client_id = api_client_id, client_secret = api_client_secret, api_base_url = config$api_base_url, token_url = config$api_token_url)

# study_name = "ESPRESSO_v2.0_DHBA"
study_name = "ESPRESSO_v3.0"

study_id = client$get_study_id_by_name(study_name)

study_data = client$load_csv_data(client$get_study_data_as_csv(study_id, "data", tmp_dir = tmp_dir))
study_structure = client$load_csv_data(client$get_study_data_as_csv(study_id, "structure", tmp_dir = tmp_dir))
study_optiongroups = client$load_csv_data(client$get_study_data_as_csv(study_id, "optiongroups", tmp_dir = tmp_dir))

library(dplyr)
library(tidyverse)

data = read.csv(
  sprintf("%s/%s/data.csv", tmp_dir, study_name), 
  header = TRUE, sep = ";")

field_defs = read.csv(
  sprintf("%s/%s/structure.csv", tmp_dir, study_name), 
  header = TRUE, sep = ";")

data = data %>%
  left_join(
    field_defs, by = "Field.ID") %>%
  select(
    Record.ID, Field.Variable.Name, Value)

df = data %>%
  pivot_wider(
    id_cols = Record.ID, names_from = Field.Variable.Name, values_from = Value)

write.csv(df, file = sprintf("%s/%s/df.csv", tmp_dir, study_name), row.names = FALSE)
