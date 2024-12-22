library(rprojroot)
library(yaml)
library(dplyr)
library(tidyverse)


# APP INITIALIZATION

app_dir = rprojroot::find_root(rprojroot::is_rstudio_project)
tmp_dir = file.path(tempdir(), "castordashboard")
setwd(app_dir)
config = yaml.load_file("config.yaml")
source("utils.R")
utils_load_packages(config$packages)
source("castorapi.R")


# CASTOR API CLIENT INITIALIZATION

api_client_id = utils_load_api_client_id(config$api_client_id_filepath)
api_client_secret = utils_load_api_client_secret(config$api_client_secret_filepath)
client = CastorAPI$new(client_id = api_client_id, client_secret = api_client_secret, api_base_url = config$api_base_url, token_url = config$api_token_url)


# BUILDING DATA FRAME FROM CSV DATA

study_name = "ESPRESSO_v3.0"
study_id = client$get_study_id_by_name(study_name)
study_data = client$load_csv_data(client$get_study_data_as_csv(study_id, "data", tmp_dir = tmp_dir))
study_structure = client$load_csv_data(client$get_study_data_as_csv(study_id, "structure", tmp_dir = tmp_dir))
study_optiongroups = client$load_csv_data(client$get_study_data_as_csv(study_id, "optiongroups", tmp_dir = tmp_dir))

data = read.csv(
  sprintf("%s/%s/data.csv", tmp_dir, study_name), 
  header = TRUE, sep = ";")

field_defs = read.csv(
  sprintf("%s/%s/structure.csv", tmp_dir, study_name), 
  header = TRUE, sep = ";")

optiongroups = read.csv(
  sprintf("%s/%s/optiongroups.csv", tmp_dir, study_name), 
  header = TRUE, sep = ";")

data = data %>%
  left_join(
    field_defs, by = "Field.ID") %>%
  select(
    Record.ID, Field.Variable.Name, Value)

df = data %>%
  pivot_wider(
    id_cols = Record.ID, names_from = Field.Variable.Name, values_from = Value)


# ONE-HOT ENCODING MULTI-VALUE COLUMNS IN DATA FRAME

multi_value_columns = field_defs$Field.Variable.Name[field_defs$Field.Type == "checkbox"]
for(column in multi_value_columns) {
  optiongroup_id = field_defs$Field.Option.Group[field_defs$Field.Variable.Name == column]
  option_values = optiongroups$Option.Value[optiongroups$Option.Group.Id == optiongroup_id]
  option_names = optiongroups$Option.Name[optiongroups$Option.Group.Id == optiongroup_id]
  for(i in 1:length(option_values)) {
    df[[paste0(column, "#", option_names[i])]] <- sapply(df[[column]], function(x) {
      option_values[i] %in% unlist(strsplit(x, ";"))
    }) * 1
  }
}
df <- df %>% select(-all_of(multi_value_columns))


# WRITE DATA FRAME TO OUTPUT

write.csv2(df, file = sprintf("%s/%s/df.csv", tmp_dir, study_name), row.names = FALSE)