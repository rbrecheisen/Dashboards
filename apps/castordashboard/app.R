# Load required packages
library(rprojroot)
library(yaml)
library(httr)
library(R6)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(janitor)
library(ggplot2)
library(lubridate)

# Initialize app settings
app_dir = rprojroot::find_root(rprojroot::is_rstudio_project)
tmp_dir = file.path(tempdir(), "castordashboard")
setwd(app_dir)
config = yaml.load_file("config.yaml")
source("utils.R")
source("castorapi.R")

# Initialize Castor API client
client = CastorAPI$new(
  client_id = utils_load_api_client_id(config$api_client_id_filepath), 
  client_secret = utils_load_api_client_secret(config$api_client_secret_filepath), 
  api_base_url = config$api_base_url, 
  token_url = config$api_token_url
)

# Retrieve Castor study data as a dataframe
study_name = "ESPRESSO_v3.0"
df = client$get_study_data_as_dataframe(study_name, tmp_dir)

# Create barchart for nr. liver procedures per month
df = df %>%
  filter(
    lever_pancreas == 0,
    operatie_lever_operatie_niet_doorgegaan != 1,
    resectie != 6
  )
df <- df %>%
  mutate(date_operatie = dmy(date_operatie)) %>%
  mutate(month = floor_date(date_operatie, "month")) %>%
  group_by(month) %>%
  summarise(num_procedures = n())

ggplot(df, aes(x = month, y = num_procedures)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Number of Liver Procedures per Month",
    x = "Month",
    y = "Number of Procedures"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
