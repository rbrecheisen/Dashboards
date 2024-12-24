library(rprojroot)
library(yaml)


# APP INITIALIZATION

app_dir = rprojroot::find_root(rprojroot::is_rstudio_project)
tmp_dir = file.path(tempdir(), "castordashboard")
tmp_dir
setwd(app_dir)
config = yaml.load_file("config.yaml")
source("utils.R")
utils_load_packages(config$packages)
source("castorapi.R")


# CASTOR API CLIENT INITIALIZATION

api_client_id = utils_load_api_client_id(config$api_client_id_filepath)
api_client_secret = utils_load_api_client_secret(config$api_client_secret_filepath)

client = CastorAPI$new(client_id = api_client_id, client_secret = api_client_secret, api_base_url = config$api_base_url, token_url = config$api_token_url)


# GET CASTOR DATA AS DATAFRAME

study_name = "ESPRESSO_v3.0"
df = client$get_study_data_as_dataframe(study_name, tmp_dir)


# CREATE BAR CHART FOR NR. OF LIVER PROCEDURES PER MONTH

df = df %>%
  filter(
    lever_pancreas == 0,
    operatie_lever_operatie_niet_doorgegaan != 1,
    resectie != 6
  )
print(df$date_operatie)

library(ggplot2)
library(lubridate)

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
