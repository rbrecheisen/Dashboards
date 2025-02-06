library(ggplot2)
library(dplyr)
library(lubridate)


source("../../apps/hpbdashboard/credentials.R")
source("../../apps/hpbdashboard/castorapiclient.R")


study_name <- "ESPRESSO_v3.0"
credentials <- CastorApiCredentials$new()
client <- CastorApiClient$new(credentials$load_client_id(), credentials$load_client_secret())
study_data <- client$get_study_data(study_name)
df <- study_data$records

target_year <- 2023

df_filtered <- df %>%
  mutate(
    date_operatie = ymd(date_operatie),
    datum_ontslag = ymd(datum_ontslag),
    surgery_to_discharge = as.numeric(datum_ontslag - date_operatie)
  ) %>%
  mutate(operatie_pancreas = factor(
    replace_na(operatie_pancreas, -1),
    levels = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    labels = c(
      "Missing", 
      "PPPD",
      "Classical Whipple",
      "PRPD",
      "Pancreas body/tail",
      "Appleby",
      "Total pancreatectomy",
      "Enucleation",
      "Biliary resection",
      "Frey",
      "Pancreaticojejunostomy",
      "Other",
      "Operation cancelled"
    )
  )) %>%
  mutate(operatie_pancreas_techniek = factor(
    replace_na(operatie_pancreas_techniek, -1),
    levels = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    labels = c(
      "Missing", 
      "Open procedure",
      "Fully laparoscopic",
      "Laparoscopic with conversion",
      "Fully robotic",
      "Robotic with conversion to laparoscopic",
      "Robotic with conversion to open",
      "Laparoscopic resection with robotic reconstruction",
      "Exploration without resection",
      "Other",
      "Not applicable"
    )
  )) %>%
  filter(year(date_operatie) == target_year) %>%
  group_by(operatie_pancreas, operatie_pancreas_techniek) %>%
  summarise(avg_days = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")

df_filtered

ggplot(df_filtered, aes(x = operatie_pancreas, y = avg_days, fill = operatie_pancreas_techniek)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Aantal dagen ligduur per operatie techniek en uitvoering in ", target_year),
       x = "Operatie techniek",
       y = "Aantal dagen",
       fill = "Uitvoering") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 50, by = 1))
