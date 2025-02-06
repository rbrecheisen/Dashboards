library(tidyverse)


source("../../apps/hpbdashboard/credentials.R")
source("../../apps/hpbdashboard/castorapiclient.R")


study_name <- "ESPRESSO_v3.0"
credentials <- CastorApiCredentials$new()
client <- CastorApiClient$new(credentials$load_client_id(), credentials$load_client_secret())
study_data <- client$get_study_data(study_name)
df <- study_data$records


df <- df %>% 
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
  filter(
    lever_pancreas == 1,
    operatie_pancreas != 11,
    operatie_pancreas_techniek == "Open procedure" | operatie_pancreas_techniek == "Laparoscopic with conversion" | operatie_pancreas_techniek == "Robotic with conversion to open"
  ) %>%
  mutate(
    date_operatie = ymd(date_operatie),
    datum_ontslag = ymd(datum_ontslag),
    surgery_to_discharge = as.numeric(datum_ontslag - date_operatie),
    month = floor_date(datum_ontslag, "month")
  )

average_times <- df %>%
  group_by(month, operatie_pancreas_techniek) %>%
  summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
average_times

ggplot(average_times, aes(x = month, y = average_times)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 250, by = 10))