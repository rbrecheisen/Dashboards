# install.packages("writexl")

library(tidyverse)
library(writexl)

source("../../apps/hpbdashboard/credentials.R")
source("../../apps/hpbdashboard/castorapiclient.R")

study_name <- "ESPRESSO_v3.0"
credentials <- CastorApiCredentials$new()
client <- CastorApiClient$new(credentials$load_client_id(), credentials$load_client_secret())
study_data <- client$get_study_data(study_name)
df <- study_data$records


df <- df %>%
  mutate(operatie_pancreas = if_else(is.na(operatie_pancreas), -1, operatie_pancreas)) %>%
  mutate(operatie_pancreas_techniek = if_else(is.na(operatie_pancreas_techniek), -1, operatie_pancreas_techniek)) %>%
  mutate(
    date_operatie = ymd(date_operatie),
    datum_ontslag = ymd(datum_ontslag),
    surgery_to_discharge = as.numeric(datum_ontslag - date_operatie),
    year = floor_date(datum_ontslag, "year")
  )

#### OPEN
# PPPD, classical Whipple, PRPD
average_times_open_pppd <- df %>% 
  filter(
    lever_pancreas == 1, # pancreas
    operatie_pancreas == 0 | operatie_pancreas == 1 | operatie_pancreas == 2,
    operatie_pancreas_techniek == 0 | operatie_pancreas_techniek == 2 | operatie_pancreas_techniek == 5
  ) %>%
  group_by(year, operatie_pancreas, operatie_pancreas_techniek) %>%
  summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
write_xlsx(average_times_open_pppd, "average_times_open_pppd.xlsx")

# Pancreas body/tail
average_times_open_tail <- df %>% 
  filter(
    lever_pancreas == 1, # pancreas
    operatie_pancreas == 3,
    operatie_pancreas_techniek == 0 | operatie_pancreas_techniek == 2 | operatie_pancreas_techniek == 5
  ) %>%
  group_by(year, operatie_pancreas, operatie_pancreas_techniek) %>%
  summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
write_xlsx(average_times_open_tail, "average_times_open_tail.xlsx")


#### LAPAROSCOPIC
# PPPD, classical Whipple, PRPD
average_times_lap_pppd <- df %>% 
  filter(
    lever_pancreas == 1, # pancreas
    operatie_pancreas == 0 | operatie_pancreas == 1 | operatie_pancreas == 2,
    operatie_pancreas_techniek == 1
  ) %>%
  group_by(year, operatie_pancreas, operatie_pancreas_techniek) %>%
  summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
write_xlsx(average_times_lap_pppd, "average_times_lap_pppd.xlsx")

# Pancreas body/tail
average_times_lap_tail <- df %>% 
  filter(
    lever_pancreas == 1, # pancreas
    operatie_pancreas == 3,
    operatie_pancreas_techniek == 1
  ) %>%
  group_by(year, operatie_pancreas, operatie_pancreas_techniek) %>%
  summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
write_xlsx(average_times_lap_tail, "average_times_lap_tail.xlsx")


#### ROBOT
# PPPD, classical Whipple, PRPD
average_times_robot_pppd <- df %>% 
  filter(
    lever_pancreas == 1, # pancreas
    operatie_pancreas == 0 | operatie_pancreas == 1 | operatie_pancreas == 2,
    operatie_pancreas_techniek == 3
  ) %>%
  group_by(year, operatie_pancreas, operatie_pancreas_techniek) %>%
  summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
write_xlsx(average_times_robot_pppd, "average_times_robot_pppd.xlsx")

# Pancreas body/tail
average_times_robot_tail <- df %>% 
  filter(
    lever_pancreas == 1, # pancreas
    operatie_pancreas == 3,
    operatie_pancreas_techniek == 3
  ) %>%
  group_by(year, operatie_pancreas, operatie_pancreas_techniek) %>%
  summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
write_xlsx(average_times_robot_tail, "average_times_robot_tail.xlsx")