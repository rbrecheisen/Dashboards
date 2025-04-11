library(tidyverse)


################################################################################
#' Title: Number of liver procedures
#' 
#' @description 
#' Shows number of liver procedures per month
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_liver_nr_procedures_chart <- function(study_data) {
  df <- study_data$records
  
  df <- df %>%
    filter(
      lever_pancreas == 0,
      operatie_lever_operatie_niet_doorgegaan != 1,
      resectie != 6
    ) %>%
    mutate(date_operatie = ymd(date_operatie)) %>%
    mutate(month = floor_date(date_operatie, "month")) %>%
    group_by(month) %>%
    summarise(num_procedures = n())
  print(df)
  
  ggplot(df, aes(x = month, y = num_procedures)) +
    geom_bar(stat = "identity") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
      title = "LIVER: Number of procedures per month",
      x = "Month",
      y = "Number of procedures"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, 50, by = 1))
}


################################################################################
#' Title: Number of open/closed liver procedures
#' 
#' @description 
#' Shows number of open/closed liver procedures
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_liver_nr_procedures_open_closed_chart <- function(study_data) {
  df <- study_data$records
  
  df <- df %>%
    filter(
      lever_pancreas == 0,
      operatie_lever_operatie_niet_doorgegaan != 1,
      resectie == 6
    ) %>%
    mutate(date_operatie = ymd(date_operatie)) %>%
    mutate(month = floor_date(date_operatie, "month")) %>%
    group_by(month) %>%
    summarise(num_procedures = n())
  print(df)
  
  ggplot(df, aes(x = month, y = num_procedures)) +
    geom_bar(stat = "identity") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
      title = "LIVER: Number of open/closed procedures per month",
      x = "Month",
      y = "Number of procedures"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, 50, by = 1))
  
}


################################################################################
#' Title: Number of complications after liver surgery
#' 
#' @description 
#' Shows number of complications after liver surgery
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_liver_nr_complications_chart <- function(study_data) {
  df <- study_data$records
  
  df <- df %>%
    mutate(orgaanfalen = factor(
      replace_na(orgaanfalen, -1),
      levels = c(-1, 0, 1, 2),
      labels = c("Missing", "No", "Single-organ", "Multi-organ")
    )) %>%
    filter(
      lever_pancreas == 0,
      operatie_lever_operatie_niet_doorgegaan != 1,
      resectie != 6,
      complicatie_ok == 1,
      complicaties_waarvoor_reinterventie == 1,
      orgaanfalen %in% c("Single-organ", "Multi-organ")
    ) %>%
    mutate(
      date_operatie = ymd(date_operatie),
      month = floor_date(date_operatie, "month")
    ) %>%
    group_by(month, orgaanfalen) %>%
    summarize(num_complications = n(), .groups = "drop")
  print(df)

  ggplot(df, aes(x = month, y = num_complications, fill = orgaanfalen)) +
    geom_bar(stat = "identity") +
    labs(
      title = "LIVER: Number of complications",
      x     = "Month",
      y     = "Number of complications",
      fill  = "Organ failure"
    ) +
    theme_minimal() +
    scale_fill_discrete(labels = c(
      "-1" = "Missing",
      "0"  = "No",
      "1"  = "Single-organ",
      "2"  = "Multi-organ"
    )) +
    scale_y_continuous(breaks = seq(0, 50, by = 1))
}


################################################################################
#' Title: Average number of days from MDT to liver surgery
#' 
#' @description 
#' Shows average number of days from MDT to liver surgery
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_liver_nr_days_mdt_to_surgery_chart <- function(study_data) {
  df <- study_data$records
  
  df = df %>%
    filter(
      lever_pancreas == 0,
      operatie_lever_operatie_niet_doorgegaan != 1,
      resectie != 6
    ) %>%
    mutate(
      date_mdo = ymd(date_mdo),
      date_operatie = ymd(date_operatie),
      mdo_to_surgery = as.numeric(date_operatie - date_mdo),
      month = floor_date(date_operatie, "month")
    ) %>%
    group_by(month) %>%
    summarize(average_times = mean(mdo_to_surgery, na.rm = TRUE), .groups = "drop")
  print(df)

  ggplot(df, aes(x = month, y = average_times)) +
    geom_bar(stat = "identity") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
      title = "LIVER: Average number of days MDT to surgery",
      x = "Month",
      y = "Average number of days"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, 200, by = 10))
}


################################################################################
#' Title: Average number of days from liver surgery to discharge
#' 
#' @description 
#' Shows average number of days from liver surgery to discharge
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_liver_nr_days_surgery_to_discharge_chart <- function(study_data) {
  df <- study_data$records
  
  df = df %>%
    filter(
      lever_pancreas == 0,
      operatie_lever_operatie_niet_doorgegaan != 1,
      resectie != 6
    ) %>%
    mutate(
      date_operatie = ymd(date_operatie),
      datum_ontslag = ymd(datum_ontslag),
      surgery_to_discharge = as.numeric(datum_ontslag - date_operatie),
      month = floor_date(datum_ontslag, "month")
    ) %>%
    group_by(month) %>%
    summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
  print(df)
  
  ggplot(df, aes(x = month, y = average_times)) +
    geom_bar(stat = "identity") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
      title = "LIVER: Average number of days surgery to discharge",
      x = "Month",
      y = "Average number of days"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, 200, by = 10))
}


################################################################################
#' Title: Number of pancreas procedures
#' 
#' @description 
#' Shows number of pancreas procedures
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_pancreas_nr_procedures_chart <- function(study_data) {
  df <- study_data$records

  df <- df %>%
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
      operatie_pancreas_techniek != 7
    ) %>%
    mutate(date_operatie = ymd(date_operatie)) %>%
    mutate(month = floor_date(date_operatie, "month")) %>%
    group_by(month, operatie_pancreas_techniek) %>%
    summarize(num_procedures = n(), .groups = "drop")
  print(df)

  ggplot(df, aes(x = month, y = num_procedures, fill = operatie_pancreas_techniek)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "PANCREAS: Number of procedures",
      x = "Month",
      y = "Number of procedures",
      fill = "Surgical technique"
    ) +
    theme_minimal() +
    scale_fill_discrete(labels = c(
      "-1" = "Missing",
      "0" = "Open procedure",
      "1" = "Fully laparoscopic",
      "2" = "Laparoscopic with conversion",
      "3" = "Fully robotic",
      "4" = "Robotic with conversion to laparoscopic",
      "5" = "Robot with conversion to open",
      "6" = "Laparoscopic resection with robotic reconstruction",
      "7" = "Exploration without resection",
      "8" = "Other",
      "9" = "Not applicable"
    )) +
    scale_y_continuous(breaks = seq(0, 50, by = 1))
}


################################################################################
#' Title: Number of open/closed pancreas procedures
#' 
#' @description 
#' Shows number of open/closed pancreas procedures
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_pancreas_nr_open_closed_procedures_chart <- function(study_data) {
  df <- study_data$records
  
  df <- df %>%
    filter(
      lever_pancreas == 1,
      operatie_pancreas != 11,
      operatie_pancreas_techniek == 7
    ) %>%
    mutate(date_operatie = ymd(date_operatie)) %>%
    mutate(month = floor_date(date_operatie, "month")) %>%
    group_by(month) %>%
    summarize(num_procedures = n(), .groups = "drop")
  print(df)
  
  ggplot(df, aes(x = month, y = num_procedures)) +
    geom_bar(stat = "identity") +
    labs(
      title = "PANCREAS: Number of open/closed procedures",
      x = "Month",
      y = "Number of procedures"
    ) +
    theme_minimal() +
    scale_y_continuous(breaks = seq(0, 50, by = 1))
}


################################################################################
#' Title: Number of open/closed pancreas procedures
#' 
#' @description 
#' Shows number of open/closed pancreas procedures
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_pancreas_nr_complications_chart <- function(study_data) {
  df <- study_data$records

  df <- df %>%
    mutate(orgaanfalen = factor(
      replace_na(orgaanfalen, -1),
      levels = c(-1, 0, 1, 2),
      labels = c("Missing", "No", "Single-organ", "Multi-organ")
    )) %>%
    filter(
      lever_pancreas == 1,
      operatie_pancreas != 11,
      operatie_pancreas_techniek < 7,
      complicatie_ok == 1,
      complicaties_waarvoor_reinterventie == 1,
      orgaanfalen %in% c("Single-organ", "Multi-organ")
    ) %>%
    mutate(
      date_operatie = ymd(date_operatie),
      month = floor_date(date_operatie, "month")
    ) %>%
    group_by(month, orgaanfalen) %>%
    summarize(num_complications = n(), .groups = "drop")
  print(df)
  
  ggplot(df, aes(x = month, y = num_complications, fill = orgaanfalen)) +
    geom_bar(stat = "identity") +
    labs(
      title = "PANCREAS: Number of complications",
      x     = "Month",
      y     = "Number of complications",
      fill  = "Organ failure"
    ) +
    theme_minimal() +
    scale_fill_discrete(labels = c(
      "-1" = "Missing",
      "0"  = "No",
      "1"  = "Single-organ",
      "2"  = "Multi-organ"
    )) +
    scale_y_continuous(breaks = seq(0, 50, by = 1))
}


################################################################################
#' Title: Average number of days between MDT and pancreas surgery
#' 
#' @description 
#' Shows average number of days between MDT and pancreas surgery
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_pancreas_nr_days_mdt_to_surgery_chart <- function(study_data) {
  df <- study_data$records
  
  df <- df %>%
    filter(
      lever_pancreas == 1,
      operatie_pancreas != 11,
      operatie_pancreas_techniek != 7
    ) %>%
    mutate(
      date_mdo = ymd(date_mdo),
      date_operatie = ymd(date_operatie),
      mdo_to_surgery = as.numeric(date_operatie - date_mdo),
      month = floor_date(date_operatie, "month")
    ) %>%
    group_by(month) %>%
    summarize(average_times = mean(mdo_to_surgery, na.rm = TRUE), .groups = "drop")
  print(df)
  
  ggplot(df, aes(x = month, y = average_times)) +
    geom_bar(stat = "identity") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
      title = "PANCREAS: Average number of days MDT to surgery",
      x = "Month",
      y = "Average number of days"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, 250, by = 10))
}


################################################################################
#' Title: Average number of days between pancreas surgery and discharge
#' 
#' @description 
#' Shows average number of days between pancreas surgery and discharge
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_pancreas_nr_days_surgery_to_discharge_chart <- function(study_data) {
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
    filter(
      lever_pancreas == 1,
      operatie_pancreas != 11,
      operatie_pancreas_techniek == 0 # Open
    ) %>%
    mutate(
      date_operatie = ymd(date_operatie),
      datum_ontslag = ymd(datum_ontslag),
      surgery_to_discharge = as.numeric(datum_ontslag - date_operatie),
      month = floor_date(datum_ontslag, "month")
    ) %>%
    group_by(month, operatie_pancreas) %>%
    summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
  print(df)
  
  ggplot(df, aes(x = month, y = average_times, fill = operatie_pancreas)) +
    geom_bar(stat = "identity") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
      title = "PANCREAS: Average number of days OPEN surgery to discharge",
      x = "Month",
      y = "Average number of days",
      fill = "Type of surgery"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(labels = c(
      "-1" = "Missing",
      "0" = "PPPD",
      "1" = "Classical Whipple",
      "2" = "PRPD",
      "3" = "Pancreas body/tail",
      "4" = "Appleby",
      "5" = "Total pancreatectomy",
      "6" = "Enucleation",
      "7" = "Biliary resection",
      "8" = "Frey",
      "9" = "Pancreaticojejunostomy",
      "10" = "Other",
      "11" = "Operation cancelled"
    )) +
    scale_y_continuous(breaks = seq(0, 250, by = 10))
}