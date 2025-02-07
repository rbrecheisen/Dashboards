library(tidyverse)


################################################################################
#' Title: Number of procedures per month
#' 
#' @description 
#' Shows number of procedures per month for liver surgery
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
#' Title: Number of open/closed procedures per month
#' 
#' @description 
#' Shows number of open/closed procedures per month for liver surgery
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_liver_nr_procedures_open_closed_chart <- function(study_data) {
  
}


################################################################################
#' Title: Number of complications per month
#' 
#' @description 
#' Shows number of complications per month after liver surgery
#' 
#' @param study_data Dataframe with study data (records, fields, options)
#' 
#' @export
show_liver_nr_complications_chart <- function(study_data) {
  
}