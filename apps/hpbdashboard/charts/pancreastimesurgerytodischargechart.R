library(R6)
library(dplyr)
library(ggplot2)

source("charts/chart.R")


PancreasTimeSurgeryToDischargeChart = R6Class(
  
  "PancreasTimeSurgeryToDischargeChart",
  
  inherit = Chart,
  
  public = list(
    
    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
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
        mutate(
          date_operatie = ymd(date_operatie),
          datum_ontslag = ymd(datum_ontslag),
          surgery_to_discharge = as.numeric(datum_ontslag - date_operatie),
          month = floor_date(datum_ontslag, "month")
        )
    },
    
    show = function() {
      
      average_times <- self$df %>%
        group_by(month, operatie_pancreas_techniek) %>%
        summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
      
      ggplot(average_times, aes(x = month, y = average_times, fill = operatie_pancreas_techniek)) +
        geom_bar(stat = "identity") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(
          title = "PANCREAS: Average number of days surgery to discharge",
          x = "Month",
          y = "Average number of days",
          fill = "Surgical technique"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
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
        scale_y_continuous(breaks = seq(0, 250, by = 10))
    }
  )
)

# load("study_data.Rdata")
# study_data <- study_data %>%
#   filter(
#     lever_pancreas == 1,
#     operatie_pancreas != 11,
#     operatie_pancreas_techniek != 7
#   ) %>%
#   mutate(
#     date_mdo = ymd(date_mdo),
#     date_operatie = ymd(date_operatie),
#     mdo_to_surgery = as.numeric(date_operatie - date_mdo),
#     month = floor_date(date_operatie, "month")
#   )
# study_data$mdo_to_surgery
