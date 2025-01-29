library(R6)
library(dplyr)
library(ggplot2)

source("charts/chart.R")


PancreasProceduresChart = R6Class(
  
  "PancreasProceduresChart",
  
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
        mutate(date_operatie = ymd(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month, operatie_pancreas_techniek) %>%
        summarize(num_procedures = n(), .groups = "drop")
    },
    
    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures, fill = operatie_pancreas_techniek)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Number of pancreas procedures",
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
#     date_operatie = ymd(date_operatie),
#     month = floor_date(date_operatie, "month")
#   ) %>%
#   group_by(month, operatie_pancreas_techniek) %>%
#   summarize(num_procedures = n(), .groups = "drop")
# study_data$operatie_pancreas_techniek
