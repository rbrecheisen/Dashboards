library(R6)
library(dplyr)
library(lubridate)
library(ggplot2)

source("charts/chart.R")


LiverProceduresOpenClosedChart = R6Class(
  
  "LiverProceduresOpenClosedChart",
  
  inherit = Chart,
  
  public = list(
    
    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 0,
          operatie_lever_operatie_niet_doorgegaan != 1,
          resectie == 6
        ) %>%
        mutate(date_operatie = ymd(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month) %>%
        summarise(num_procedures = n())
    },
    
    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures)) +
        geom_bar(stat = "identity") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(
          title = "LIVER: Number of open/closed procedures",
          x = "Month",
          y = "Number of procedures"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)

# load("study_data.Rdata")
# study_data <- study_data %>%
#   filter(
#     lever_pancreas == 0,
#     operatie_lever_operatie_niet_doorgegaan != 1,
#     resectie == 6
#   )
# nrow(study_data)
# do <- study_data$date_operatie
# do <- ymd(do)